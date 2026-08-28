;;; gptel-usage-test.el --- Tests for gptel-usage.el  -*- lexical-binding: t; -*-

;; Unit tests for the gptel usage tracker:
;; - pricing arithmetic (`gptel-usage--cost')
;; - record writing / log round-trip (`gptel-usage--record', `gptel-usage--read-log')
;; - the error-containment guarantee (tracking must never break gptel)
;; - advice plumbing of `gptel-usage-mode'
;; - report rendering (`gptel-usage-report')
;;
;; The FSM -> :tokens contract is exercised with a synthetic FSM built
;; via `gptel-make-fsm', the same way gptel's own tests build FSMs (see
;; gptel-request-test.el).  `gptel--handle-post-insert' / `gptel--handle-error'
;; receive the FSM as their sole argument (see gptel-send--handlers in
;; gptel.el), so exercising `gptel-usage--record' directly with such an
;; FSM is equivalent to what the :after advice runs.
;;
;; These tests pin the behavior as committed: :tokens (this turn) is the
;; data source, not :tokens-full (whole request, see `gptel-usage--record').

(require 'ert)
(require 'gptel)
(require 'gptel-usage)
(require 'gptel-test-backends)

(defmacro gptel-usage-test--with-log (&rest body)
  "Evaluate BODY with `gptel-usage-log-file' bound to a fresh temp file."
  (declare (indent 0))
  `(let ((gptel-usage-log-file (make-temp-file "gptel-usage-test-")))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (delete-file gptel-usage-log-file)))))

(defun gptel-usage-test--fsm (&optional tokens)
  "Return a synthetic gptel FSM with `gptel-test-backends' openai backend.
TOKENS, if given, is placed on the info plist under :tokens."
  (gptel-make-fsm
   :info (nconc (and tokens (list :tokens tokens))
                (list :backend (alist-get 'openai gptel-test-backends)
                      :model 'gpt-4o-mini))))


;;;; Pricing arithmetic

(ert-deftest gptel-usage-test-cost ()
  "Cost is USD price-per-M token times token counts."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0 :cached 1.0)))))
    (should (equal (gptel-usage--cost
                    "gpt-4o-mini"
                    '(:input 1000000 :output 500000 :cached 250000))
                   20.25))))

(ert-deftest gptel-usage-test-cost-missing-keys-are-zero ()
  "Absent token kinds need no rate: unused cache does not block costing."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
    ;; No cache rates in pricing, and no cache tokens used.
    (should (equal (gptel-usage--cost "gpt-4o-mini" '(:input 100000 :output 200000)) 5.0))))

(ert-deftest gptel-usage-test-cost-unknown-model-returns-nil ()
  "A model with no pricing entry yields nil (unknown), not 0."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
    (should (null (gptel-usage--cost "some-other-model" '(:input 100 :output 100))))))

(ert-deftest gptel-usage-test-cost-nil-pricing-returns-nil ()
  "A model explicitly mapped to nil pricing counts as unknown."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . nil))))
    (should (null (gptel-usage--cost "gpt-4o-mini" '(:input 100 :output 100))))))


;;;; Cache read/write pricing

(ert-deftest gptel-usage-test-cost-cache-read-and-write ()
  "Cache reads and writes are billed at their own rates."
  (let ((gptel-usage-pricing
         '(("claude" . (:input 3.0 :output 15.0
                        :cache-read 0.3 :cache-write 3.75)))))
    ;; :input includes the 1M cache writes, so the fresh input is 1M.
    ;; 1M*3 + 1M*15 + 1M*0.3 + 1M*3.75 = 22.05
    (should (< (abs (- (gptel-usage--cost
                        "claude"
                        '(:input 2000000 :output 1000000
                          :cached 1000000 :cache 1000000))
                       22.05))
               1e-9))))

(ert-deftest gptel-usage-test-cost-cache-write-not-double-counted ()
  "Cache write tokens are billed once, not as input and write both.

Anthropic and Bedrock fold cache creation tokens into :input (verified
by `gptel-usage-test-backend-folds-cache-write-into-input'), so costing
must subtract :cache from :input.  With a write rate far above the
input rate, double counting is unmistakable in the total."
  (let ((gptel-usage-pricing
         '(("claude" . (:input 1.0 :output 0.0
                        :cache-read 0.0 :cache-write 1000.0)))))
    ;; Anthropic-shaped: 100 fresh + 1000 written (folded) = :input 1100.
    ;; Correct:      100*1 + 1000*1000 = 1000100 -> $1.0001
    ;; Double count: 1100*1 + 1000*1000 = 1001100 -> $1.0011
    (let ((cost (gptel-usage--cost
                 "claude" '(:input 1100 :output 0 :cached 5000 :cache 1000))))
      (should (< (abs (- cost 1.0001)) 1e-9)))))

(ert-deftest gptel-usage-test-cost-cached-alias-for-cache-read ()
  "The legacy :cached pricing key still works as the cache read rate."
  (let ((old '(("m" . (:input 10.0 :output 0.0 :cached 1.0))))
        (new '(("m" . (:input 10.0 :output 0.0 :cache-read 1.0)))))
    (let ((tokens '(:input 1000000 :output 0 :cached 1000000)))
      (should (equal (let ((gptel-usage-pricing old))
                       (gptel-usage--cost "m" tokens))
                     (let ((gptel-usage-pricing new))
                       (gptel-usage--cost "m" tokens)))))))

(ert-deftest gptel-usage-test-cost-cache-read-takes-precedence ()
  "When both keys are present, :cache-read wins over :cached."
  (let ((gptel-usage-pricing
         '(("m" . (:input 0.0 :output 0.0 :cache-read 2.0 :cached 99.0)))))
    (should (< (abs (- (gptel-usage--cost "m" '(:input 0 :output 0 :cached 1000000))
                       2.0))
               1e-9))))

(ert-deftest gptel-usage-test-cost-unknown-when-write-rate-missing ()
  "Nonzero cache writes without a :cache-write rate make cost unknown.

Billing them at zero would silently understate the cost, which is the
same failure mode the nil-pricing convention exists to avoid."
  (let ((gptel-usage-pricing '(("claude" . (:input 3.0 :output 15.0 :cache-read 0.3))))) 
    (should (null (gptel-usage--cost
                   "claude"
                   '(:input 1100 :output 10 :cached 500 :cache 1000))))))

(ert-deftest gptel-usage-test-cost-known-when-write-rate-missing-but-unused ()
  "A missing :cache-write rate is fine when no cache writes occurred."
  (let ((gptel-usage-pricing '(("claude" . (:input 3.0 :output 15.0 :cache-read 0.3)))))
    (should (gptel-usage--cost
             "claude" '(:input 1000 :output 10 :cached 500 :cache 0)))))

(ert-deftest gptel-usage-test-cost-unknown-when-read-rate-missing ()
  "Nonzero cache reads without a read rate make cost unknown."
  (let ((gptel-usage-pricing '(("m" . (:input 3.0 :output 15.0)))))
    (should (null (gptel-usage--cost "m" '(:input 100 :output 10 :cached 500))))))

(ert-deftest gptel-usage-test-cost-never-negative-input-term ()
  "A :cache larger than :input must not produce a cost-reducing term.

Guards the subtraction against a future upstream change to the
fold-cache-writes-into-input invariant."
  (let ((gptel-usage-pricing
         '(("m" . (:input 1000.0 :output 0.0 :cache-read 0.0 :cache-write 0.0))))) 
    ;; :cache exceeds :input; the input term must clamp at 0, not go negative.
    (should (>= (gptel-usage--cost "m" '(:input 10 :output 0 :cache 1000)) 0.0))))


;;;; Upstream invariants that costing depends on

(ert-deftest gptel-usage-test-backend-folds-cache-write-into-input ()
  "Anthropic and Bedrock fold cache creation tokens into :input.

`gptel-usage--cost' subtracts :cache from :input to avoid double
billing.  That is only correct while gptel's backends keep folding
cache writes into :input, so pin the behavior here: if gptel changes
it, this fails and the costing must be revisited."
  (require 'gptel-anthropic)
  (require 'gptel-bedrock)
  (let ((info (list :probe nil)))
    (gptel--anthropic-update-tokens
     '(:input_tokens 100 :output_tokens 7
       :cache_creation_input_tokens 1000 :cache_read_input_tokens 5000)
     info)
    (let ((tokens (plist-get info :tokens)))
      (should (equal (plist-get tokens :cache) 1000))
      (should (equal (plist-get tokens :cached) 5000))
      ;; The invariant: :input == fresh input + cache writes.
      (should (equal (plist-get tokens :input) 1100))))
  (let ((info (list :probe nil)))
    (gptel--bedrock-update-tokens
     '(:inputTokens 100 :outputTokens 7
       :cacheWriteInputTokens 1000 :cacheReadInputTokens 5000)
     info)
    (let ((tokens (plist-get info :tokens)))
      (should (equal (plist-get tokens :cache) 1000))
      (should (equal (plist-get tokens :input) 1100)))))

(ert-deftest gptel-usage-test-backend-without-cache-writes ()
  "OpenAI reports no :cache and excludes cache reads from :input.

The other half of the costing assumption: for backends without prompt
cache writes, :cache is absent (so the subtraction is a no-op)."
  (require 'gptel-openai)
  (let ((info (list :probe nil)))
    (gptel--openai-update-tokens
     '(:prompt_tokens 1100 :completion_tokens 7
       :prompt_tokens_details (:cached_tokens 500))
     info)
    (let ((tokens (plist-get info :tokens)))
      (should (null (plist-get tokens :cache)))
      (should (equal (plist-get tokens :cached) 500))
      ;; :input excludes the cached tokens here.
      (should (equal (plist-get tokens :input) 600)))))


;;;; Recording and log round-trip

(ert-deftest gptel-usage-test-record ()
  "Record writes one plist line with tokens, backend, model and cost."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0 :cached 1.0)))))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 1000 :output 500 :cached 200)))
      (let ((records (gptel-usage--read-log)))
        (should (= (length records) 1))
        (let ((r (car records)))
          (should (stringp (plist-get r :timestamp)))
          (should (equal (plist-get r :backend) "OpenAI"))
          (should (equal (plist-get r :model) "gpt-4o-mini"))
          (should (equal (plist-get r :input) 1000))
          (should (equal (plist-get r :output) 500))
          (should (equal (plist-get r :cached) 200))
          ;; 0.01 + 0.01 + 0.0002 = 0.0202 (use tolerance)
          (should (< (abs (- (plist-get r :cost) 0.0202)) 1e-9)))))))

(ert-deftest gptel-usage-test-record-includes-cache-and-version ()
  "Records carry cache write counts and the schema version."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record
       (gptel-usage-test--fsm '(:input 1100 :output 5 :cached 500 :cache 1000)))
      (let ((r (car (gptel-usage--read-log))))
        (should (equal (plist-get r :cache) 1000))
        (should (equal (plist-get r :cached) 500))
        (should (equal (plist-get r :v) gptel-usage-record-version))))))

(ert-deftest gptel-usage-test-record-cache-defaults-to-zero ()
  "Backends that report no cache writes record :cache as 0, not nil."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 10 :output 5)))
      (should (equal (plist-get (car (gptel-usage--read-log)) :cache) 0)))))

(ert-deftest gptel-usage-test-record-no-tokens-no-record ()
  "FSM without :tokens (error/empty usage) appends nothing."
  (gptel-usage-test--with-log
    (gptel-usage--record (gptel-usage-test--fsm))
    (should (null (gptel-usage--read-log)))))

(ert-deftest gptel-usage-test-record-unknown-cost-still-recorded ()
  "Records with unknown pricing are still logged, :cost nil."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 10 :output 20)))
      (should (null (plist-get (car (gptel-usage--read-log)) :cost))))))

(ert-deftest gptel-usage-test-record-errors-contained ()
  "A misconfigured pricing entry must not propagate out of --record."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input "oops")))))
      ;; Should not signal; emits a gptel-usage: message instead.
      (should-not (condition-case err
                      (progn (gptel-usage--record (gptel-usage-test--fsm '(:input 10)))
                             nil)
                    (error err))))))

(ert-deftest gptel-usage-test-read-log-roundtrip ()
  "Multiple records read back in append order."
  (gptel-usage-test--with-log
    (gptel-usage--record (gptel-usage-test--fsm '(:input 1 :output 2)))
    (gptel-usage--record (gptel-usage-test--fsm '(:input 3 :output 4)))
    (let ((records (gptel-usage--read-log)))
      (should (= (length records) 2))
      (should (equal (plist-get (nth 0 records) :input) 1))
      (should (equal (plist-get (nth 1 records) :input) 3)))))

(ert-deftest gptel-usage-test-read-log-missing-file ()
  "Missing log file yields nil, not an error."
  (let ((gptel-usage-log-file (make-temp-file "gptel-usage-nonexistent-")))
    (delete-file gptel-usage-log-file)
    (should (null (gptel-usage--read-log)))))


;;;; gptel-usage-mode advice plumbing

(ert-deftest gptel-usage-test-mode-adds-and-removes-advice ()
  "Enabling the mode advises both FSM handlers; disabling removes it."
  (gptel-usage-mode 1)
  (unwind-protect
      (progn
        (should (advice-member-p #'gptel-usage--record 'gptel--handle-post-insert))
        (should (advice-member-p #'gptel-usage--record 'gptel--handle-error)))
    (gptel-usage-mode -1))
  (should-not (advice-member-p #'gptel-usage--record 'gptel--handle-post-insert))
  (should-not (advice-member-p #'gptel-usage--record 'gptel--handle-error)))

(defmacro gptel-usage-test--with-mode (&rest body)
  "Evaluate BODY with `gptel-usage-mode' enabled, disabling it after."
  (declare (indent 0))
  `(unwind-protect (progn (gptel-usage-mode 1) ,@body)
     (gptel-usage-mode -1)))

(ert-deftest gptel-usage-test-advice-records-via-real-handler ()
  "Calling the real advised handler records usage.

This is the end-to-end check of the advice: rather than calling
`gptel-usage--record' directly, invoke `gptel--handle-post-insert' --
the function `gptel-usage-mode' advises, and the DONE handler in
`gptel-send--handlers' -- with a realistic FSM and confirm a record
lands in the log."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-mode
      (with-temp-buffer
        (insert "hello")
        (let* ((buf (current-buffer))
               (fsm (gptel-make-fsm
                     :table gptel-send--transitions
                     :handlers gptel-send--handlers
                     :info (list :backend (alist-get 'openai gptel-test-backends)
                                 :model 'gpt-4o-mini
                                 :buffer buf
                                 :position (copy-marker (point-max))
                                 :tracking-marker (copy-marker (point-max))
                                 :tokens '(:input 7 :output 3)))))
          (gptel--handle-post-insert fsm)
          (let ((r (car (gptel-usage--read-log))))
            (should r)
            (should (equal (plist-get r :input) 7))
            (should (equal (plist-get r :output) 3))
            (should (equal (plist-get r :model) "gpt-4o-mini"))))))))

(ert-deftest gptel-usage-test-advice-not-recorded-when-mode-off ()
  "With the mode disabled, the real handler records nothing."
  (gptel-usage-test--with-log
    (with-temp-buffer
      (insert "hello")
      (let* ((buf (current-buffer))
             (fsm (gptel-make-fsm
                   :table gptel-send--transitions
                   :handlers gptel-send--handlers
                   :info (list :backend (alist-get 'openai gptel-test-backends)
                               :model 'gpt-4o-mini
                               :buffer buf
                               :position (copy-marker (point-max))
                               :tracking-marker (copy-marker (point-max))
                               :tokens '(:input 7 :output 3)))))
        (gptel--handle-post-insert fsm)
        (should (null (gptel-usage--read-log)))))))

(ert-deftest gptel-usage-test-record-is-idempotent-per-turn ()
  "The same turn's :tokens is recorded once, even via several handlers.

A request that reaches more than one advised handler (or is recorded
twice for any other reason) must not be double-counted."
  (gptel-usage-test--with-log
    (let ((fsm (gptel-usage-test--fsm '(:input 10 :output 20))))
      (gptel-usage--record fsm)
      (gptel-usage--record fsm)
      (gptel-usage--record fsm)
      (should (= (length (gptel-usage--read-log)) 1)))))

(ert-deftest gptel-usage-test-record-new-turn-after-dedup ()
  "A fresh :tokens object on the same FSM is recorded again.

Backends install a new :tokens plist per turn, so multi-turn requests
and retries must not be suppressed by the idempotency guard."
  (gptel-usage-test--with-log
    (let ((fsm (gptel-usage-test--fsm '(:input 10 :output 20))))
      (gptel-usage--record fsm)
      ;; Simulate the next turn: backend installs a fresh plist.
      (plist-put (gptel-fsm-info fsm) :tokens (list :input 1 :output 2))
      (gptel-usage--record fsm)
      (let ((records (gptel-usage--read-log)))
        (should (= (length records) 2))
        (should (equal (plist-get (nth 1 records) :input) 1))))))


;;;; Coverage assumptions
;;
;; gptel-usage.el documents which request paths are tracked.  These tests
;; pin the gptel-side facts that documentation depends on, so the claim
;; cannot silently drift if gptel reshuffles its handler tables.

(ert-deftest gptel-usage-test-coverage-gptel-send-is-tracked ()
  "`gptel-send' FSMs run the advised handlers at DONE and ERRS."
  (should (memq 'gptel--handle-post-insert (alist-get 'DONE gptel-send--handlers)))
  (should (memq 'gptel--handle-error (alist-get 'ERRS gptel-send--handlers))))

(ert-deftest gptel-usage-test-coverage-gptel-request-is-not-tracked ()
  "Plain `gptel-request' FSMs do NOT run the advised handlers.

`gptel-request--handlers' uses `gptel--handle-post' for DONE/ERRS, so
bare `gptel-request' callers are not tracked.  If this test starts
failing, gptel changed its default handlers and the COVERAGE section of
gptel-usage.el should be revisited."
  (require 'gptel-request)
  (should-not (memq 'gptel--handle-post-insert
                    (alist-get 'DONE gptel-request--handlers)))
  (should-not (memq 'gptel--handle-error
                    (alist-get 'ERRS gptel-request--handlers))))

(ert-deftest gptel-usage-test-record-function-shape ()
  "gptel-usage--record accepts an FSM (the handler argument contract).
gptel-send--handlers call gptel--handle-post-insert and
gptel--handle-error with the request FSM; since gptel-usage-mode adds
gptel-usage--record as :after advice on those handlers, --record must
accept an FSM.  This test pins that contract by invoking --record with a
synthetic FSM and asserting the log is written (success path) or left
untouched (no tokens)."
  (gptel-usage-test--with-log
    (gptel-usage--record (gptel-usage-test--fsm '(:input 5 :output 5)))
    (should (= (length (gptel-usage--read-log)) 1))
    (gptel-usage--record (gptel-usage-test--fsm))
    (should (= (length (gptel-usage--read-log)) 1))))

(ert-deftest gptel-usage-test-record-tokens-not-tokens-full ()
  "Data source is :tokens (this turn), not :tokens-full (whole request).

Pins the committed `gptel-usage--record' contract: of the two keys
gptel keeps on the FSM info plist (\"per-turn\" :tokens and cumulative
:tokens-full), the record function reads :tokens.  So the logged
numbers must match exactly what is in :tokens, even when :tokens-full
carries a different (larger) value."
  (gptel-usage-test--with-log
    (let ((fsm (gptel-make-fsm
                :info (list :backend (alist-get 'openai gptel-test-backends)
                            :model 'gpt-4o-mini
                            :tokens '(:input 10 :output 20 :cached 5)
                            :tokens-full '(:input 1000 :output 2000 :cached 500)))))
      (gptel-usage--record fsm)
      (let ((r (car (gptel-usage--read-log))))
        (should (equal (plist-get r :input) 10))
        (should (equal (plist-get r :output) 20))
        (should (equal (plist-get r :cached) 5))))))


;;;; Report rendering

(ert-deftest gptel-usage-test-report ()
  "Report renders header, per-model group and total cost."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0 :cached 1.0)))))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 1000000 :output 1000000)))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 1000000 :output 1000000)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let ((text (buffer-string)))
          (should (string-match-p "Backend" text))
          (should (string-match-p "gpt-4o-mini" text))
          ;; 2 requests × (10 + 20) USD
          (should (string-match-p "\\$60\\.0000" text)))))))

(ert-deftest gptel-usage-test-report-unknown-total ()
  "Report flags unknown-priced groups and totals only known cost."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 10 :output 10)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let ((text (buffer-string)))
          (should (string-match-p "unknown" text))
          (should (string-match-p "no pricing configured" text)))))))

(ert-deftest gptel-usage-test-report-reads-legacy-records ()
  "Pre-v2 records (no :cache, no :v) still read and report as zero writes.

Logs are append-only, so a v2 reader must tolerate records written by
the original schema."
  (let ((gptel-usage-log-file (make-temp-file "gptel-usage-test-")))
    (unwind-protect
        (progn
          (with-temp-file gptel-usage-log-file
            ;; Exactly the original (v1) record shape.
            (insert (prin1-to-string
                     '(:timestamp "2024-01-01T00:00:00+0000" :backend "OpenAI"
                       :model "gpt-4o-mini" :input 100 :output 50 :cached 10
                       :cost 0.5))
                    "\n"))
          (let ((records (gptel-usage--read-log)))
            (should (= (length records) 1))
            (should (null (plist-get (car records) :v))))
          ;; Must not error on the absent :cache key.
          (gptel-usage-report)
          (with-current-buffer "*gptel-usage*"
            (let ((text (buffer-string)))
              (should (string-match-p "gpt-4o-mini" text))
              (should (string-match-p "\\$0\\.5000" text)))))
      (ignore-errors (delete-file gptel-usage-log-file)))))

(ert-deftest gptel-usage-test-report-mixed-versions ()
  "A log holding both v1 and v2 records aggregates cleanly."
  (let ((gptel-usage-log-file (make-temp-file "gptel-usage-test-")))
    (unwind-protect
        (progn
          (with-temp-file gptel-usage-log-file
            (insert (prin1-to-string
                     '(:timestamp "2024-01-01T00:00:00+0000" :backend "B"
                       :model "m" :input 100 :output 0 :cached 0 :cost 1.0))
                    "\n"
                    (prin1-to-string
                     '(:v 2 :timestamp "2024-01-02T00:00:00+0000" :backend "B"
                       :model "m" :input 100 :output 0 :cached 0 :cache 50
                       :cost 2.0))
                    "\n"))
          (gptel-usage-report)
          (with-current-buffer "*gptel-usage*"
            (let ((text (buffer-string)))
              ;; 2 requests, cache writes only from the v2 record.
              (should (string-match-p "\\$3\\.0000" text)))))
      (ignore-errors (delete-file gptel-usage-log-file)))))

(provide 'gptel-usage-test)
;;; gptel-usage-test.el ends here
