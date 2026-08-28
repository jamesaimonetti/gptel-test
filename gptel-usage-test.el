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


;;;; Per-buffer cost in the header line

(defmacro gptel-usage-test--with-header-mode (&rest body)
  "Evaluate BODY with `gptel-usage-header-line-mode' enabled."
  (declare (indent 0))
  `(unwind-protect (progn (gptel-usage-header-line-mode 1) ,@body)
     (gptel-usage-header-line-mode -1)))

(defun gptel-usage-test--record-in (buffer tokens &optional model)
  "Record TOKENS as a request made in BUFFER, for MODEL."
  (gptel-usage--record
   (gptel-make-fsm
    :info (list :backend (alist-get 'openai gptel-test-backends)
                :model (or model 'gpt-4o-mini)
                :buffer buffer
                :tokens tokens))))

(ert-deftest gptel-usage-test-buffer-cost-accumulates ()
  "Costs accumulate per buffer: last request and running total."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
      (with-temp-buffer
        (gptel-usage-test--record-in (current-buffer) '(:input 1000000 :output 0))
        (should (< (abs (- gptel-usage--last-cost 10.0)) 1e-9))
        (should (< (abs (- gptel-usage--buffer-cost 10.0)) 1e-9))
        (gptel-usage-test--record-in (current-buffer) '(:input 2000000 :output 0))
        ;; Last is this request only; the total covers both.
        (should (< (abs (- gptel-usage--last-cost 20.0)) 1e-9))
        (should (< (abs (- gptel-usage--buffer-cost 30.0)) 1e-9))
        (should-not gptel-usage--buffer-cost-partial)))))

(ert-deftest gptel-usage-test-buffer-cost-is-per-buffer ()
  "Each buffer keeps its own total; requests do not leak across buffers."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
      (with-temp-buffer
        (let ((a (current-buffer)))
          (with-temp-buffer
            (let ((b (current-buffer)))
              (gptel-usage-test--record-in a '(:input 1000000 :output 0))
              (gptel-usage-test--record-in b '(:input 3000000 :output 0))
              (should (< (abs (- (buffer-local-value 'gptel-usage--buffer-cost a)
                                 10.0))
                         1e-9))
              (should (< (abs (- (buffer-local-value 'gptel-usage--buffer-cost b)
                                 30.0))
                         1e-9)))))))))

(ert-deftest gptel-usage-test-buffer-cost-unpriced-is-partial ()
  "An unpriced request is excluded from the total and flags it partial.

Counting it as zero would understate the cost, the same failure the
nil-pricing convention exists to avoid."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
      (with-temp-buffer
        (gptel-usage-test--record-in (current-buffer) '(:input 1000000 :output 0))
        (gptel-usage-test--record-in (current-buffer) '(:input 5000000 :output 0)
                                     'unpriced-model)
        (should (null gptel-usage--last-cost))
        ;; Total unchanged by the unpriced request, but marked partial.
        (should (< (abs (- gptel-usage--buffer-cost 10.0)) 1e-9))
        (should gptel-usage--buffer-cost-partial)))))

(ert-deftest gptel-usage-test-record-survives-dead-buffer ()
  "Recording still works when the request buffer is gone.

Two separate guarantees: the log write does not depend on the buffer
being live (it happens first), and updating the per-buffer totals skips
a dead buffer rather than erroring.  `with-current-buffer' signals on a
dead buffer, so without the liveness check the error would be caught by
the handler in `gptel-usage--record' and reported to the user."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil)
          (dead (generate-new-buffer " *gptel-usage-dead*"))
          (complaints nil))
      (kill-buffer dead)
      (should-not (buffer-live-p dead))
      (cl-letf* ((orig (symbol-function 'message))
                 ((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (when (and (stringp fmt) (string-prefix-p "gptel-usage:" fmt))
                      (push (apply #'format fmt args) complaints))
                    (apply orig fmt args))))
        (gptel-usage-test--record-in dead '(:input 10 :output 5)))
      ;; The record is written...
      (should (= (length (gptel-usage--read-log)) 1))
      ;; ...and no failure was reported along the way.
      (should-not complaints))))

(ert-deftest gptel-usage-test-header-annotates-both-scopes ()
  "The header indicator gains the last-request and buffer costs."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
        (with-temp-buffer
          (let ((tokens '(:input 1000000 :output 0)))
            (gptel-usage-test--record-in (current-buffer) tokens)
            (gptel--update-token-usage tokens tokens))
          ;; (IDX REQUEST BUFFER)
          (should (string-match-p "\\$10\\.00" (nth 1 gptel--token-usage-strings)))
          (should (string-match-p "\\$10\\.00" (nth 2 gptel--token-usage-strings)))
          ;; Token text is kept, not replaced.
          (should (string-match-p "1M" (nth 1 gptel--token-usage-strings))))))))

(ert-deftest gptel-usage-test-header-survives-later-requests ()
  "Costs are re-appended after gptel rebuilds the display strings.

`gptel--update-token-usage' rebuilds them from scratch each time, so a
one-shot annotation would be wiped by the next request."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
        (with-temp-buffer
          (let ((t1 '(:input 1000000 :output 0)))
            (gptel-usage-test--record-in (current-buffer) t1)
            (gptel--update-token-usage t1 t1))
          (let ((t2 '(:input 1000000 :output 0)))
            (gptel-usage-test--record-in (current-buffer) t2)
            (gptel--update-token-usage t2 t2))
          ;; Second request: last is 10, buffer total is 20.
          (should (string-match-p "\\$10\\.00" (nth 1 gptel--token-usage-strings)))
          (should (string-match-p "\\$20\\.00" (nth 2 gptel--token-usage-strings)))
          ;; And not doubly appended.
          (should-not (string-match-p "\\$.*\\$" (nth 2 gptel--token-usage-strings))))))))

(ert-deftest gptel-usage-test-header-marks-partial-total ()
  "A total omitting unpriced requests is shown with a trailing \"+\"."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
        (with-temp-buffer
          (let ((tokens '(:input 1000000 :output 0)))
            (gptel-usage-test--record-in (current-buffer) tokens)
            (gptel--update-token-usage tokens tokens))
          (let ((tokens '(:input 500 :output 0)))
            (gptel-usage-test--record-in (current-buffer) tokens 'unpriced)
            (gptel--update-token-usage tokens tokens))
          ;; Unknown per-request cost: no cost on the request scope at all.
          (should-not (string-match-p "\\$" (nth 1 gptel--token-usage-strings)))
          (should (string-match-p "\\$10\\.00\\+" (nth 2 gptel--token-usage-strings))))))))

(ert-deftest gptel-usage-test-header-silent-until-first-cost ()
  "A buffer with nothing recorded shows no cost, not \"$0.00\".

`gptel-usage--buffer-cost' starting at 0.0 would render as $0.00 and
claim the session was free, which is exactly wrong in the case that
matters: usage is being tracked but nothing has been priced yet."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (with-temp-buffer
        ;; gptel reports token usage, but nothing has been recorded.
        (let ((tokens '(:input 1000 :output 500)))
          (gptel--update-token-usage tokens tokens))
        (should-not (string-match-p "\\$" (nth 1 gptel--token-usage-strings)))
        (should-not (string-match-p "\\$" (nth 2 gptel--token-usage-strings)))))))

(ert-deftest gptel-usage-test-header-silent-when-only-unpriced ()
  "Requests that are all unpriced show no total, rather than \"$0.00+\"."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (let ((gptel-usage-pricing nil))
        (with-temp-buffer
          (let ((tokens '(:input 1000 :output 500)))
            (gptel-usage-test--record-in (current-buffer) tokens 'unpriced)
            (gptel--update-token-usage tokens tokens))
          (should gptel-usage--buffer-cost-partial)
          (should (null gptel-usage--buffer-cost))
          (should-not (string-match-p "\\$" (nth 2 gptel--token-usage-strings))))))))

(ert-deftest gptel-usage-test-format-cost ()
  "Cost formatting never renders a nonzero cost as free."
  ;; Nothing recorded / unknown: no string at all.
  (should (null (gptel-usage--format-cost nil)))
  ;; A real zero from a priced model is legitimate.
  (should (equal (gptel-usage--format-cost 0.0) "$0.00"))
  ;; Below display precision: report a bound, never "$0.0000".
  (should (equal (gptel-usage--format-cost 0.00001) "<$0.0001"))
  (should-not (equal (gptel-usage--format-cost 0.00001) "$0.0000"))
  ;; Sub-dollar keeps 4dp; dollars and up use 2dp.
  (should (equal (gptel-usage--format-cost 0.2043245) "$0.2043"))
  (should (equal (gptel-usage--format-cost 12.3456) "$12.35"))
  ;; The partial marker rides along.
  (should (equal (gptel-usage--format-cost 1.5 t) "$1.50+")))

(ert-deftest gptel-usage-test-header-mode-off-does-not-annotate ()
  "With the display mode off, gptel's indicator is left alone."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
      (with-temp-buffer
        (let ((tokens '(:input 1000000 :output 0)))
          (gptel-usage-test--record-in (current-buffer) tokens)
          (gptel--update-token-usage tokens tokens))
        (should-not (string-match-p "\\$" (nth 1 gptel--token-usage-strings)))
        (should-not (string-match-p "\\$" (nth 2 gptel--token-usage-strings)))))))

(ert-deftest gptel-usage-test-header-renders-in-segment ()
  "The cost reaches gptel's rendered header-line segment and tooltip.

End-to-end check of what the user actually sees: evaluate gptel's own
header-line info form and look for the cost in both the visible text
and the tooltip, which lists both scopes at once."
  (gptel-usage-test--with-log
    (gptel-usage-test--with-header-mode
      (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 0.0)))))
        (with-temp-buffer
          (setq-local gptel-mode t
                      gptel-backend (alist-get 'openai gptel-test-backends)
                      gptel-model 'gpt-4o-mini)
          (let ((tokens '(:input 1000000 :output 0)))
            (gptel-usage-test--record-in (current-buffer) tokens)
            (gptel--update-token-usage tokens tokens))
          ;; `gptel--header-line-info' is (:eval FORM); evaluate FORM directly,
          ;; as batch has no window for its align-to spacer.
          (let* ((seg (eval (cadr gptel--header-line-info) t))
                 (text (substring-no-properties seg)))
            (should (string-match-p "\\$10\\.00" text))
            ;; The tooltip is built from the same strings, so it gains the
            ;; costs for both scopes.
            (let ((tip (get-text-property 2 'help-echo seg)))
              (should (stringp tip))
              (should (string-match-p "Last request:.*\\$10\\.00" tip))
              (should (string-match-p "This buffer:.*\\$10\\.00" tip)))))))))

(ert-deftest gptel-usage-test-header-mode-advice-plumbing ()
  "Enabling and disabling the display mode adds and removes the advice."
  (gptel-usage-header-line-mode 1)
  (unwind-protect
      (should (advice-member-p #'gptel-usage--annotate-header
                               'gptel--update-token-usage))
    (gptel-usage-header-line-mode -1))
  (should-not (advice-member-p #'gptel-usage--annotate-header
                               'gptel--update-token-usage)))

(ert-deftest gptel-usage-test-header-strings-shape ()
  "gptel's token display strings are (IDX REQUEST BUFFER).

`gptel-usage--annotate-header' writes into slots 1 and 2 of this list.
If gptel changes the shape, this fails instead of the annotation
silently landing in the wrong place."
  (with-temp-buffer
    (gptel--update-token-usage '(:input 10 :output 5) '(:input 10 :output 5))
    (should (= (length gptel--token-usage-strings) 3))
    (should (integerp (car gptel--token-usage-strings)))
    (should (stringp (nth 1 gptel--token-usage-strings)))
    (should (stringp (nth 2 gptel--token-usage-strings)))))


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

(defun gptel-usage-test--report-table ()
  "Return the report's Org table from point-min as a Lisp structure.
Rows are lists of cell strings; horizontal rules appear as the symbol
`hline'.  Signals if the buffer holds no table."
  (require 'org-table)
  (goto-char (point-min))
  (should (re-search-forward "^|" nil t))
  (org-table-to-lisp))

(defun gptel-usage-test--report-row (model)
  "Return the report row for MODEL as a list of cell strings, or nil."
  (cl-find-if (lambda (row) (and (listp row) (equal (nth 1 row) model)))
              (gptel-usage-test--report-table)))

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
          ;; 2 requests × (10 + 20) USD.  Costs are bare numbers so the Org
          ;; column stays numeric.
          (should (string-match-p "\\b60\\.0000\\b" text))
          (should-not (string-match-p "\\$" text)))))))


;;;; Org output

(ert-deftest gptel-usage-test-report-buffer-is-org-mode ()
  "The report buffer is in `org-mode'."
  (gptel-usage-test--with-log
    (gptel-usage--record (gptel-usage-test--fsm '(:input 10 :output 5)))
    (gptel-usage-report)
    (with-current-buffer "*gptel-usage*"
      (should (derived-mode-p 'org-mode))
      ;; Writable, so Org table commands (sort, formulas) work.
      (should-not buffer-read-only)
      ;; Freshly generated content should not look like unsaved edits.
      (should-not (buffer-modified-p)))))

(ert-deftest gptel-usage-test-report-is-a-valid-org-table ()
  "The report parses as an Org table with the expected shape."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 1000000 :output 0)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let* ((table (gptel-usage-test--report-table))
               (header (car table)))
          ;; Header, hline, one data row, hline, total row.
          (should (equal header '("Backend" "Model" "Reqs" "Input" "Output"
                                  "CacheRd" "CacheWr" "Cost (USD)")))
          (should (memq 'hline table))
          ;; Every non-rule row has the same number of cells as the header.
          (dolist (row table)
            (when (listp row)
              (should (= (length row) (length header)))))
          ;; Last row is the total.
          (should (equal (car (car (last table))) "Total")))))))

(ert-deftest gptel-usage-test-report-total-row ()
  "The total row sums requests, tokens and known cost."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 1000000 :output 0)))
      (gptel-usage--record (gptel-usage-test--fsm '(:input 3000000 :output 0)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let ((total (car (last (gptel-usage-test--report-table)))))
          (should (equal (nth 0 total) "Total"))
          (should (equal (nth 2 total) "2"))         ;requests
          (should (equal (nth 3 total) "4000000"))   ;input
          (should (equal (nth 7 total) "40.0000"))))))) ;4M * $10/M

(ert-deftest gptel-usage-test-report-escapes-pipes ()
  "A \"|\" in a backend or model name cannot break the table.

An unescaped pipe splits a cell in two and shifts every later column.
Note that `org-table-align' pads all rows to the widest one, so simply
comparing cell counts between rows does NOT catch this -- the header
gets padded too.  Assert the absolute column count and the position of
a known value instead."
  (gptel-usage-test--with-log
    (let* ((gptel-usage-pricing nil)
           (backend (gptel--make-openai :name "we|rd" :models '(m)))
           (fsm (gptel-make-fsm
                 :info (list :backend backend :model 'a\|b
                             :tokens '(:input 10 :output 5)))))
      (gptel-usage--record fsm)
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let* ((table (gptel-usage-test--report-table))
               (header (car table))
               (row (nth 2 table)))
          ;; Two extra pipes would widen every row to 10 columns.
          (should (= (length header) 8))
          (should (= (length row) 8))
          ;; Columns stay in place: Reqs is still the third cell.
          (should (equal (nth 2 row) "1"))
          (should (equal (nth 3 row) "10"))
          ;; The name survives in escaped form, not as a split cell.
          (should (string-match-p "vert" (nth 0 row)))
          (should-not (string-match-p "|" (nth 0 row))))))))

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

(ert-deftest gptel-usage-test-report-shows-cache-columns ()
  "Report has separate cache read and write columns, with the counts."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record
       (gptel-usage-test--fsm '(:input 1100 :output 5 :cached 700 :cache 300)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let ((text (buffer-string)))
          (should (string-match-p "CacheRd" text))
          (should (string-match-p "CacheWr" text))
          (should (string-match-p "700" text))
          (should (string-match-p "300" text)))))))

(ert-deftest gptel-usage-test-report-columns-are-disjoint ()
  "Reported Input excludes cache writes, so token columns do not overlap.

Backends that report cache writes fold them into :input.  Showing that
raw number next to a CacheWr column would double-count the same tokens
on screen, so the report subtracts them: for :input 1100 :cache 300 the
Input column must read 800."
  (gptel-usage-test--with-log
    (let ((gptel-usage-pricing nil))
      (gptel-usage--record
       (gptel-usage-test--fsm '(:input 1100 :output 5 :cached 700 :cache 300)))
      (gptel-usage-report)
      (with-current-buffer "*gptel-usage*"
        (let ((row (gptel-usage-test--report-row "gpt-4o-mini")))
          (should row)
          (should (equal (nth 2 row) "1"))     ;Reqs
          (should (equal (nth 3 row) "800"))   ;Input = 1100 - 300
          (should (equal (nth 4 row) "5"))     ;Output
          (should (equal (nth 5 row) "700"))   ;CacheRd
          (should (equal (nth 6 row) "300")))))))

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
            (let ((row (gptel-usage-test--report-row "gpt-4o-mini")))
              (should row)
              (should (equal (nth 6 row) "0"))     ;CacheWr defaults to 0
              (should (equal (nth 7 row) "0.5000")))))
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
            (let ((row (gptel-usage-test--report-row "m")))
              (should row)
              (should (equal (nth 2 row) "2"))     ;both records counted
              ;; Cache writes come only from the v2 record.
              (should (equal (nth 6 row) "50"))
              (should (equal (nth 7 row) "3.0000")))))
      (ignore-errors (delete-file gptel-usage-log-file)))))

(provide 'gptel-usage-test)
;;; gptel-usage-test.el ends here
