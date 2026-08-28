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
  "Missing :input/:output/:cached in TOKENS or pricing count as zero."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
    ;; No :cached in pricing, no :cached in tokens.
    (should (equal (gptel-usage--cost "gpt-4o-mini" '(:input 100000 :output 200000)) 5.0))))

(ert-deftest gptel-usage-test-cost-unknown-model-returns-nil ()
  "A model with no pricing entry yields nil (unknown), not 0."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . (:input 10.0 :output 20.0)))))
    (should (null (gptel-usage--cost "some-other-model" '(:input 100 :output 100))))))

(ert-deftest gptel-usage-test-cost-nil-pricing-returns-nil ()
  "A model explicitly mapped to nil pricing counts as unknown."
  (let ((gptel-usage-pricing '(("gpt-4o-mini" . nil))))
    (should (null (gptel-usage--cost "gpt-4o-mini" '(:input 100 :output 100))))))


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

(provide 'gptel-usage-test)
;;; gptel-usage-test.el ends here
