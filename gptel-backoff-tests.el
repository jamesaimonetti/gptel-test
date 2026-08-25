;;; gptel-backoff-tests.el --- Tests for gptel-backoff  -*- lexical-binding: t; -*-

;; Tests for the retry/backoff + concurrency limiting feature
;; (gptel-backoff.el).  Pure/unit tests only; live transport tests live
;; outside this repository (see PLAN.md "End-to-end smoke tests").

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-request)
(require 'gptel-backoff)

(defconst gptel-backoff-tests--backend
  (gptel-make-openai "TEST-BACKEND" :key "sk-test" :models '(test-model))
  "Backend used by the unit tests.")

(ert-deftest gptel-backoff-setting-normalizes-keywords ()
  "Per-backend plists are keyed by keywords; plain symbols normalize."
  (let ((gptel-backoff--backend-settings
         '(("TEST-BACKEND" :max-retries 3 :concurrency 2))))
    (should (= (gptel-backoff--setting gptel-backoff-tests--backend
                                       :max-retries 7)
               3))
    (should (= (gptel-backoff--setting gptel-backoff-tests--backend
                                       'max-retries 7)
               3))
    (should (= (gptel-backoff--setting gptel-backoff-tests--backend
                                       'concurrency nil)
               2))
    ;; Unknown key falls back to default.
    (should (= (gptel-backoff--setting gptel-backoff-tests--backend
                                       'base-delay 1.5)
               1.5))))

(ert-deftest gptel-backoff-status-int ()
  "Status strings and integers compare equal."
  (should (= (gptel-backoff--status-int "429") 429))
  (should (= (gptel-backoff--status-int 429) 429)))

(ert-deftest gptel-backoff-retry-after-parses ()
  "Retry-After: delta-seconds, HTTP-date, absent, and garbage."
  (should (= (gptel-backoff--retry-after '(("retry-after" . "10"))) 10))
  ;; HTTP-date in the future yields the number of seconds.
  (should (integerp (gptel-backoff--retry-after
                     (list (cons "retry-after"
                                 (format-time-string
                                  "%a, %d %b %Y %H:%M:%S GMT"
                                  (time-add (current-time)
                                            (seconds-to-time 30))))))))
  (should (null (gptel-backoff--retry-after nil)))
  (should (null (gptel-backoff--retry-after '(("content-type" . "text/plain")))))
  (should (null (gptel-backoff--retry-after '(("retry-after" . "not-a-date"))))))

(ert-deftest gptel-backoff-jitter-identity-and-range ()
  "Factor 0 is identity; otherwise delay stays within [1-f,1+f]."
  (should (= (gptel-backoff--jitter 10.0 0) 10.0))
  (should (= (gptel-backoff--jitter 10.0 nil) 10.0))
  (let ((gptel-backoff-jitter-factor 0.2))
    (dotimes (_ 50)
      (let ((d (gptel-backoff--jitter 10.0 0.2)))
        (should (<= 8.0 d 12.0))
        (should (numberp d))))))

(ert-deftest gptel-backoff-delay-exponential-and-cap ()
  "Delay is base*2^(attempt-1), capped at max-delay; retry-after is a floor."
  (let ((info (list :backend gptel-backoff-tests--backend :http-headers nil)))
    (let ((gptel-backoff-base-delay 1.0)
          (gptel-backoff-max-delay 60.0)
          (gptel-backoff-jitter-factor 0.0))
      (should (= (gptel-backoff--delay 1 info) 1.0))
      (should (= (gptel-backoff--delay 2 info) 2.0))
      (should (= (gptel-backoff--delay 3 info) 4.0)))
    ;; Cap.
    (let ((gptel-backoff-base-delay 10.0)
          (gptel-backoff-max-delay 15.0)
          (gptel-backoff-jitter-factor 0.0))
      (should (= (gptel-backoff--delay 3 info) 15.0)))
    ;; Retry-After floor.
    (let ((gptel-backoff-base-delay 1.0)
          (gptel-backoff-max-delay 60.0)
          (gptel-backoff-jitter-factor 0.0))
      (should (= (gptel-backoff--delay 1
                                       (list :backend gptel-backoff-tests--backend
                                             :http-headers '(("retry-after" . "20"))))
                 20.0)))))

(ert-deftest gptel-backoff-retryable-p-status-and-headers ()
  "Header wins; retryable statuses and error types; non-retryable otherwise."
  (should (gptel-backoff--retryable-p
           gptel-backoff-tests--backend
           (list :http-status "429" :http-headers nil :error nil)))
  (should (gptel-backoff--retryable-p
           gptel-backoff-tests--backend
           (list :http-status 503 :http-headers nil :error nil)))
  (should (not (gptel-backoff--retryable-p
                gptel-backoff-tests--backend
                (list :http-status "400" :http-headers nil :error nil))))
  (should (not (gptel-backoff--retryable-p
                gptel-backoff-tests--backend
                (list :http-status "401" :http-headers nil :error nil))))
  ;; x-should-retry wins over status.
  (should (not (gptel-backoff--retryable-p
                gptel-backoff-tests--backend
                (list :http-status "429"
                      :http-headers '(("x-should-retry" . "false"))
                      :error nil))))
  (should (gptel-backoff--retryable-p
           gptel-backoff-tests--backend
           (list :http-status "200"
                 :http-headers '(("x-should-retry" . "true"))
                 :error nil)))
  ;; JSON error type, case-insensitive.
  (should (gptel-backoff--retryable-p
           gptel-backoff-tests--backend
           (list :http-status "200" :http-headers nil
                 :error '(:type "overloaded_error" :message "boom"))))
  (should (gptel-backoff--retryable-p
           gptel-backoff-tests--backend
           (list :http-status "200" :http-headers nil
                 :error '(:code "RATE_LIMIT_ERROR"))))
  ;; Unknown error is not retried (conservative).
  (should (not (gptel-backoff--retryable-p
                gptel-backoff-tests--backend
                (list :http-status "200" :http-headers nil
                      :error '(:type "invalid_request_error" :message "nope"))))))

(ert-deftest gptel-backoff-error-retryable-p ()
  "Accepts plist and string error data; empty strings are not retryable."
  (should (gptel-backoff--error-retryable-p '(:type "overloaded_error")))
  (should (gptel-backoff--error-retryable-p "server_error"))
  (should (not (gptel-backoff--error-retryable-p "some other thing")))
  (should (not (gptel-backoff--error-retryable-p nil)))
  (should (not (gptel-backoff--error-retryable-p ""))))

(ert-deftest gptel-backoff-retry-p-budget-and-toggle ()
  "retry-p respects the toggle, the attempt budget, and a nil backend."
  (let* ((gptel-backoff-max-retries 2)
         (gptel-backoff-enabled t)
         (no-backend (list :http-status "429" :http-headers nil :error nil)))
    ;; No backend => never retryable by design.
    (should (not (gptel-backoff--retry-p no-backend)))
    (let ((info (list :backend gptel-backoff-tests--backend
                      :http-status "429" :http-headers nil :error nil)))
      (should (gptel-backoff--retry-p (plist-put (copy-sequence info)
                                                 :backoff-attempts 0)))
      (should (gptel-backoff--retry-p (plist-put (copy-sequence info)
                                                 :backoff-attempts 1)))
      ;; Budget exhausted.
      (should (not (gptel-backoff--retry-p (plist-put (copy-sequence info)
                                                      :backoff-attempts 2))))
      ;; Disabled.
      (let ((gptel-backoff-enabled nil))
        (should (not (gptel-backoff--retry-p info)))))))

(ert-deftest gptel-backoff-install-idempotent-and-layout ()
  "Install adds RTRY/QUEUE, orders retry-p before error-p, wraps WAIT, is idempotent."
  (let ((fsm (gptel-make-fsm)))
    (gptel-backoff--install fsm)
    (let ((table (gptel-fsm-table fsm))
          (handlers (gptel-fsm-handlers fsm)))
      ;; RTRY and QUEUE rows exist and lead to WAIT.
      (should (equal (cdr (assq 'RTRY table)) '((t . WAIT))))
      (should (equal (cdr (assq 'QUEUE table)) '((t . WAIT))))
      ;; Type row: retry predicate precedes the error predicate.
      (let ((type-row (cdr (assq 'TYPE table))))
        (should (eq (caar type-row) #'gptel-backoff--retry-p))
        (should (eq (car (cadr type-row)) #'gptel--error-p)))
      ;; TRET row likewise.
      (let ((tret-row (cdr (assq 'TRET table))))
        (should (eq (caar tret-row) #'gptel-backoff--retry-p))
        (should (eq (car (cadr tret-row)) #'gptel--error-p)))
      ;; WAIT handler is the limiter gate (a closure), not gptel--handle-wait.
      (should (not (memq #'gptel--handle-wait (cdr (assq 'WAIT handlers)))))
      ;; New handlers present.
      (should (assq 'RTRY handlers))
      (should (assq 'QUEUE handlers))
      ;; Attempt counter initialised.
      (should (= (plist-get (gptel-fsm-info fsm) :backoff-attempts) 0)))
    ;; Idempotent: reinstalling does not duplicate rows or handlers.
    (gptel-backoff--install fsm)
    (let ((table (gptel-fsm-table fsm))
          (handlers (gptel-fsm-handlers fsm)))
      (should (= (cl-count-if (lambda (row) (eq (car row) 'RTRY)) table) 1))
      (should (= (cl-count-if (lambda (row) (eq (car row) 'QUEUE)) table) 1))
      (should (= (cl-count-if (lambda (row) (eq (car row) 'RTRY)) handlers) 1)))))

(ert-deftest gptel-backoff-installed-p ()
  "installed-p keys off the presence of the RTRY row."
  (let ((fsm (gptel-make-fsm)))
    (should (not (gptel-backoff--installed-p fsm)))
    (gptel-backoff--install fsm)
    (should (gptel-backoff--installed-p fsm))))

(ert-deftest gptel-backoff-semaphore-acquire-and-release ()
  "Semaphore accounting: acquire increments, release decrements and pumps."
  ;; Use a dedicated backend name so the semaphore table is fresh; the
  ;; global table is shared across tests.
  (let* ((backend (gptel-make-openai "SEM-BACKEND" :key "sk" :models '(m)))
         (gptel-backoff-default-concurrency 1)
         (sem (gptel-backoff--semaphore backend))
         (queued (gptel-make-fsm)))
    (setf (nth 1 sem) (list queued))
    (should (gptel-backoff--acquire backend sem))
    (should (= (nth 0 sem) 1))
    ;; Over limit now.
    (should (not (gptel-backoff--acquire backend sem)))
    (should (eq (gptel-fsm-state queued) 'INIT)) ;still queued
    ;; Release pumps the queued FSM: it must transition to WAIT.  Stub the
    ;; WAIT handler so the resumed FSM does not try to dispatch a request
    ;; (it has no :backend/request data in this unit test).
    (cl-letf (((symbol-function 'gptel--handle-wait) (lambda (_fsm))))
      (gptel-backoff--release-backend backend sem))
    (should (= (nth 0 sem) 0))
    (should (eq (gptel-fsm-state queued) 'WAIT))
    (should (null (nth 1 sem)))))

(ert-deftest gptel-backoff-release-removes-queued-and-dispatched ()
  "release() is idempotent and clears both queue position and slot."
  (let* ((gptel-backoff-default-concurrency 2)
         (backend gptel-backoff-tests--backend)
         (fsm (gptel-make-fsm)))
    (setf (gptel-fsm-info fsm)
          (list :backend backend
                :http-status "200" :http-headers nil :error nil
                :backoff-dispatched t
                :queued t))
    (let ((sem (gptel-backoff--semaphore backend)))
      (setf (nth 0 sem) 5)             ;pretend other requests are active
      (setf (nth 1 sem) (list fsm)))
    ;; Dispatched + queued simultaneously should not normally happen, but
    ;; release must be safe.
    (gptel-backoff--release fsm)
    (let ((sem (gptel-backoff--semaphore backend)))
      (should (= (nth 0 sem) 4))
      (should (null (nth 1 sem)))
      (should (not (plist-get (gptel-fsm-info fsm) :queued)))
      (should (not (plist-get (gptel-fsm-info fsm) :backoff-dispatched))))
    ;; Second call: no-op.
    (gptel-backoff--release fsm)
    (should (= (nth 0 (gptel-backoff--semaphore backend)) 4))))

(ert-deftest gptel-backoff-release-sets-cooldown-on-429 ()
  "A finished retryable 429 with the limiter enabled cooldowns the backend."
  (let* ((backend gptel-backoff-tests--backend)
         (gptel-backoff--respect-retry-after t)
         (gptel-backoff-cooldown 30.0)
         (gptel-backoff-jitter-factor 0.0)
         (fsm (gptel-make-fsm)))
    (setf (gptel-fsm-info fsm)
          (list :backend backend
                :http-status "429"
                :http-headers '(("retry-after" . "15"))
                :backoff-dispatched t))
    (gptel-backoff--release fsm)
    (let ((sem (gptel-backoff--semaphore backend)))
      (should (time-less-p (current-time) (nth 2 sem))))))

(ert-deftest gptel-backoff-truncate-stream-deletes-partial-output ()
  "truncate-stream removes the region between :position and :tracking-marker."
  (let* ((buf (get-buffer-create "*bt-buffer*"))
         (pos (set-marker (make-marker) (point-min) buf)))
    (with-current-buffer buf
      (insert "PREFIX ")                ;stable text before the response
      (move-marker pos (point))         ;response starts here
      (insert "PARTIAL DIRTY")          ;streamed, then the stream failed
      (let ((info (list :stream t
                        :position (copy-marker pos)
                        :tracking-marker (copy-marker (point) buf)
                        :partial_text '("DIRTY") :partial_json '("x"))))
        (gptel-backoff--truncate-stream info)
        (should (equal (buffer-string) "PREFIX "))
        (should (null (plist-get info :tracking-marker)))
        (should (null (plist-get info :partial_text)))
        (should (null (plist-get info :partial_json)))))
    (kill-buffer buf)))

(ert-deftest gptel-backoff-truncate-stream-noop-non-stream ()
  "truncate-stream does nothing for non-streaming requests."
  (let ((info (list :stream nil :position nil)))
    (gptel-backoff--truncate-stream info)
    (should (equal info (list :stream nil :position nil)))))

(ert-deftest gptel-backoff-fire-guards ()
  "fire() no-ops on stale/cancelled fsms or dead buffers, transitions otherwise."
  ;; Stale: state is no longer RTRY.
  (let ((fsm (gptel-make-fsm))
        (info (list :buffer (current-buffer))))
    (setf (gptel-fsm-state fsm) 'DONE)
    (setf (gptel-fsm-info fsm) info)
    (gptel-backoff--fire fsm)
    (should (eq (gptel-fsm-state fsm) 'DONE)))
  ;; Cancelled.
  (let ((fsm (gptel-make-fsm))
        (info (list :cancelled t :buffer (current-buffer))))
    (setf (gptel-fsm-state fsm) 'RTRY)
    (setf (gptel-fsm-info fsm) info)
    (gptel-backoff--fire fsm)
    (should (eq (gptel-fsm-state fsm) 'RTRY)))
  ;; Dead buffer.
  (let ((fsm (gptel-make-fsm))
        (buf (get-buffer-create "*bf-buffer*")))
    (setf (gptel-fsm-state fsm) 'RTRY)
    (setf (gptel-fsm-info fsm) (list :buffer buf))
    (kill-buffer buf)
    (gptel-backoff--fire fsm)
    (should (eq (gptel-fsm-state fsm) 'RTRY)))
  ;; Live: re-enters WAIT.
  (let ((fsm (gptel-make-fsm))
        (buf (current-buffer)))
    (gptel-backoff--install fsm)
    (setf (gptel-fsm-state fsm) 'RTRY)
    (setf (gptel-fsm-info fsm)
          (list :buffer buf :backend gptel-backoff-tests--backend
                :data '(:model "test-model" :messages []) :position (point-marker)))
    ;; Suppress the network dispatch the WAIT handler would trigger.
    (cl-letf (((symbol-function 'gptel--handle-wait)
               (lambda (_fsm))))
      (gptel-backoff--fire fsm))
    (should (eq (gptel-fsm-state fsm) 'WAIT))))

(ert-deftest gptel-backoff-handle-retry-registers-and-schedules ()
  "handle-retry increments attempts, registers in the alist, and sets a timer."
  (let* ((fsm (gptel-make-fsm))
         (buf (current-buffer)))
    (gptel-backoff--install fsm)
    (setf (gptel-fsm-info fsm)
          (list :backend gptel-backoff-tests--backend
                :buffer buf
                :http-status "429" :http-headers nil :error nil
                :stream nil))
    ;; Avoid actually scheduling a real timer; fake run-at-time and let
    ;; cleanup-parked's cancel-timer accept our fake value.
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat _fun &rest _args)
                 (list 'timer 'fake)))
              ((symbol-function 'cancel-timer)
               (lambda (_timer) t)))
      (gptel-backoff--handle-retry fsm)
      (should (= (plist-get (gptel-fsm-info fsm) :backoff-attempts) 1))
      ;; Parked entry (nil key) present in gptel--request-alist for abort.
      (should (cl-find-if (lambda (e) (and (null (car e))
                                           (eq (cadr e) fsm)))
                          gptel--request-alist))
      ;; The timer slot was populated.
      (should (consp (plist-get (gptel-fsm-info fsm) :backoff-timer)))
      ;; Cleanup while the fakes are active so the fake timer is accepted.
      (gptel-backoff--cleanup-parked fsm)
      (should (not (cl-find-if (lambda (e) (eq (cadr e) fsm))
                               gptel--request-alist))))))

(ert-deftest gptel-backoff-cleanup-parked-cancels-timer ()
  "cleanup-parked cancels the retry timer and marks the request cancelled."
  (let* ((fsm (gptel-make-fsm))
         (cancelled nil))
    (setf (gptel-fsm-info fsm)
          (list :buffer (current-buffer)
                :backoff-timer (list 'timer "fake")))
    (cl-letf (((symbol-function 'cancel-timer) (lambda (timer)
                                                 (setq cancelled (eq timer (plist-get
                                                                           (gptel-fsm-info fsm)
                                                                           :backoff-timer))))))
      (gptel-backoff--cleanup-parked fsm)
      (should cancelled)
      (should (plist-get (gptel-fsm-info fsm) :cancelled)))))

(provide 'gptel-backoff-tests)
;;; gptel-backoff-tests.el ends here
