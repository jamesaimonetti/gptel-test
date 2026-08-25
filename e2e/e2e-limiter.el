;;; e2e-limiter.el --- Concurrency limiter e2e tests  -*- lexical-binding: t; -*-

;; Concurrency limiter e2e (curl non-stream) against
;; `gptel-e2e-server.py 8902` (slow 0.4s responses).
;;
;; Phase 1 (limit 1): f1 dispatches and holds the slot (active=1, f1=WAIT),
;; f2 parks in QUEUE; when f1 finishes, the pump resumes f2, which then
;; completes.  Both end in DONE, semaphore back to 0/0.
;;
;; Phase 2 (fresh backend, limit 1): queue f2 behind f1, abort f2 while
;; parked, verify it is removed from the semaphore queue and never resumed
;; (f2 stays ABRT after f1 finishes).
;;
;; NOTE: 8902 always returns 200, so no retries interfere.
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-backoff)

(defun e2e-lim--make-backend (name)
  (let ((b (gptel-make-openai name :key "sk-test" :models '(gpt-4o)
                              :stream nil :host "127.0.0.1"
                              :protocol "http"
                              :endpoint "/v1/chat/completions")))
    (setf (gptel-backend-url b) "http://127.0.0.1:8902/v1/chat/completions")
    b))

;; Returns (FSM . DONE-COUNTER); DONE-COUNTER is a cons cell whose car is
;; incremented each time this FSM's callback fires.
(defun e2e-lim--fsm (backend buf)
  "Return (FSM . COUNTER) for a limiter e2e request.

COUNTER is a cons cell whose car is incremented by the callback, so the
driver can wait for exactly one successful callback per request."
  (let* ((fsm (gptel-make-fsm))
         (n (list 0)))
    (gptel-backoff--install fsm)
    (setf (gptel-fsm-info fsm)
          (list :backend backend
                :buffer buf
                :stream nil
                :position (set-marker (make-marker) (point-min) buf)
                :data '(:model "gpt-4o" :messages [(:role "user" :content "hi")])
                :callback (lambda (_r _i) (cl-incf (car n)))))
    (cons fsm n)))

(defun e2e-lim--wait (n pair delay &optional timeout)
  (let ((pump 0))
    (while (and (< pump (or timeout 200))
                (< (car (cdr pair)) n))
      (sit-for delay)
      (setq pump (1+ pump)))))

;; --- Phase 0: sanity-check that callbacks fire at all (no limiter, no
;; concurrency): a single request must reach its callback.
(let* ((backend0 (e2e-lim--make-backend "LIM-SANITY"))
       (b0 (get-buffer-create "*lim-sanity*"))
       (p0 (e2e-lim--fsm backend0 b0)))
  (let ((gptel-use-curl t)
        (gptel-backoff-default-concurrency nil))
    (gptel--fsm-transition (car p0))
    (e2e-lim--wait 1 p0 0.05 200)
    (princ (format "E2E-LIM-SANITY: calls=%S state=%S\n"
                   (car (cdr p0)) (gptel-fsm-state (car p0)))))
  (kill-buffer b0))

;; --- Phase 1: queue + resume ---
(let* ((gptel-use-curl t)
       (gptel-backoff-default-concurrency 1)
       (gptel-backoff-base-delay 0.05)
       (gptel-backoff-jitter-factor 0.0)
       (gptel-backoff-max-retries 1)
       (backend (e2e-lim--make-backend "LIM-E2E"))
       (b1 (get-buffer-create "*lim-a*"))
       (b2 (get-buffer-create "*lim-b*"))
       (p1 (e2e-lim--fsm backend b1))
       (p2 (e2e-lim--fsm backend b2))
       (f1 (car p1)) (f2 (car p2))
       (sem (gptel-backoff--semaphore backend)))
  ;; f1 dispatches immediately and holds the slot for ~0.4s (server is
  ;; slow).  Dispatch f2 while f1 is still in flight so it must queue.
  (gptel--fsm-transition f1)
  (sit-for 0.15)
  (gptel--fsm-transition f2)
  (sit-for 0.05)
  (princ (format "E2E-LIM-MID: f1=%S f2=%S active=%S queued=%S queuedflag=%S\n"
                 (gptel-fsm-state f1) (gptel-fsm-state f2)
                 (nth 0 sem) (length (nth 1 sem))
                 (plist-get (gptel-fsm-info f2) :queued)))
  ;; Wait for both callbacks.
  (e2e-lim--wait 1 p1 0.05 200)
  (e2e-lim--wait 1 p2 0.05 200)
  (princ (format "E2E-LIM-DONE: f1=%S f2=%S active=%S queued=%S calls1=%S calls2=%S\n"
                 (gptel-fsm-state f1) (gptel-fsm-state f2)
                 (nth 0 sem) (length (nth 1 sem))
                 (car (cdr p1)) (car (cdr p2)))))
(kill-buffer "*lim-a*") (kill-buffer "*lim-b*")

;; --- Phase 2: abort a queued request ---
(let* ((gptel-use-curl t)
       (gptel-backoff-default-concurrency 1)
       (gptel-backoff-base-delay 0.05)
       (gptel-backoff-jitter-factor 0.0)
       (gptel-backoff-max-retries 1)
       (backend (e2e-lim--make-backend "LIM-E2E-ABORT"))
       (b1 (get-buffer-create "*lim-abort-a*"))
       (b2 (get-buffer-create "*lim-abort-b*"))
       (p1 (e2e-lim--fsm backend b1))
       (p2 (e2e-lim--fsm backend b2))
       (f1 (car p1)) (f2 (car p2))
       (sem (gptel-backoff--semaphore backend)))
  (gptel--fsm-transition f1)
  (e2e-lim--wait 0 p1 0.05 60)         ;give it time to dispatch
  (gptel--fsm-transition f2)           ;queued
  (sit-for 0.1)
  (princ (format "E2E-LIM-ABORT-MID: f1=%S f2=%S queuedflag=%S sem-queue=%S\n"
                 (gptel-fsm-state f1) (gptel-fsm-state f2)
                 (plist-get (gptel-fsm-info f2) :queued)
                 (length (nth 1 sem))))
  (gptel-abort b2)                     ;abort while parked
  (princ (format "E2E-LIM-ABORT: f2=%S queuedflag=%S sem-queue=%S\n"
                 (gptel-fsm-state f2)
                 (plist-get (gptel-fsm-info f2) :queued)
                 (length (nth 1 sem))))
  ;; f2 must not be resumed later: after f1 finishes, f2 stays ABRT.
  (e2e-lim--wait 1 p1 0.05 200)
  (princ (format "E2E-LIM-ABORT-DONE: f1=%S f2=%S sem-queue=%S calls1=%S calls2=%S\n"
                 (gptel-fsm-state f1) (gptel-fsm-state f2)
                 (length (nth 1 sem))
                 (car (cdr p1)) (car (cdr p2)))))
(kill-buffer "*lim-abort-a*") (kill-buffer "*lim-abort-b*")
