;; url-retrieve transport: fake 429 backend (with Retry-After: 1) that
;; returns 200 after two 429s.  Requires `gptel-e2e-server.py 8899'.
;;
;; Verifies: retried automatically (3 server requests), final response
;; delivered once, no spurious error callback, FSM ends in DONE,
;; :backoff-attempts incremented, :http-headers captured.
(require 'gptel)
(require 'gptel-openai)
(require 'gptel-backoff)

(gptel-make-openai "URLE2E" :key "sk-test" :models '(gpt-4o) :stream nil
                   :host "127.0.0.1" :protocol "http" :endpoint "/v1/chat/completions")
(setf (gptel-backend-url (gptel-get-backend "URLE2E"))
      "http://127.0.0.1:8899/v1/chat/completions")

(let* ((gptel-use-curl nil)             ; force url-retrieve
       (gptel-backoff-base-delay 0.1)
       (gptel-backoff-jitter-factor 0.0)
       (gptel-backoff-max-retries 2)
       (responses nil)
       (buf (get-buffer-create "*e2e-url*"))
       (fsm (gptel-make-fsm)))
  (gptel-backoff--install fsm)
  (let* ((pos (set-marker (make-marker) (point-min) buf)))
    (setf (gptel-fsm-info fsm)
          (nconc (list :backend (gptel-get-backend "URLE2E")
                       :buffer buf
                       :position pos
                       :data '(:model "gpt-4o"
                               :messages [(:role "user" :content "hi")])
                       :callback
                       (lambda (response _i)
                         (push response responses)))
                 (gptel-fsm-info fsm))))
  (with-current-buffer buf (insert "hi\n"))
  (gptel--fsm-transition fsm)            ; INIT -> WAIT
  (let ((pump 0))
    (while (and (< pump 600) (null responses))
      (sit-for 0.05)
      (setq pump (1+ pump))))
  (let* ((info (gptel-fsm-info fsm))
         (attempts (plist-get info :backoff-attempts))
         (headers (plist-get info :http-headers)))
    (princ (format "E2E-URL: state=%S responses=%S attempts=%S retry-after=%S\n"
                   (gptel-fsm-state fsm)
                   (mapcar (lambda (r) (if (stringp r) r (prin1-to-string r)))
                           responses)
                   attempts
                   (cdr (assoc "retry-after" headers))))
    (princ (format "E2E-URL-CHECKS: final-ok=%S no-error-call=%S attempts-ok=%S\n"
                   (equal responses '("URL FINAL OK"))
                   (not (member nil responses))
                   (and attempts (>= attempts 2))))))
