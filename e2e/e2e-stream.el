;; Streaming curl transport: mid-stream retryable error (Anthropic
;; `overloaded_error` on an HTTP 200 SSE stream) must be retried after
;; backoff, with partial output truncated so the retried stream is not
;; duplicated.
;;
;; Run by the e2e driver against `gptel-e2e-server.py 8901`:
;;   request #1 -> partial deltas then event: error (overloaded_error)
;;   request #2+ -> full clean stream "ANTH FINAL OK"
(require 'gptel)
(require 'gptel-anthropic)
(require 'gptel-backoff)

(gptel-make-anthropic "STREAMFAKE" :key "sk-test"
                      :models '(claude-test) :stream t
                      :host "127.0.0.1" :protocol "http" :endpoint "/v1/messages")
(setf (gptel-backend-url (gptel-get-backend "STREAMFAKE"))
      "http://127.0.0.1:8901/v1/messages")

(let* ((gptel-use-curl t)
       (gptel-backoff-base-delay 0.1)
       (gptel-backoff-jitter-factor 0.0)
       (gptel-backoff-max-retries 2)
       (done nil)
       (final nil)
       (buf (get-buffer-create "*e2e-stream*"))
       (fsm (gptel-make-fsm)))
  (gptel-backoff--install fsm)
  (let* ((pos (set-marker (make-marker) (point-min) buf))
         (cb (lambda (response info)
               (cond
                ((stringp response)
                 (gptel-curl--stream-insert-response response info))
                ((eq response t)
                 (setq done t))
                ((and (consp response) (eq (car response) 'reasoning))
                 (gptel-curl--stream-insert-response response info))
                ((and (consp response) (memq (car response) '(tool-call tool-result)))
                 (gptel-curl--stream-insert-response response info))))))
    (setf (gptel-fsm-info fsm)
          (list :backend (gptel-get-backend "STREAMFAKE")
                :buffer buf
                :position pos
                :data '(:model "claude-test"
                        :stream t
                        :max_tokens 4096
                        :messages [(:role "user" :content "hi")])
                :stream t
                :callback cb)))
  (with-current-buffer buf
    (insert "user: hi\n"))
  (gptel--fsm-transition fsm)            ; INIT -> WAIT (dispatch)
  (let ((pump 0))
    (while (and (< pump 500) (not done))
      (sit-for 0.05)
      (setq pump (1+ pump))))
  (with-current-buffer buf
    (setq final (buffer-substring-no-properties (point-min) (point-max))))
  (let ((info (gptel-fsm-info fsm)))
    (princ (format "E2E-STREAM: done=%S state=%S attempts=%S installed=%S err=%S hstatus=%S history=%S\n"
                   done (gptel-fsm-state fsm)
                   (plist-get info :backoff-attempts)
                   (gptel-backoff--installed-p fsm)
                   (plist-get info :error)
                   (plist-get info :http-status)
                   (plist-get info :history))))
  (princ (format "E2E-STREAM-CONTENT: %S\n" final))
  (let ((count 0) (from 0))
    (while (setq from (string-search "ANTH FINAL OK" final from))
      (setq count (1+ count) from (1+ from)))
    (princ (format "E2E-STREAM-CHECKS: has-final=%S no-partial=%S final-once=%S\n"
                   (and (string-search "ANTH FINAL OK" final) t)
                   (not (string-search "PARTIAL DIRTY" final))
                   (= count 1))))
  (kill-buffer buf))
