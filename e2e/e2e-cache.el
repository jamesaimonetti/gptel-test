;; e2e harness: Anthropic prompt-cache behavior against the fake
;; `gptel-e2e-server.py 8903' endpoint, which simulates Anthropic's
;; cache_read/cache_creation/input split from a cache table.
;;
;; Drives the real prompt path (gptel-request -> prompt buffer copy ->
;; gptel--parse-buffer -> gptel--request-data -> curl), then asserts:
;;   - turn 1 (single message, fresh conversation): the server writes the
;;     prefix (cache_creation > 0, cache_read == 0).
;;   - turn 2 (user one / assistant one / user two): the shared prefix is
;;     read from cache (cache_read > 0, cache_creation == 0), because the
;;     breakpoint is on the SECOND-TO-LAST message.  The server logs the
;;     exact cache_control placement as `layout: m1:5m,...`; `run-e2e.sh`
;;     asserts the incoming message (index 2) carries no breakpoint.
;;   - at gptel-log-level debug, gptel--anthropic-update-tokens emits the
;;     per-turn "anthropic cache usage" log line.
(require 'gptel)
(require 'gptel-anthropic)

(gptel-make-anthropic "CACHEFAKE" :key "sk-test"
                      :models '(claude-test) :stream nil
                      :host "127.0.0.1" :protocol "http" :endpoint "/v1/messages")
(setf (gptel-backend-url (gptel-get-backend "CACHEFAKE"))
      "http://127.0.0.1:8903/v1/messages")

;; Give the test model the cache capability (bare symbols get none).
(setplist 'claude-test '(:capabilities (cache) :mime-types nil))

(defun e2e-cache--send (buf)
  "Send a real gptel request for everything up to point in BUF.

Returns the INFO plist captured by the response callback."
  (with-current-buffer buf (goto-char (point-max)))
  (let ((done nil) (info-last nil))
    (gptel-request
     nil
     :buffer buf
     :position (with-current-buffer buf (copy-marker (point-max)))
     :callback (lambda (response i)
                 (setq info-last i
                       done (or (stringp response) (eq response t)))))
    (let ((pump 0))
      (while (and (< pump 1000) (not done))
        (sit-for 0.05)
        (setq pump (1+ pump))))
    (unless done (error "e2e-cache: request did not complete"))
    ;; Let the curl sentinel finish its async cleanup before the next turn.
    (let ((pump 0))
      (while (and (< pump 200)
                  (cl-some (lambda (e) (and (processp (car e)) (process-live-p (car e))))
                           gptel--request-alist))
        (sit-for 0.05)
        (setq pump (1+ pump))))
    info-last))

(let* ((gptel-use-curl t)
       (gptel-model 'claude-test)
       (gptel-cache t)
       (gptel-system-prompt "You are a helpful cache probe.")
       (gptel-stream nil)
       (gptel-log-level 'debug)
       (log-buf (get-buffer-create "*gptel-e2e-cache-log*"))
       (gptel--log-buffer-name log-buf)
       (buf (get-buffer-create "*e2e-cache*")))
  (unwind-protect
      (progn
        (with-current-buffer buf
          (erase-buffer)
          (setq-local gptel-backend (gptel-get-backend "CACHEFAKE")
                      gptel-model 'claude-test
                      gptel-cache t
                      gptel-system-prompt "You are a helpful cache probe."
                      gptel-stream nil)
          (insert "user one\n"))
        ;; Turn 1: single message => breakpoint on it (fresh conversation);
        ;; server writes the prefix.
        (let* ((info1 (e2e-cache--send buf))
               (tokens (plist-get info1 :tokens)))
          (princ (format "E2E-CACHE-1: input=%S cached=%S cache=%S\n"
                         (plist-get tokens :input)
                         (plist-get tokens :cached)
                         (plist-get tokens :cache))))
        ;; gptel inserts the assistant reply with the 'gptel property.
        (with-current-buffer buf
          (goto-char (point-max))
          (insert gptel-response-separator
                  (propertize "assistant one" 'gptel 'response)
                  gptel-response-separator)
          (insert "user two\n"))
        ;; Turn 2: [user one, assistant one, user two] -> breakpoint on
        ;; assistant one (index 1, second-to-last); shared prefix read
        ;; from cache.  The server's own log is the ground truth for
        ;; breakpoint placement (asserted by run-e2e.sh).
        (let* ((info2 (e2e-cache--send buf))
               (tokens (plist-get info2 :tokens)))
          (princ (format "E2E-CACHE-2: input=%S cached=%S cache=%S\n"
                         (plist-get tokens :input)
                         (plist-get tokens :cached)
                         (plist-get tokens :cache)))
          ;; Core claim: turn 2 reads the shared prefix (cached > 0).
          ;; Turn 1 wrote it (cache > 0, cached == 0).  The server's
          ;; `layout: m1:5m` log line proves the breakpoint is on the
          ;; second-to-last message.
          (princ (format "E2E-CACHE-CHECKS: second-read=%S first-wrote=%S\n"
                         (> (plist-get tokens :cached) 0)
                         (> (plist-get tokens :cache) 0))))
        (with-current-buffer log-buf
          (princ (format "E2E-CACHE-LOG: %s\n"
                         (if (string-match-p "anthropic cache usage" (buffer-string))
                             "cache-line=yes" "cache-line=no")))))
    (kill-buffer log-buf)
    (kill-buffer buf)))
