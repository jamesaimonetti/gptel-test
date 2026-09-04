;;; gptel-anthropic-cache-test.el --- Tests for Anthropic prompt caching  -*- lexical-binding: t; -*-

;; Unit tests for the Anthropic prompt-cache behavior of this branch:
;;
;; - A. breakpoint placement: the message cache_control breakpoint goes
;;   on the SECOND-TO-LAST message when there are ≥2 messages, so the
;;   incoming prompt (the last message, which changes every request) is
;;   never cached; a single message (fresh conversation) falls back to
;;   caching itself.
;; - B. `gptel-anthropic-cache-ttl': emitted on every breakpoint
;;   (system, tools, messages) via `gptel--anthropic-cache-entry', and
;;   uniform per request (Anthropic requires non-increasing TTLs).
;; - `gptel-cache' nil / `system' / `tool' values leave messages
;;   uncached.
;; - D: the per-turn cache usage debug log line.

(require 'ert)
(require 'gptel)
(require 'gptel-anthropic)

(defconst gptel-anthropic-cache-test-model
  (gptel--make-anthropic :name "Anthropic" :models '(claude-test-cache))
  "Anthropic backend whose model advertises the cache capability.")

;; Set the cache capability on the test model (bare symbol specs get no
;; capabilities from `gptel--process-models').
(setplist 'claude-test-cache '(:capabilities (cache) :mime-types nil))

(defmacro gptel-anthropic-cache-test--with (&rest body)
  "Run BODY with a cache-capable Anthropic model selected."
  (declare (indent 0))
  `(let ((gptel-model 'claude-test-cache))
     ,@body))

(defun gptel-anthropic-cache-test--cache-entry (message)
  "Return the cache_control plist on MESSAGE, or nil if none.

MESSAGE is a message plist as produced by `gptel--parse-list' /
`gptel--parse-buffer'.  The breakpoint, when present, lives on the
first cacheable content block: block 0 for ordinary messages, or the
text block after a leading thinking block; string content means no
breakpoint (the helper wraps strings when it adds one)."
  (let ((content (plist-get message :content)))
    (when (vectorp content)
      (cl-loop for i below (length content)
               for block = (aref content i)
               thereis (plist-get block :cache_control)))))

;;;; A. Breakpoint placement -- gptel--parse-list

(ert-deftest gptel-anthropic-cache-test-list-multi-msg-breakpoint ()
  "With ≥2 messages the breakpoint is on the second-to-last message.

The last message is the incoming (changing) prompt and must stay
uncached; the second-to-last is the stable prefix shared with the
previous request."
  (gptel-anthropic-cache-test--with
    (let* ((gptel-cache '(message))
           (prompts (gptel--parse-list
                     gptel-anthropic-cache-test-model
                     '("user one" "assistant one" "user two"))))
      (should (= (length prompts) 3))
      ;; Second-to-last (the assistant reply) carries the breakpoint.
      (should (gptel-anthropic-cache-test--cache-entry (nth 1 prompts)))
      (should (equal (gptel-anthropic-cache-test--cache-entry (nth 1 prompts))
                     '(:type "ephemeral" :ttl "5m")))
      ;; The incoming prompt and the earlier user message are uncached.
      (should-not (gptel-anthropic-cache-test--cache-entry (nth 0 prompts)))
      (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 prompts))))))

(ert-deftest gptel-anthropic-cache-test-list-single-message-fallback ()
  "A single message (fresh conversation) falls back to caching itself."
  (gptel-anthropic-cache-test--with
    (let* ((gptel-cache '(message))
           (prompts (gptel--parse-list
                     gptel-anthropic-cache-test-model '("only"))))
      (should (= (length prompts) 1))
      (should (gptel-anthropic-cache-test--cache-entry (car prompts))))))

(ert-deftest gptel-anthropic-cache-test-list-cache-disabled ()
  "`gptel-cache' nil or lacking `message' leaves messages uncached."
  (gptel-anthropic-cache-test--with
    (dolist (cache (list nil '(system) '(tool) '()))
      (let* ((gptel-cache cache)
             (prompts (gptel--parse-list
                       gptel-anthropic-cache-test-model
                       '("a" "b" "c"))))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 1 prompts)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 prompts)))))))

(ert-deftest gptel-anthropic-cache-test-list-cache-t ()
  "`gptel-cache' t caches the message breakpoint too."
  (gptel-anthropic-cache-test--with
    (let* ((gptel-cache t)
           (prompts (gptel--parse-list
                     gptel-anthropic-cache-test-model
                     '("a" "b" "c"))))
      (should (gptel-anthropic-cache-test--cache-entry (nth 1 prompts))))))

(ert-deftest gptel-anthropic-cache-test-content-normalized-to-vector ()
  "Message caching pins every message to array content.

An incoming message carries no breakpoint and is sent as-is this turn;
next turn it is part of the cached prefix.  If its JSON representation
flipped between a string and an array depending on where the
breakpoint sat, the prompt hash would change and the cache would miss.
So with message caching on, ALL messages use a content vector."
  (gptel-anthropic-cache-test--with
    (let* ((gptel-cache '(message))
           (prompts (gptel--parse-list
                     gptel-anthropic-cache-test-model
                     '("a" "b" "c"))))
      (dolist (msg prompts)
        (should (vectorp (plist-get msg :content)))
        (should (plist-get (aref (plist-get msg :content) 0) :text))))))

(ert-deftest gptel-anthropic-cache-test-list-model-without-cache ()
  "A model without the cache capability is never decorated."
  (let ((gptel-model 'claude-no-cache))
    (gptel-make-anthropic "ANTH-NOCACHE" :models '(claude-no-cache))
    (let* ((gptel-cache t)
           (prompts (gptel--parse-list
                     (gptel-get-backend "ANTH-NOCACHE")
                     '("a" "b" "c"))))
      (should-not (gptel-anthropic-cache-test--cache-entry (nth 1 prompts))))))

;;;; A. Breakpoint placement -- gptel--parse-buffer

(ert-deftest gptel-anthropic-cache-test-buffer-multi-msg-breakpoint ()
  "Buffer parse: breakpoint on the previous assistant turn.

Point is at the end of the buffer, so the last message is the new
user prompt; it must be uncached, and the assistant reply before it
carries the breakpoint."
  (gptel-anthropic-cache-test--with
    (let ((gptel-cache '(message)))
      (with-temp-buffer
        (insert "user one")
        (insert gptel-response-separator
                (propertize "assistant one" 'gptel 'response)
                gptel-response-separator)
        (insert "user two")
        (goto-char (point-max))
        (let ((prompts (gptel--parse-buffer gptel-anthropic-cache-test-model)))
          (should (= (length prompts) 3))
          (should (gptel-anthropic-cache-test--cache-entry (nth 1 prompts)))
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 0 prompts)))
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 prompts))))))))

(ert-deftest gptel-anthropic-cache-test-buffer-tool-turn-breakpoint ()
  "Agentic turn: the shared tool-result prefix gets the breakpoint,
and the incoming prompt stays uncached.

Buffer layout mirrors `gptel--display-tool-results': assistant
tool_use, user tool_result, assistant reply, then the new prompt."
  (gptel-anthropic-cache-test--with
    (let ((gptel-cache '(message)))
      (with-temp-buffer
        (insert "user one")
        (insert gptel-response-separator
                (propertize
                 (concat "(:name \"probe\" :args nil)\n\nresult one")
                 'gptel '(tool . "toolu_abc"))
                gptel-response-separator)
        (insert gptel-response-separator
                (propertize "assistant reply" 'gptel 'response)
                gptel-response-separator)
        (insert "user two")
        (goto-char (point-max))
        (let ((prompts (gptel--parse-buffer gptel-anthropic-cache-test-model)))
          ;; user, tool_use, tool_result, assistant, user
          (should (= (length prompts) 5))
          ;; The assistant reply (second-to-last) is the breakpoint.
          (should (gptel-anthropic-cache-test--cache-entry (nth 3 prompts)))
          ;; Tool blocks parse to vectors but are not the breakpoint.
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 0 prompts)))
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 1 prompts)))
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 prompts)))
          ;; The incoming prompt is uncached.
          (should-not (gptel-anthropic-cache-test--cache-entry (nth 4 prompts))))))))

(ert-deftest gptel-anthropic-cache-test-buffer-single-message-fallback ()
  "Buffer parse with one message falls back to caching it."
  (gptel-anthropic-cache-test--with
    (let ((gptel-cache '(message)))
      (with-temp-buffer
        (insert "user only")
        (goto-char (point-max))
        (let ((prompts (gptel--parse-buffer gptel-anthropic-cache-test-model)))
          (should (= (length prompts) 1))
          (should (gptel-anthropic-cache-test--cache-entry (car prompts))))))))

;;;; C. Re-stamp on reused payloads (agentic tool loops)

(ert-deftest gptel-anthropic-cache-test-restamp-moves-breakpoint ()
  "Re-stamping a reused payload moves the breakpoint to the new
second-to-last message after history grows between tool rounds.

Simulates an agentic loop: the FSM reuses the same DATA and appends
assistant and tool_result messages after the initial parse, so the
stale breakpoint from the single-message fallback would pin an
outdated prefix.  `gptel--cache-messages' must clear it and re-stamp
the last stable block before the incoming prompt."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-cache '(message))
           (data (list :messages
                       (vconcat
                        (list (list :role "user"
                                    :content (vector
                                              (list :type "text"
                                                    :text "user one"))))))))
      ;; Initial parse stamps the single message (fresh-conversation
      ;; fallback).
      (gptel--anthropic-cache-messages (append (plist-get data :messages) nil))
      (should (gptel-anthropic-cache-test--cache-entry
               (aref (plist-get data :messages) 0)))
      ;; Agentic growth: assistant reply + tool_result, another partial
      ;; assistant turn, then the incoming prompt.
      (setf (plist-get data :messages)
            (vconcat
             (plist-get data :messages)
             (list (list :role "assistant"
                         :content (vector (list :type "text"
                                                :text "assistant one")))
                   (list :role "user"
                         :content (vector (list :type "tool_result"
                                                :tool_use_id "t1"
                                                :content "tool result one")))
                   (list :role "assistant"
                         :content (vector (list :type "tool_use" :id "t2"
                                                :name "probe" :input nil)))
                   (list :role "user"
                         :content (vector (list :type "tool_result"
                                                :tool_use_id "t2"
                                                :content "tool result two")))
                   (list :role "user"
                         :content (vector (list :type "text"
                                                :text "user two"))))))
      (gptel--cache-messages backend data)
      (let ((messages (append (plist-get data :messages) nil)))
        ;; user one, assistant one, tool_result, assistant(tool_use),
        ;; tool_result, user two = 6 messages.
        (should (= (length messages) 6))
        ;; Breakpoint on the second-to-last (the tool_result before the
        ;; incoming prompt); stale breakpoint on the first message is gone.
        (should (gptel-anthropic-cache-test--cache-entry (nth 4 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 0 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 1 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 3 messages)))
        ;; Incoming prompt never cached.
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 5 messages)))
        ;; Every message is pinned to array content for a stable hash.
        (dolist (msg messages)
          (should (vectorp (plist-get msg :content))))))))

(ert-deftest gptel-anthropic-cache-test-restamp-normalizes-string-content ()
  "The re-stamp pins string content to arrays, like the initial parse.

Messages appended between tool rounds can be plain strings in the same
position where the initial parse would have normalized them; a flip
between string and array representation on the wire would change the
prompt-cache prefix hash and miss."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-cache '(message))
           (data (list :messages
                       (vconcat
                        (list (list :role "user" :content "user one")
                              (list :role "assistant" :content "assistant one")
                              (list :role "user" :content "user two"))))))
      (gptel--cache-messages backend data)
      (let ((messages (append (plist-get data :messages) nil)))
        (dolist (msg messages)
          (should (vectorp (plist-get msg :content))))
        (should (gptel-anthropic-cache-test--cache-entry (nth 1 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 0 messages)))
        (should-not (gptel-anthropic-cache-test--cache-entry (nth 2 messages)))))))

(ert-deftest gptel-anthropic-cache-test-restamp-respects-history-only ()
  "Re-stamping leaves system and tools breakpoints untouched; it only
touches messages.  (System/tools are set in `gptel--request-data' and
never change between tool rounds.)"
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (data (list :messages (vconcat (list (list :role "user"
                                                      :content "hi")))
                       :system (vector (list :type "text" :text "sys"
                                             :cache_control (gptel--anthropic-cache-entry)))
                       :tools (vector (list :name "probe"
                                            :cache_control (gptel--anthropic-cache-entry))))))
      (gptel--cache-messages backend data)
      ;; System/tools breakpoints survive the re-stamp.
      (should (plist-get (aref (plist-get data :system) 0) :cache_control))
      (should (plist-get (aref (plist-get data :tools) 0) :cache_control)))))

(ert-deftest gptel-anthropic-cache-test-restamp-default-noop ()
  "The default `gptel--cache-messages' is a no-op for backends without
prompt caching."
  (let ((data (list :messages [(list :role "user" :content "hi")])))
    (should (eq (gptel--cache-messages 'some-other-backend data) data))
    (should (equal data (list :messages [(list :role "user" :content "hi")])))))

(ert-deftest gptel-anthropic-cache-test-clear-cache-control ()
  "`gptel--anthropic-clear-cache-control' removes breakpoints from every
content block of a message."
  (let ((msg (list :role "user"
                   :content (vector (list :type "text" :text "a"
                                          :cache_control (gptel--anthropic-cache-entry))
                                    (list :type "text" :text "b"
                                          :cache_control (gptel--anthropic-cache-entry))))))
    (gptel--anthropic-clear-cache-control msg)
    (let ((content (plist-get msg :content)))
      (should-not (plist-member (aref content 0) :cache_control))
      (should-not (plist-member (aref content 1) :cache_control))
      ;; Other keys are preserved.
      (should (equal (plist-get (aref content 0) :text) "a")))))

(ert-deftest gptel-anthropic-cache-test-thinking-first-message-breakpoint ()
  "A breakpoint message whose first block is a thinking block gets the
breakpoint on the following text block, not the thinking block.

Anthropic rejects cache_control on thinking blocks
(`invalid_request_error': \"...thinking.cache_control: Extra inputs are
not permitted\"), so the stamp must skip block 0 and mark the first
cacheable block instead."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-cache '(message))
           (data (list :messages
                       (vconcat
                        (list (list :role "user"
                                    :content (vector
                                              (list :type "text" :text "user one")))
                              ;; Extended-thinking assistant turn: block 0
                              ;; is a thinking block (as appended by the
                              ;; stream parser), then text + tool_use.
                              (list :role "assistant"
                                    :content (vector
                                              (list :type "thinking"
                                                    :thinking "hmm..."
                                                    :signature "sig123")
                                              (list :type "text" :text "I'll call a tool")
                                              (list :type "tool_use"
                                                    :id "t1" :name "probe" :input nil)))
                              (list :role "user"
                                    :content (vector
                                              (list :type "tool_result"
                                                    :tool_use_id "t1"
                                                    :content "result"))))))))
      ;; The assistant turn (second-to-last, index 1) carries the
      ;; breakpoint.
      (gptel--cache-messages backend data)
      (let* ((messages (append (plist-get data :messages) nil))
             (content (plist-get (nth 1 messages) :content)))
        (should (equal (length messages) 3))
        ;; The thinking block itself must NOT carry cache_control...
        (should-not (plist-member (aref content 0) :cache_control))
        ;; ...and the breakpoint lands on the first cacheable block
        ;; (the text block that follows the thinking block).
        (should (equal (plist-get (aref content 1) :cache_control)
                       '(:type "ephemeral" :ttl "5m")))
        ;; The wire form would be exactly
        ;;   messages[1].content = [thinking, text+cache_control, tool_use]
        (should (equal (gptel--json-encode (nth 1 messages))
                       (concat
                        "{\"role\":\"assistant\",\"content\":["
                        "{\"type\":\"thinking\",\"thinking\":\"hmm...\",\"signature\":\"sig123\"},"
                        "{\"type\":\"text\",\"text\":\"I'll call a tool\","
                        "\"cache_control\":{\"type\":\"ephemeral\",\"ttl\":\"5m\"}},"
                        "{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"probe\",\"input\":{}}"
                        "]}")))))))

;;;; B. TTL

(ert-deftest gptel-anthropic-cache-test-ttl-default-5m ()
  "The default TTL is 5m and appears on every breakpoint."
  (should (equal gptel-anthropic-cache-ttl "5m"))
  (should (equal (gptel--anthropic-cache-entry)
                 '(:type "ephemeral" :ttl "5m"))))

(ert-deftest gptel-anthropic-cache-test-ttl-1h-on-messages ()
  "Setting `gptel-anthropic-cache-ttl' to \"1h\" flows to messages."
  (gptel-anthropic-cache-test--with
    (let ((gptel-cache '(message))
          (gptel-anthropic-cache-ttl "1h"))
      (let ((prompts (gptel--parse-list
                      gptel-anthropic-cache-test-model
                      '("a" "b" "c"))))
        (should (equal (gptel-anthropic-cache-test--cache-entry (nth 1 prompts))
                       '(:type "ephemeral" :ttl "1h")))))))

(ert-deftest gptel-anthropic-cache-test-ttl-uniform-across-request-data ()
  "The chosen TTL is applied uniformly to tools and system.

Uniform TTLs avoid Anthropic's non-increasing-TTL ordering rule."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-backend backend)
           (gptel-cache t)
           (gptel-anthropic-cache-ttl "1h")
           (gptel-system-prompt "sys"))
      (gptel-make-tool :name "probe" :description "d" :args nil
                       :function (lambda ()))
      (setq gptel-tools (list (gptel-get-tool "probe")))
      (let ((data (gptel--request-data
                   backend
                   (list (list :role "user"
                               :content [(list :type "text" :text "hi")])))))
        ;; System block.
        (should (equal (plist-get (aref (plist-get data :system) 0)
                                  :cache_control)
                       '(:type "ephemeral" :ttl "1h")))
        ;; Tools block.
        (should (equal (plist-get (aref (plist-get data :tools) 0)
                                  :cache_control)
                       '(:type "ephemeral" :ttl "1h")))))))

(ert-deftest gptel-anthropic-cache-test-tools-remain-5m-by-default ()
  "Without `message` in gptel-cache, tools/system still follow the TTL."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-backend backend)
           (gptel-cache t)
           (gptel-anthropic-cache-ttl "5m")
           (gptel-system-prompt "sys"))
      (gptel-make-tool :name "probe2" :description "d" :args nil
                       :function (lambda ()))
      (setq gptel-tools (list (gptel-get-tool "probe2")))
      (let ((data (gptel--request-data
                   backend
                   (list (list :role "user"
                               :content [(list :type "text" :text "hi")])))))
        (should (equal (plist-get (aref (plist-get data :tools) 0)
                                  :cache_control)
                       '(:type "ephemeral" :ttl "5m")))))))

(ert-deftest gptel-anthropic-cache-test-system-uncached-when-cache-nil ()
  "`gptel-cache' nil means no system breakpoint either."
  (gptel-anthropic-cache-test--with
    (let* ((backend gptel-anthropic-cache-test-model)
           (gptel-backend backend)
           (gptel-cache nil)
           (gptel-system-prompt "sys"))
      (let ((data (gptel--request-data
                   backend
                   (list (list :role "user"
                               :content [(list :type "text" :text "hi")])))))
        (should-not (plist-get (aref (plist-get data :system) 0)
                               :cache_control))))))

;;;; D. Debug cache-health log line

(ert-deftest gptel-anthropic-cache-test-debug-log-line ()
  "At `debug' log level the token update logs a per-turn cache summary."
  (gptel-anthropic-cache-test--with
    (let* ((gptel-log-level 'debug)
           (buffer (get-buffer-create "*gptel-anthropic-cache-log*"))
           (gptel--log-buffer-name buffer)   ;redirect gptel--log target
           (info (list :probe nil)))
      (unwind-protect
          (progn
            (gptel--anthropic-update-tokens
             '(:input_tokens 100 :output_tokens 7
               :cache_creation_input_tokens 1000
               :cache_read_input_tokens 5000)
             info)
            (with-current-buffer buffer
              (let ((text (buffer-string)))
                (should (string-match-p "anthropic cache usage" text))
                (should (string-match-p "cache_read_input_tokens" text))
                (should (string-match-p "5000" text))
                (should (string-match-p "1000" text)))))
        (kill-buffer buffer))
      ;; Token accounting is unaffected by logging.
      (let ((tokens (plist-get info :tokens)))
        (should (equal (plist-get tokens :cached) 5000))
        (should (equal (plist-get tokens :cache) 1000))
        (should (equal (plist-get tokens :input) 1100))))))

(ert-deftest gptel-anthropic-cache-test-debug-log-silent-at-info ()
  "At `info' (or nil) level, no cache summary is logged."
  (gptel-anthropic-cache-test--with
    (dolist (level (list nil 'info))
      (let* ((gptel-log-level level)
             (buffer (get-buffer-create (format "*gptel-anthropic-cache-log-%S*" level)))
             (gptel--log-buffer-name buffer)
             (info (list :probe nil)))
        (gptel--anthropic-update-tokens
         '(:input_tokens 1 :output_tokens 1)
         info)
        (with-current-buffer buffer
          (should (string-empty-p (buffer-string))))
        (kill-buffer buffer)))))

(provide 'gptel-anthropic-cache-test)
;;; gptel-anthropic-cache-test.el ends here
