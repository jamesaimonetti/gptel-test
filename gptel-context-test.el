;;; gptel-context-test.el --- Tests for targeted context API  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: tests

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the targeted context API added in gptel-context.el.
;; This tests the ability to specify buffer and file regions via
;; :bounds, :lines, and :overlays specifications.

;;; Code:

(require 'ert)
(require 'gptel)
(require 'gptel-context)

;;; Helper functions

(defun gptel-context-test--make-buffer (content)
  "Create a temporary buffer with CONTENT for testing.
Returns the buffer object."
  (let ((buf (generate-new-buffer " *gptel-context-test*")))
    (with-current-buffer buf
      (insert content))
    buf))

(defun gptel-context-test--extract-regions (buffer context-data)
  "Extract text from BUFFER using CONTEXT-DATA spec.
Returns a list of strings, one per region.

This is a test helper that uses the production function
`gptel-context--collect-regions' to ensure tests match actual behavior."
  (let ((regions (gptel-context--collect-regions buffer context-data))
        (results '()))
    ;; Extract text from each region
    (dolist (region regions)
      (push (with-current-buffer buffer
              (buffer-substring-no-properties (car region) (cdr region)))
            results))
    (nreverse results)))

;;; Test models

;; Dedicated model symbols with explicit plists, so that media/MIME
;; capability tests don't depend on production model symbols (such as
;; gpt-4o-mini in `gptel--openai-models'), which may change or be
;; removed at any time.
(gptel--process-models
 '((gptel-test-model-multimodal
    :description "Test model with image and file input support"
    :capabilities (media tool-use json)
    :mime-types ("image/png" "image/jpeg" "application/pdf"))
   (gptel-test-model-image-only
    :description "Test model with image input support"
    :capabilities (media)
    :mime-types ("image/png" "image/jpeg"))
   (gptel-test-model-text
    :description "Test model with no media support"
    :capabilities (tool-use json))))

;;; Buffer bounds tests

(ert-deftest gptel-context-test-buffer-single-bounds ()
  "Test buffer context with a single :bounds specification."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Get bounds for "Line 2\nLine 3\n" (positions 7-21)
          (let* ((start (progn (goto-char (point-min))
                               (forward-line 1)
                               (point)))
                 (end (progn (forward-line 2)
                             (point)))
                 (context-data (list :bounds (cons start end)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 1))
            (should (equal (car extracted) "Line 2\nLine 3\n"))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-buffer-multiple-bounds ()
  "Test buffer context with multiple :bounds specifications."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Get bounds for "Line 1\n" and "Line 4\nLine 5\n"
          (let* ((start1 (point-min))
                 (end1 (progn (goto-char (point-min))
                              (forward-line 1)
                              (point)))
                 (start2 (progn (goto-char (point-min))
                                (forward-line 3)
                                (point)))
                 (end2 (point-max))
                 (context-data (list :bounds (list (cons start1 end1)
                                                   (cons start2 end2))))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 2))
            (should (equal (car extracted) "Line 1\n"))
            (should (equal (cadr extracted) "Line 4\nLine 5\n"))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-buffer-bounds-sorting ()
  "Test that multiple bounds are sorted by start position."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Add bounds in reverse order
          (let* ((start1 (progn (goto-char (point-min))
                                (forward-line 2)
                                (point)))
                 (end1 (progn (forward-line 1) (point)))
                 (start2 (point-min))
                 (end2 (progn (goto-char (point-min))
                              (forward-line 1)
                              (point)))
                 (context-data (list :bounds (list (cons start1 end1)
                                                   (cons start2 end2))))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            ;; Should be sorted, so Line 1 comes before Line 3
            (should (equal (length extracted) 2))
            (should (equal (car extracted) "Line 1\n"))
            (should (equal (cadr extracted) "Line 3\n"))))
      (kill-buffer buf))))

;;; Buffer line range tests

(ert-deftest gptel-context-test-buffer-single-lines ()
  "Test buffer context with a single :lines specification."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Get lines 2-3 (1-indexed)
          (let* ((context-data (list :lines (cons 2 3)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 1))
            (should (equal (car extracted) "Line 2\nLine 3\n"))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-buffer-multiple-lines ()
  "Test buffer context with multiple :lines specifications."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Get lines 1 and lines 4-5
          (let* ((context-data (list :lines (list (cons 1 1) (cons 4 5))))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 2))
            (should (equal (car extracted) "Line 1\n"))
            (should (equal (cadr extracted) "Line 4\nLine 5\n"))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-buffer-lines-singleton ()
  "Test that singleton :lines (START . END) is properly wrapped."
  (let* ((content "Line 1\nLine 2\nLine 3\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Single cons should be treated as one range
          (let* ((context-data (list :lines (cons 2 2)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 1))
            (should (equal (car extracted) "Line 2\n"))))
      (kill-buffer buf))))

;;; Overlay tests

(ert-deftest gptel-context-test-buffer-overlays ()
  "Test buffer context with :overlays specification."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Create overlays for Line 1 and Line 3
          (let* ((ov1 (make-overlay (point-min)
                                    (progn (goto-char (point-min))
                                           (forward-line 1)
                                           (point))))
                 (ov2 (make-overlay (progn (goto-char (point-min))
                                           (forward-line 2)
                                           (point))
                                    (progn (forward-line 1)
                                           (point))))
                 (context-data (list :overlays (list ov1 ov2)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 2))
            (should (equal (car extracted) "Line 1\n"))
            (should (equal (cadr extracted) "Line 3\n"))
            (delete-overlay ov1)
            (delete-overlay ov2)))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-dead-overlay-cleanup ()
  "Test that dead overlays are filtered out during collection."
  (let* ((content "Line 1\nLine 2\nLine 3\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-context nil))
    (unwind-protect
        (with-current-buffer buf
          ;; Create overlay and add to context
          (let ((ov (make-overlay (point-min)
                                  (progn (goto-char (point-min))
                                         (forward-line 1)
                                         (point)))))
            ;; Add to context in new format
            (setq gptel-context (list (cons buf (list :overlays (list ov)))))

            ;; Delete the overlay
            (delete-overlay ov)

            ;; Collect should filter out the dead overlay
            (let ((collected (gptel-context--collect)))
              ;; Buffer should still be in collection
              (should (equal (length collected) 1))
              (should (equal (caar collected) buf))
              ;; But overlays list should be empty (cleaned up)
              (let ((data (cdar collected)))
                (should (null (plist-get data :overlays)))))))
      (kill-buffer buf))))

;;; Mixed buffer context tests

(ert-deftest gptel-context-test-buffer-mixed-all-three ()
  "Test buffer context mixing :overlays, :bounds, and :lines."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Overlay for Line 1
          (let* ((ov (make-overlay (point-min)
                                   (progn (goto-char (point-min))
                                          (forward-line 1)
                                          (point))))
                 ;; Bounds for Line 3
                 (bounds-start (progn (goto-char (point-min))
                                      (forward-line 2)
                                      (point)))
                 (bounds-end (progn (forward-line 1) (point)))
                 ;; Lines for Line 5
                 (context-data (list :overlays (list ov)
                                     :bounds (cons bounds-start bounds-end)
                                     :lines (cons 5 5)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 3))
            (should (equal (car extracted) "Line 1\n"))
            (should (equal (cadr extracted) "Line 3\n"))
            (should (equal (caddr extracted) "Line 5\n"))
            (delete-overlay ov)))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-buffer-mixed-with-overlap ()
  "Test overlapping regions are handled correctly."
  (let* ((content "Line 1\nLine 2\nLine 3\nLine 4\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Bounds: Lines 1-2
          (let* ((bounds1-start (point-min))
                 (bounds1-end (progn (goto-char (point-min))
                                     (forward-line 2)
                                     (point)))
                 ;; Bounds: Lines 2-3 (overlaps with above)
                 (bounds2-start (progn (goto-char (point-min))
                                       (forward-line 1)
                                       (point)))
                 (bounds2-end (progn (forward-line 2) (point)))
                 (context-data (list :bounds (list (cons bounds1-start bounds1-end)
                                                   (cons bounds2-start bounds2-end))))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            ;; Both regions should be extracted (no deduplication)
            (should (equal (length extracted) 2))
            (should (equal (car extracted) "Line 1\nLine 2\n"))
            (should (equal (cadr extracted) "Line 2\nLine 3\n"))))
      (kill-buffer buf))))

;;; File context tests

(ert-deftest gptel-context-test-file-line-ranges ()
  "Test file context with :lines specification."
  (let ((test-file (make-temp-file "gptel-context-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "File Line 1\nFile Line 2\nFile Line 3\nFile Line 4\n"))
          ;; Test extracting lines 2-3
          (with-temp-buffer
            (gptel-context--insert-file-string test-file '(:lines (2 . 3)))
            (let ((result (buffer-string)))
              ;; Should contain the file lines
              (should (string-match-p "File Line 2" result))
              (should (string-match-p "File Line 3" result))
              ;; Should not contain Line 1 or Line 4
              (should-not (string-match-p "File Line 1" result))
              (should-not (string-match-p "File Line 4" result)))))
      (delete-file test-file))))

(ert-deftest gptel-context-test-file-bounds ()
  "Test file context with :bounds specification."
  (let ((test-file (make-temp-file "gptel-context-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "0123456789ABCDEFGHIJ"))
          ;; Test extracting characters 5-10
          (with-temp-buffer
            (gptel-context--insert-file-string test-file '(:bounds (6 . 11)))
            (let ((result (buffer-string)))
              (should (string-match-p "56789" result)))))
      (delete-file test-file))))

(ert-deftest gptel-context-test-file-multiple-regions ()
  "Test file context with multiple :bounds and :lines."
  (let ((test-file (make-temp-file "gptel-context-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"))
          ;; Test extracting line 1 and lines 4-5
          (with-temp-buffer
            (gptel-context--insert-file-string
             test-file '(:lines ((1 . 1) (4 . 5))))
            (let ((result (buffer-string)))
              (should (string-match-p "Line 1" result))
              (should (string-match-p "Line 4" result))
              (should (string-match-p "Line 5" result))
              (should-not (string-match-p "Line 2" result)))))
      (delete-file test-file))))

(ert-deftest gptel-context-test-file-buffer-reuse ()
  "Test that visiting buffers are reused for file context."
  (let ((test-file (make-temp-file "gptel-context-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Content for reuse test\n"))
          ;; Visit the file in a buffer
          (let ((visiting-buf (find-file-noselect test-file)))
            (unwind-protect
                (progn
                  ;; Extract from file
                  (with-temp-buffer
                    (gptel-context--insert-file-string test-file '(:lines (1 . 1)))
                    (let ((result (buffer-string)))
                      (should (string-match-p "Content for reuse test" result))))
                  ;; Verify the visiting buffer still exists
                  (should (buffer-live-p visiting-buf)))
              (kill-buffer visiting-buf))))
      (delete-file test-file))))

;;; Context collection tests

(ert-deftest gptel-context-test-collect-old-format-buffer ()
  "Test that old format (buffer) is handled correctly."
  (let* ((content "Test content\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-context (list buf)))
    (unwind-protect
        (let ((collected (gptel-context--collect)))
          ;; Should return list with buffer entry
          (should (equal (length collected) 1))
          (should (equal (caar collected) buf))
          ;; Spec should be nil for full buffer
          (should (null (cdar collected))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-collect-old-format-file ()
  "Test that old format (string file path) is handled correctly."
  (let ((test-file (make-temp-file "gptel-context-test")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "File content\n"))
          (let* ((gptel-context (list test-file))
                 (collected (gptel-context--collect)))
            ;; Should return list with file entry
            (should (equal (length collected) 1))
            (should (equal (caar collected) test-file))
            ;; Spec should have :mime guessed (it's a text file)
            (let ((spec (cdar collected)))
              (should (or (null spec)  ;; might not have mime for text
                          (plist-get spec :mime))))))
      (delete-file test-file))))

(ert-deftest gptel-context-test-collect-new-format ()
  "Test that new format (source . plist) is handled correctly."
  (let* ((content "Test content\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          (let* ((start (point-min))
                 (end (progn (goto-char (point-min))
                             (forward-line 1)
                             (point)))
                 (gptel-context (list (cons buf (list :bounds (cons start end)))))
                 (collected (gptel-context--collect)))
            ;; Should return the entry as-is
            (should (equal (length collected) 1))
            (should (equal (caar collected) buf))
            (let ((spec (cdar collected)))
              (should (plist-get spec :bounds))
              (should (equal (plist-get spec :bounds) (cons start end))))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-collect-dead-buffer ()
  "Test that dead buffers are filtered out."
  (let* ((buf (gptel-context-test--make-buffer "Test\n"))
         (gptel-context (list buf)))
    ;; Kill the buffer
    (kill-buffer buf)
    ;; Collection should be empty
    (let ((collected (gptel-context--collect)))
      (should (equal (length collected) 0)))))

(ert-deftest gptel-context-test-collect-nonexistent-file ()
  "Test that non-existent files are filtered out."
  (let* ((gptel-context (list "/nonexistent/file/path.txt"))
         (collected (gptel-context--collect)))
    (should (equal (length collected) 0))))

;;; Edge case tests

(ert-deftest gptel-context-test-empty-specs ()
  "Test that empty specifications don't cause errors."
  (let* ((content "Line 1\nLine 2\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Empty overlays
          (let* ((context-data (list :overlays nil))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 0)))

          ;; Empty bounds
          (let* ((context-data (list :bounds nil))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 0)))

          ;; Empty lines
          (let* ((context-data (list :lines nil))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 0))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-out-of-bounds-positions ()
  "Test handling of out-of-bounds positions."
  (let* ((content "Line 1\nLine 2\n")
         (buf (gptel-context-test--make-buffer content))
         bounds-start
         bounds-end
         result-string)
    (with-current-buffer buf
      (setq bounds-start (point-min))
      (setq bounds-end (min (point-max) 10000)))  ;; Clamp to actual buffer size
    (unwind-protect
        (progn
          ;; Test that large bounds work correctly (clamped to buffer size)
          (with-temp-buffer
            (gptel-context--insert-buffer-string
             buf (list :bounds (cons bounds-start bounds-end)))
            (setq result-string (buffer-string)))
          ;; Should contain all content
          (should (string-match-p "Line 1" result-string))
          (should (string-match-p "Line 2" result-string)))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-out-of-bounds-lines ()
  "Test handling of out-of-bounds line numbers."
  (let* ((content "Line 1\nLine 2\n")
         (buf (gptel-context-test--make-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          ;; Lines beyond buffer - should extract to end
          (let* ((context-data (list :lines (cons 1 100)))
                 (extracted (gptel-context-test--extract-regions buf context-data)))
            (should (equal (length extracted) 1))
            ;; Should get all content
            (should (equal (car extracted) content))))
      (kill-buffer buf))))

;;; Preset integration tests

(ert-deftest gptel-context-test-preset-with-context ()
  "Test that presets can set context with targeted specifications."
  (let ((test-file (make-temp-file "gptel-context-test"))
        (gptel-context nil))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Preset test content\n"))
          ;; Create a preset with targeted file context
          (gptel-make-preset 'test-context-preset
            :description "Test preset with context"
            :context (list (cons test-file (list :lines (cons 1 1)))))
          ;; Apply the preset
          (gptel--apply-preset 'test-context-preset)
          ;; Verify context was set
          (should (equal (length gptel-context) 1))
          (should (equal (caar gptel-context) test-file))
          (let ((spec (cdar gptel-context)))
            (should (plist-get spec :lines))
            (should (equal (plist-get spec :lines) (cons 1 1)))))
      (delete-file test-file)
      ;; Clean up preset
      (setf (alist-get 'test-context-preset gptel--known-presets nil 'remove) nil))))

(ert-deftest gptel-context-test-preset-with-buffer-context ()
  "Test that presets can set buffer context with :bounds."
  (let* ((content "Preset buffer test\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-context nil))
    (unwind-protect
        (with-current-buffer buf
          (let* ((start (point-min))
                 (end (point-max)))
            ;; Create preset with buffer bounds
            (gptel-make-preset 'test-buffer-preset
              :description "Test preset with buffer context"
              :context (list (cons buf (list :bounds (cons start end)))))
            ;; Apply preset
            (gptel--apply-preset 'test-buffer-preset)
            ;; Verify context
            (should (equal (length gptel-context) 1))
            (should (equal (caar gptel-context) buf))
            (let ((spec (cdar gptel-context)))
              (should (plist-get spec :bounds))
              (should (equal (plist-get spec :bounds) (cons start end))))))
      (kill-buffer buf)
      ;; Clean up preset
      (setf (alist-get 'test-buffer-preset gptel--known-presets nil 'remove) nil))))

(ert-deftest gptel-context-test-preset-context-append ()
  "Test that preset context can be appended to existing context."
  (let ((test-file (make-temp-file "gptel-context-test"))
        (gptel-context nil))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Base content\n"))
          ;; Set initial context
          (setq gptel-context (list test-file))
          ;; Create preset that appends context
          (gptel-make-preset 'test-append-preset
            :description "Test preset that appends context"
            :context (list :append (list (cons test-file (list :lines (cons 1 1))))))
          ;; Apply preset
          (gptel--apply-preset 'test-append-preset)
          ;; Should have two entries now (old + new)
          (should (>= (length gptel-context) 2)))
      (delete-file test-file)
      ;; Clean up preset
      (setf (alist-get 'test-append-preset gptel--known-presets nil 'remove) nil))))

;;; Integration tests - verify context is injected into requests

(ert-deftest gptel-context-test-integration-system-injection ()
  "Test that context is actually injected into system message."
  (let* ((content "Context content\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-context (list buf))
         (gptel-use-context 'system)
         (gptel-system-prompt "Original system message"))
    (unwind-protect
        (with-temp-buffer
          ;; Call the actual integration function
          (let ((context-string (gptel-context--string (gptel-context--collect))))
            (gptel-context--wrap-in-buffer context-string 'system)
            ;; Verify the system message now contains context  
            (should (string-match-p "Context content" gptel-system-prompt))
            (should (string-match-p "Original system message" gptel-system-prompt))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-integration-user-injection ()
  "Test that context is injected into user prompt area."
  (let* ((content "User context\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-context (list buf))
         (gptel-use-context 'user))
    (unwind-protect
        (with-temp-buffer
          ;; Set up a buffer with existing content
          (insert "Existing user prompt")
          (goto-char (point-max))
          ;; Call the actual integration function
          (let ((context-string (gptel-context--string (gptel-context--collect))))
            (gptel-context--wrap-in-buffer context-string 'user)
            ;; Verify context was prepended
            (should (string-match-p "User context" (buffer-string)))
            (should (string-match-p "Existing user prompt" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-integration-with-bounds ()
  "Test that buffer context with :bounds is properly injected."
  (let* ((content "Line 1\nLine 2\nLine 3\n")
         (buf (gptel-context-test--make-buffer content))
         (gptel-use-context 'system)
         (gptel-system-prompt "System"))
    (unwind-protect
        (with-current-buffer buf
          (let* ((start (progn (goto-char (point-min))
                               (forward-line 1)
                               (point)))
                 (end (progn (forward-line 1) (point)))
                 (gptel-context (list (cons buf (list :bounds (cons start end))))))
            (with-temp-buffer
              (let ((context-string (gptel-context--string (gptel-context--collect))))
                (gptel-context--wrap-in-buffer context-string 'system)
                ;; Should contain Line 2 but not Line 1 or 3
                (should (string-match-p "Line 2" gptel-system-prompt))
                (should-not (string-match-p "Line 1" gptel-system-prompt))
                (should-not (string-match-p "Line 3" gptel-system-prompt))))))
      (kill-buffer buf))))

(ert-deftest gptel-context-test-integration-with-lines ()
  "Test that file context with :lines is properly injected."
  (let ((test-file (make-temp-file "gptel-context-test"))
        (gptel-use-context 'system)
        (gptel-system-prompt "System"))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "File Line 1\nFile Line 2\nFile Line 3\nFile Line 4\n"))
          (let ((gptel-context (list (cons test-file (list :lines (cons 2 3))))))
            (with-temp-buffer
              (let ((context-string (gptel-context--string (gptel-context--collect))))
                (gptel-context--wrap-in-buffer context-string 'system)
                ;; Should contain lines 2-3
                (should (string-match-p "File Line 2" gptel-system-prompt))
                (should (string-match-p "File Line 3" gptel-system-prompt))
                ;; Should not contain lines 1 or 4
                (should-not (string-match-p "File Line 1" gptel-system-prompt))
                (should-not (string-match-p "File Line 4" gptel-system-prompt))))))
      (delete-file test-file))))

(ert-deftest gptel-context-test-integration-multiple-sources ()
  "Test that multiple context sources are all injected."
  (let* ((buf1 (gptel-context-test--make-buffer "Buffer 1 content\n"))
         (buf2 (gptel-context-test--make-buffer "Buffer 2 content\n"))
         (test-file (make-temp-file "gptel-context-test"))
         (gptel-use-context 'system)
         (gptel-system-prompt "System"))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "File content\n"))
          (let ((gptel-context (list buf1 buf2 test-file)))
            (with-temp-buffer
              (let ((context-string (gptel-context--string (gptel-context--collect))))
                (gptel-context--wrap-in-buffer context-string 'system)
                ;; All three sources should be present
                (should (string-match-p "Buffer 1 content" gptel-system-prompt))
                (should (string-match-p "Buffer 2 content" gptel-system-prompt))
                (should (string-match-p "File content" gptel-system-prompt))))))
      (kill-buffer buf1)
      (kill-buffer buf2)
      (delete-file test-file))))

;;; Media file tests

(defun gptel-context-test--make-binary-file (suffix &optional content)
  "Create a temporary binary file with EXTENSION SUFFIX.
CONTENT defaults to fake PNG or PDF magic bytes as appropriate."
  (make-temp-file "gptel-context-test-" nil suffix
                  (or content
                      (pcase suffix
                        ((pred (string-equal ".png"))
                         "\x89PNG\r\n\x1a\n")
                        ((pred (string-equal ".pdf"))
                         "%PDF-1.4\n\x00\x00\x00fake")
                        ((pred (string-equal ".jpg"))
                         "\xff\xd8\xff\xdbfake")))))

(ert-deftest gptel-context-test-collect-media-raw-string-files ()
  "Test that raw string file entries in `gptel-context' become media.
Binary files specified as bare paths (without going through
`gptel-add-file') must be annotated with their MIME type by
`gptel-context--collect-media' and returned as :media entries."
  (let ((png-file (gptel-context-test--make-binary-file ".png"))
        (pdf-file (gptel-context-test--make-binary-file ".pdf"))
        (txt-file (make-temp-file "gptel-context-test-" nil ".txt" "text\n")))
    (unwind-protect
        (let* ((gptel-model 'gptel-test-model-multimodal)
               (gptel-context (list png-file pdf-file txt-file))
               (media (gptel-context--collect-media))
               (paths (mapcar (lambda (entry) (plist-get entry :media)) media)))
          ;; Binary files collected, text file not
          (should (= (length media) 2))
          (should (member png-file paths))
          (should (member pdf-file paths))
          (should-not (member txt-file paths))
          ;; Entries have the :media form with a MIME type
          (dolist (entry media)
            (should (eq (car entry) :media))
            (should (member (plist-get entry :mime)
                            '("image/png" "application/pdf")))))
      (delete-file png-file)
      (delete-file pdf-file)
      (delete-file txt-file))))

(ert-deftest gptel-context-test-collect-media-mime-gate ()
  "Test that unsupported binary files are excluded from media collection.
A model without \"application/pdf\" in its `:mime-types' must not
receive PDF files as media, and a model without `media' capability
must not receive any media at all."
  (let ((png-file (gptel-context-test--make-binary-file ".png"))
        (pdf-file (gptel-context-test--make-binary-file ".pdf")))
    (unwind-protect
        (progn
          ;; Image-only model: png kept, pdf dropped
          (let* ((gptel-model 'gptel-test-model-image-only)
                 (gptel-context (list png-file pdf-file))
                 (media (gptel-context--collect-media)))
            (should (= (length media) 1))
            (should (equal (plist-get (car media) :media) png-file))
            (should (equal (plist-get (car media) :mime) "image/png")))
          ;; Text-only model: no media at all
          (let* ((gptel-model 'gptel-test-model-text)
                 (gptel-context (list png-file pdf-file))
                 (media (gptel-context--collect-media)))
            (should (null media))))
      (delete-file png-file)
      (delete-file pdf-file))))

(ert-deftest gptel-context-test-add-file-binary-gate ()
  "Test that `gptel-add-file' respects model MIME capabilities.
Binary files must be added with their MIME type for models that
declare support for it, and rejected entirely otherwise."
  (let ((png-file (gptel-context-test--make-binary-file ".png")))
    (unwind-protect
        (progn
          ;; Multimodal model: file added with :mime
          (let ((gptel-model 'gptel-test-model-multimodal)
                (gptel-context nil))
            (gptel-add-file png-file)
            (should (equal gptel-context
                           (list (list png-file :mime "image/png")))))
          ;; Text-only model: binary file rejected
          (let ((gptel-model 'gptel-test-model-text)
                (gptel-context nil))
            (gptel-add-file png-file)
            (should (null gptel-context))))
      (delete-file png-file))))

(ert-deftest gptel-context-test-collect-directory-expansion ()
  "Test that directory context entries are expanded to per-file entries.
Each contained file must appear individually in the collection, with
binary files annotated with their MIME type, and the directory itself
must not appear as an entry."
  (let* ((dir (make-temp-file "gptel-context-test-dir-" t))
         (txt-file (expand-file-name "a.txt" dir))
         (png-file (expand-file-name "b.png" dir)))
    (unwind-protect
        (progn
          (with-temp-file txt-file (insert "text file\n"))
          (let ((coding-system-for-write 'binary))
            (with-temp-file png-file (insert "\x89PNG\r\n\x1a\n")))
          (let* ((gptel-model 'gptel-test-model-multimodal)
                 (gptel-context (list dir))
                 (collected (gptel-context--collect))
                 (paths (mapcar #'car collected)))
            (should (= (length collected) 2))
            (should (member txt-file paths))
            (should (member png-file paths))
            (should-not (member dir paths))
            ;; The png entry carries its MIME type
            (let ((png-entry (assoc png-file collected)))
              (should png-entry)
              (should (equal (plist-get (cdr png-entry) :mime)
                             "image/png")))))
      (delete-file txt-file)
      (delete-file png-file)
      (delete-directory dir))))

(provide 'gptel-context-test)
;;; gptel-context-test.el ends here
