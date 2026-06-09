;;; codex-ide-session-mode-tests.el --- Tests for codex-ide session mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-mode'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide)
(require 'codex-ide-session-mode)

(defun codex-ide-session-mode-test--flatten-imenu-labels (index)
  "Return INDEX labels in depth-first order for assertions."
  (let (labels)
    (cl-labels ((walk (entry)
                  (push (car entry) labels)
                  (unless (markerp (cdr entry))
                    (mapc #'walk (cdr entry)))))
      (mapc #'walk index))
    (nreverse labels)))

(ert-deftest codex-ide-session-mode-docstring-lists-key-bindings ()
  (let ((doc (documentation #'codex-ide-session-mode t)))
    (dolist (binding '("* \\<codex-ide-session-mode-map>\\[codex-ide-submit]"
                       "* \\[codex-ide-interrupt]"
                       "* \\[codex-ide-session-diff-open]"
                       "* \\[codex-ide-apply-config-preset]"
                       "* \\[codex-ide-previous-prompt-line]"
                       "* \\[codex-ide-session-mode-nav-forward]"
                       "* \\<codex-ide-session-prompt-minor-mode-map>\\[codex-ide-previous-prompt-history]"))
      (should (string-match-p (regexp-quote binding) doc)))))

(ert-deftest codex-ide-session-mode-binds-session-diff-open ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should (eq (key-binding (kbd "C-c C-d"))
                #'codex-ide-session-diff-open))))

(ert-deftest codex-ide-session-mode-binds-apply-config-preset ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should (eq (key-binding (kbd "C-c C-p"))
                #'codex-ide-apply-config-preset))))

(ert-deftest codex-ide-session-mode-imenu-indexes-user-prompts ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          first-pos
          second-pos
          index)
      (insert "> first prompt\nassistant reply\n> second prompt\nmore detail\n")
      (goto-char (point-min))
      (setq first-pos (+ (line-beginning-position) 2))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 2)
      (setq second-pos (+ (line-beginning-position) 2))
      (let ((start (line-beginning-position)))
        (forward-line 2)
        (codex-ide-renderer-style-user-prompt-region start (point)))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("first prompt" "second prompt↵more detail")))
      (should (= (marker-position (cdr (nth 0 index))) first-pos))
      (should (= (marker-position (cdr (nth 1 index))) second-pos)))))

(ert-deftest codex-ide-session-mode-imenu-uses-numbered-empty-prompt-labels ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> \n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("Prompt 1"))))))

(ert-deftest codex-ide-session-mode-imenu-normalizes-label-whitespace ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert ">   prompt\twith\n spaced   words  \n# Heading\twith   spaces\n")
      (goto-char (point-min))
      (let ((start (line-beginning-position)))
        (forward-line 2)
        (codex-ide-renderer-style-user-prompt-region start (point)))
      (add-text-properties
       (point)
       (point-max)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("prompt with↵spaced words"))))))

(ert-deftest codex-ide-session-mode-imenu-ignores-agent-messages ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> do work\n# Implementation\nDetails\n## Validation\nDone\n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (let ((start (point)))
        (goto-char (point-max))
        (add-text-properties
         start
         (point)
         `(,codex-ide-agent-item-type-property "agentMessage")))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("do work"))))))

(ert-deftest codex-ide-session-mode-imenu-keeps-prompts-in-buffer-order ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((inhibit-read-only t)
          index)
      (insert "> first prompt\nAgent one\n> second prompt\nAgent two\n")
      (goto-char (point-min))
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (add-text-properties
       (line-beginning-position)
       (line-end-position)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (forward-line 1)
      (codex-ide-renderer-style-user-prompt-region
       (line-beginning-position)
       (line-end-position))
      (forward-line 1)
      (add-text-properties
       (line-beginning-position)
       (line-end-position)
       `(,codex-ide-agent-item-type-property "agentMessage"))
      (setq index (codex-ide-session-mode--imenu-create-index))
      (should (equal (codex-ide-session-mode-test--flatten-imenu-labels index)
                     '("first prompt" "second prompt"))))))

(ert-deftest codex-ide-session-mode-theme-refresh-subscribes-and-tears-down-hooks ()
  (let ((schedule-count 0)
        (codex-ide-session-mode--theme-refresh-buffers nil)
        (buffer (generate-new-buffer " *codex-ide-session-theme-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'codex-ide-renderer-schedule-theme-refresh)
                     (lambda ()
                       (setq schedule-count (1+ schedule-count)))))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (should (= schedule-count 1))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (run-hook-with-args 'disable-theme-functions 'test-theme)
            (should (= schedule-count 3))
            (fundamental-mode)
            (should-not (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (should (= schedule-count 4))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (should (= schedule-count 5))
            (kill-buffer buffer)
            (should-not (memq buffer codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-session-mode-notifies-session-diff-of-point-turn ()
  (let* ((buffer (generate-new-buffer " *codex-ide-session-diff-point-test*"))
         (session (make-instance 'codex-ide-session :buffer buffer))
         notified)
    (unwind-protect
        (with-current-buffer buffer
          (codex-ide-session-mode)
          (setq-local codex-ide--session session)
          (let ((inhibit-read-only t))
            (insert "> first\nresult\n\n> second\nresult\n"))
          (goto-char (point-min))
          (let ((first-marker (copy-marker (point) nil)))
            (search-forward "> second")
            (let ((second-marker (copy-marker (match-beginning 0) nil)))
              (codex-ide--record-turn-start session "turn-1" first-marker)
              (codex-ide--record-turn-start session "turn-2" second-marker)
              (cl-letf (((symbol-function
                          'codex-ide-session-diff-transcript-point-changed)
                         (lambda (notified-session turn-id)
                           (setq notified
                                 (list notified-session turn-id)))))
                (codex-ide-session-mode--notify-diff-point-changed)
                (should (equal notified (list session "turn-2")))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'codex-ide-session-mode-tests)

;;; codex-ide-session-mode-tests.el ends here
