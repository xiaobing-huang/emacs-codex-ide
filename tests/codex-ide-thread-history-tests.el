;;; codex-ide-thread-history-tests.el --- Tests for stored thread history helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for stored thread normalization.

;;; Code:

(require 'ert)
(require 'json)
(require 'codex-ide-thread-history)

(ert-deftest codex-ide-thread-history-adds-rollout-render-items ()
  (let ((path (make-temp-file "codex-ide-thread-history-" nil ".jsonl"))
        (patch-text (string-join
                     '("*** Begin Patch"
                       "*** Update File: foo.txt"
                       "@@"
                       "-old"
                       "+new"
                       "*** End Patch")
                     "\n")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dolist (entry
                     `(((type . "event_msg")
                        (payload . ((type . "task_started"))))
                       ((type . "response_item")
                        (payload . ((type . "custom_tool_call")
                                    (name . "apply_patch")
                                    (call_id . "call-patch-1")
                                    (input . ,patch-text))))
                       ((type . "event_msg")
                        (payload . ((type . "task_complete"))))))
              (insert (json-encode entry) "\n")))
          (let* ((thread-read
                  `((thread . ((id . "thread-1")
                               (path . ,path)
                               (turns . [((id . "turn-1")
                                          (items . [((type . "userMessage")
                                                     (text . "change it"))
                                                    ((type . "agentMessage")
                                                     (text . "done"))]))])))))
                 (normalized
                  (codex-ide--thread-read-with-rollout-render-items
                   thread-read))
                 (turn (car (append (codex-ide--thread-read-turns normalized)
                                    nil)))
                 (items (append (codex-ide--thread-read-items turn) nil)))
            (should (equal (mapcar (lambda (item) (alist-get 'type item)) items)
                           '("userMessage" "fileChange" "agentMessage")))
            (should (equal (alist-get 'diff
                                      (car (alist-get 'changes (cadr items))))
                           patch-text))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest codex-ide-thread-history-preserves-thread-read-without-rollout ()
  (let ((thread-read '((thread . ((id . "thread-1")
                                  (turns . [((id . "turn-1")
                                             (items . []))]))))))
    (should (eq (codex-ide--thread-read-with-rollout-render-items thread-read)
                thread-read))))

(provide 'codex-ide-thread-history-tests)

;;; codex-ide-thread-history-tests.el ends here
