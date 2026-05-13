;;; codex-ide-diff-data-tests.el --- Tests for codex-ide diff data -*- lexical-binding: t; -*-

;;; Commentary:

;; Diff normalization and lookup coverage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-combine-diff-texts-trims-and-joins-blocks ()
  (should
   (equal (codex-ide--combine-diff-texts
           '("diff --git a/foo b/foo\n@@ -1 +1 @@\n-old\n+new\n"
             nil
             "   \n"
             "diff --git a/bar b/bar\n@@ -1 +1 @@\n-before\n+after  "))
          (concat
           "diff --git a/foo b/foo\n@@ -1 +1 @@\n-old\n+new"
           "\n\n"
           "diff --git a/bar b/bar\n@@ -1 +1 @@\n-before\n+after"))))

(ert-deftest codex-ide-combine-diff-texts-returns-nil-when-empty ()
  (should-not (codex-ide--combine-diff-texts nil))
  (should-not (codex-ide--combine-diff-texts '(" \n" nil))))

(ert-deftest codex-ide-file-change-diff-text-uses-apply-patch-file-header ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Update File: foo.txt"
                        "@@"
                        "-old"
                        "+new"
                        "*** End Patch")
                      "\n"))
         (expected-diff (string-join
                         '("--- a/foo.txt"
                           "+++ b/foo.txt"
                           "@@"
                           "-old"
                           "+new")
                         "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "patch")
                              (diff . ,patch-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   expected-diff))
    (should-not (string-match-p
                 (rx line-start "*** " (or "Begin" "End" "Update") (* nonl))
                 (codex-ide--file-change-diff-text item)))))

(ert-deftest codex-ide-file-change-diff-text-normalizes-top-level-apply-patch ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Add File: foo.txt"
                        "@@"
                        "+new"
                        "*** End Patch")
                      "\n"))
         (item `((type . "fileChange")
                 (diff . ,patch-text))))
    (should (equal (codex-ide--file-change-diff-text item)
                   (string-join
                    '("--- /dev/null"
                      "+++ b/foo.txt"
                      "@@"
                      "+new")
                    "\n")))))

(ert-deftest codex-ide-file-change-diff-text-normalizes-multi-file-apply-patch ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Update File: foo.txt"
                        "@@"
                        "-old"
                        "+new"
                        "*** Delete File: bar.txt"
                        "@@"
                        "-gone"
                        "*** End Patch")
                      "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "patch")
                              (diff . ,patch-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   (string-join
                    '("--- a/foo.txt"
                      "+++ b/foo.txt"
                      "@@"
                      "-old"
                      "+new"
                      ""
                      "--- a/bar.txt"
                      "+++ /dev/null"
                      "@@"
                      "-gone")
                    "\n")))))

(ert-deftest codex-ide-file-change-diff-text-keeps-git-diff-header ()
  (let* ((diff-text (string-join
                     '("diff --git a/foo.txt b/foo.txt"
                       "--- a/foo.txt"
                       "+++ b/foo.txt"
                       "@@ -1 +1 @@"
                       "-old"
                       "+new")
                     "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "foo.txt")
                              (diff . ,diff-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   diff-text))))

(ert-deftest codex-ide-file-change-diff-text-preserves-project-local-absolute-headers ()
  (let* ((root (file-name-as-directory
                (expand-file-name "codex-ide-test-project"
                                  temporary-file-directory)))
         (file (expand-file-name "lib/foo.txt" root))
         (diff-text (string-join
                     (list (format "diff --git a/%s b/%s" file file)
                           (format "--- a/%s" file)
                           (format "+++ b/%s" file)
                           "@@ -1 +1 @@"
                           "-old"
                           "+new")
                     "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . ,file)
                              (diff . ,diff-text)))))))
    (let ((default-directory root))
      (should (equal (codex-ide--file-change-diff-text item)
                     diff-text)))))

(ert-deftest codex-ide-file-change-diff-text-wraps-headerless-patch-as-git-diff ()
  (let* ((patch-text (string-join
                      '("@@ -3,2 +3,3 @@"
                        " context"
                        "+new")
                      "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "foo.txt")
                              (diff . ,patch-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   (string-join
                    '("diff --git a/foo.txt b/foo.txt"
                      "--- a/foo.txt"
                      "+++ b/foo.txt"
                      "@@ -3,2 +3,3 @@"
                      " context"
                      "+new")
                    "\n")))))

(ert-deftest codex-ide-turn-file-change-diff-texts-normalizes-historical-items ()
  (let* ((diff-1 (string-join
                  '("diff --git a/foo.txt b/foo.txt"
                    "--- a/foo.txt"
                    "+++ b/foo.txt"
                    "@@ -1 +1 @@"
                    "-old"
                    "+new")
                  "\n"))
         (diff-2 (string-join
                  '("diff --git a/bar.txt b/bar.txt"
                    "--- a/bar.txt"
                    "+++ b/bar.txt"
                    "@@ -2 +2 @@"
                    "-before"
                    "+after")
                  "\n"))
         (file-change-1 `((type . "fileChange")
                          (id . "file-change-1")
                          (changes . (((path . "foo.txt")
                                       (diff . ,diff-1))))))
         (file-change-2 `((type . "fileChange")
                          (id . "file-change-2")
                          (changes . (((path . "bar.txt")
                                       (diff . ,diff-2))))))
         (expected-1 (codex-ide--file-change-diff-text file-change-1))
         (expected-2 (codex-ide--file-change-diff-text file-change-2))
         (turn `((id . "turn-latest")
                 (items . (,file-change-1 ,file-change-2)))))
    (should
     (equal (codex-ide--turn-file-change-diff-texts turn)
            (list expected-1 expected-2)))))

(ert-deftest codex-ide-register-submitted-turn-prompt-clears-prior-diffs ()
  (let* ((session (make-instance 'codex-ide-session))
         (item `((type . "fileChange")
                 (id . "file-change-1")
                 (changes . (((path . "foo.txt")
                              (diff . ,(string-join
                                        '("diff --git a/foo.txt b/foo.txt"
                                          "--- a/foo.txt"
                                          "+++ b/foo.txt"
                                          "@@ -1 +1 @@"
                                          "-old"
                                          "+new")
                                        "\n"))))))))
    (setf (codex-ide-session-current-turn-id session) "turn-1")
    (codex-ide--mark-current-turn-diff-started session "turn-1")
    (codex-ide--put-current-turn-file-change session "file-change-1" item)
    (should (codex-ide--current-turn-diff-texts session))
    (setf (codex-ide-session-current-turn-id session) nil)
    (codex-ide--register-submitted-turn-prompt session "next prompt")
    (should-not (codex-ide--current-turn-diff-texts session))))

(ert-deftest codex-ide-mark-current-turn-diff-started-clears-prior-diffs ()
  (let* ((session (make-instance 'codex-ide-session))
         (item `((type . "fileChange")
                 (id . "file-change-1")
                 (changes . (((path . "foo.txt")
                              (diff . ,(string-join
                                        '("diff --git a/foo.txt b/foo.txt"
                                          "--- a/foo.txt"
                                          "+++ b/foo.txt"
                                          "@@ -1 +1 @@"
                                          "-old"
                                          "+new")
                                        "\n"))))))))
    (codex-ide--mark-current-turn-diff-started session "turn-1")
    (codex-ide--put-current-turn-file-change session "file-change-1" item)
    (should (codex-ide--current-turn-diff-texts session))
    (codex-ide--mark-current-turn-diff-started session "turn-2")
    (should-not (codex-ide--current-turn-diff-texts session))))

(ert-deftest codex-ide-read-turn-combined-diff-text-uses-rollout-render-items ()
  (let ((path (make-temp-file "codex-ide-diff-rollout-" nil ".jsonl"))
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
          (let* ((session (make-instance 'codex-ide-session
                                         :thread-id "thread-1"))
                 (thread-read
                  `((thread . ((id . "thread-1")
                               (path . ,path)
                               (turns . [((id . "turn-1")
                                          (items . [((type . "userMessage")
                                                     (text . "change it"))
                                                    ((type . "agentMessage")
                                                     (text . "done"))]))]))))))
            (cl-letf (((symbol-function 'codex-ide--read-thread)
                       (lambda (_session thread-id _include-history)
                         (should (equal thread-id "thread-1"))
                         thread-read)))
              (should
               (equal (codex-ide-diff-data-combined-turn-diff-text
                       session
                       "turn-1")
                      (string-join
                       '("--- a/foo.txt"
                         "+++ b/foo.txt"
                         "@@"
                         "-old"
                         "+new")
                       "\n"))))))
      (when (file-exists-p path)
        (delete-file path)))))

;;; codex-ide-diff-data-tests.el ends here
