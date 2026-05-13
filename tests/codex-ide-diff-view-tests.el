;;; codex-ide-diff-view-tests.el --- Tests for codex-ide diff views -*- lexical-binding: t; -*-

;;; Commentary:

;; Diff viewer coverage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-diff-open-buffer-displays-codex-diff-mode-buffer ()
  (let ((display-call nil)
        diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil)))
          (setq diff-buffer
                (codex-ide-diff-open-buffer
                 (string-join
                  '("diff --git a/foo.txt b/foo.txt"
                    "--- a/foo.txt"
                    "+++ b/foo.txt"
                    "@@ -1 +1 @@"
                    "-old"
                    "+new")
                  "\n")))
          (should (buffer-live-p diff-buffer))
          (should (equal (car display-call) diff-buffer))
          (with-current-buffer diff-buffer
            (should (eq major-mode 'codex-ide-diff-mode))
            (should (derived-mode-p 'diff-mode))
            (should buffer-read-only)
            (should (string-match-p
                     (regexp-quote "diff --git a/foo.txt b/foo.txt")
                     (buffer-string)))
            (should (string-suffix-p "\n" (buffer-string)))
            (should (string-match-p "foo\\.txt" (buffer-name)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(ert-deftest codex-ide-diff-open-buffer-binds-return-to-source-jump ()
  (let ((display-call nil)
        diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil)))
          (setq diff-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                 nil
                 default-directory))
          (should (equal (car display-call) diff-buffer))
          (with-current-buffer diff-buffer
            (should (eq (key-binding (kbd "RET"))
                        #'codex-ide-diff-goto-source-at-point))
            (should (eq (key-binding (kbd "<return>"))
                        #'codex-ide-diff-goto-source-at-point))
            (should (equal (expand-file-name default-directory)
                           (file-name-as-directory
                            (expand-file-name default-directory))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(ert-deftest codex-ide-diff-mode-binds-file-folding-commands ()
  (with-temp-buffer
    (codex-ide-diff-mode)
    (should (eq (key-binding (kbd "C-c TAB"))
                #'codex-ide-diff-toggle-file-at-point))
    (should (eq (key-binding (kbd "C-c C-a"))
                #'codex-ide-diff-collapse-all-files))
    (should (eq (key-binding (kbd "C-c C-e"))
                #'codex-ide-diff-expand-all-files))))

(ert-deftest codex-ide-diff-collapse-and-expand-all-files ()
  (with-temp-buffer
    (insert (string-join
             '("diff --git a/foo.txt b/foo.txt"
               "--- a/foo.txt"
               "+++ b/foo.txt"
               "@@ -1 +1 @@"
               "-old"
               "+new"
               "diff --git a/bar.txt b/bar.txt"
               "--- a/bar.txt"
               "+++ b/bar.txt"
               "@@ -1 +1 @@"
               "-older"
               "+newer")
             "\n"))
    (codex-ide-diff-mode)
    (should (= (codex-ide-diff-collapse-all-files) 2))
    (let ((folds (seq-filter
                  (lambda (overlay)
                    (overlay-get overlay 'codex-ide-diff-file-fold))
                  (overlays-in (point-min) (point-max)))))
      (should (= (length folds) 2))
      (dolist (overlay folds)
        (should (overlay-get overlay 'invisible))
        (should (string-match-p
                 "hidden diff lines"
                 (or (overlay-get overlay 'after-string) "")))))
    (codex-ide-diff-expand-all-files)
    (should-not
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'codex-ide-diff-file-fold))
      (overlays-in (point-min) (point-max))))))

(ert-deftest codex-ide-diff-toggle-file-at-point-folds-single-file ()
  (with-temp-buffer
    (insert (string-join
             '("diff --git a/foo.txt b/foo.txt"
               "--- a/foo.txt"
               "+++ b/foo.txt"
               "@@ -1 +1 @@"
               "-old"
               "+new"
               "diff --git a/bar.txt b/bar.txt"
               "--- a/bar.txt"
               "+++ b/bar.txt"
               "@@ -1 +1 @@"
               "-older"
               "+newer")
             "\n"))
    (codex-ide-diff-mode)
    (goto-char (point-min))
    (codex-ide-diff-toggle-file-at-point)
    (let ((folds (seq-filter
                  (lambda (overlay)
                    (overlay-get overlay 'codex-ide-diff-file-fold))
                  (overlays-in (point-min) (point-max)))))
      (should (= (length folds) 1))
      (should (string-match-p
               "foo\\.txt"
               (buffer-substring-no-properties
                (point-min)
                (overlay-start (car folds))))))
    (codex-ide-diff-toggle-file-at-point)
    (should-not
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'codex-ide-diff-file-fold))
      (overlays-in (point-min) (point-max))))))

(ert-deftest codex-ide-diff-collapse-all-files-supports-headerless-normalized-diff ()
  (with-temp-buffer
    (insert (string-join
             '("--- a/foo.txt"
               "+++ b/foo.txt"
               "@@ -1 +1 @@"
               "-old"
               "+new"
               "--- a/bar.txt"
               "+++ b/bar.txt"
               "@@ -1 +1 @@"
               "-older"
               "+newer")
             "\n"))
    (codex-ide-diff-mode)
    (should (= (codex-ide-diff-collapse-all-files) 2))))

(ert-deftest codex-ide-diff-source-location-tracks-hunk-new-lines ()
  (let ((diff-text
         (string-join
          '("diff --git a/foo.txt b/bar.txt"
            "--- a/foo.txt"
            "+++ b/bar.txt"
            "@@ -1,3 +10,4 @@"
            " context"
            "-old"
            "+new"
            " after")
          "\n")))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 4)
                   '(:path "bar.txt" :line 10)))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 5)
                   '(:path "bar.txt" :line 11)))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 6)
                   '(:path "bar.txt" :line 11)))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 7)
                   '(:path "bar.txt" :line 12)))))

(ert-deftest codex-ide-diff-source-location-resolves-normalized-headerless-patch ()
  (let* ((item `((type . "fileChange")
                 (changes . (((path . "foo.txt")
                              (diff . ,(string-join
                                        '("@@ -3,2 +3,3 @@"
                                          " context"
                                          "+new")
                                        "\n")))))))
         (diff-text (codex-ide--file-change-diff-text item)))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 4)
                   '(:path "foo.txt" :line 3)))
    (should (equal (codex-ide-diff--source-location-for-line diff-text 5)
                   '(:path "foo.txt" :line 4)))))

(ert-deftest codex-ide-diff-goto-source-resolves-project-relative-header ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-view-" t)))
         (file (expand-file-name "lib/foo.txt" root))
         (diff-text (string-join
                     '("diff --git a/lib/foo.txt b/lib/foo.txt"
                       "--- a/lib/foo.txt"
                       "+++ b/lib/foo.txt"
                       "@@ -1 +1 @@"
                       "-old"
                       "+new")
                     "\n"))
         visited-buffer)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "new\n"))
          (codex-ide-diff-goto-source diff-text 5 root)
          (setq visited-buffer (current-buffer))
          (should (equal (buffer-file-name visited-buffer) file))
          (should (= (line-number-at-pos) 1)))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-open-buffer-reuses-explicit-buffer-name ()
  (let ((display-calls nil)
        (buffer-name "*codex[my-project]*-diff")
        first-buffer
        second-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (push (list buffer action) display-calls)
                     nil)))
          (setq first-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                 buffer-name))
          (setq second-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/bar.txt b/bar.txt\n@@ -1 +1 @@\n-older\n+newer"
                 buffer-name))
          (should (eq first-buffer second-buffer))
          (should (equal (buffer-name first-buffer) buffer-name))
          (with-current-buffer first-buffer
            (should (string-match-p "bar\\.txt" (buffer-string)))
            (should-not (string-match-p "foo\\.txt" (buffer-string)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest codex-ide-diff-buffer-name-for-session-appends-suffix ()
  (should (equal (codex-ide-diff-buffer-name-for-session "*codex[my-project]*")
                 "*codex[my-project]*-diff")))

(ert-deftest codex-ide-session-diff-buffer-name-for-session-appends-session-suffix ()
  (should (equal (codex-ide-session-diff-buffer-name-for-session
                  "*codex[my-project]*")
                 "*codex[my-project]*-session-diff")))

(ert-deftest codex-ide-session-diff-open-uses-canonical-mode-and-live-source ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-session-diff]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (display-call nil)
         diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil))
                  ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                   (lambda (resolved-session &optional turn-id)
                     (should (eq resolved-session session))
                     (should-not turn-id)
                     "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new")))
          (setq diff-buffer (codex-ide-session-diff-open))
          (should (buffer-live-p diff-buffer))
          (should (eq (car display-call) diff-buffer))
          (should (equal (buffer-name diff-buffer)
                         "*codex[test-session-diff]*-session-diff"))
          (with-current-buffer diff-buffer
            (should (eq major-mode 'codex-ide-session-diff-mode))
            (should (derived-mode-p 'codex-ide-diff-mode))
            (should (eq codex-ide-session-diff--session session))
            (should (eq codex-ide-session-diff-source 'live))
            (should (string-match-p "foo\\.txt" (buffer-string)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-refreshes-live-buffer ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-live-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         (diff-text "diff --git a/one.txt b/one.txt\n@@ -1 +1 @@\n-old\n+new"))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'live))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (should-not turn-id)
                       diff-text)))
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (should (string-match-p "one\\.txt" (buffer-string))))
            (setq diff-text
                  "diff --git a/two.txt b/two.txt\n@@ -1 +1 @@\n-old\n+new")
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (should (string-match-p "two\\.txt" (buffer-string)))
              (should-not (string-match-p "one\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-ignores-pinned-buffer ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-pinned-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'pinned)
            (let ((inhibit-read-only t))
              (insert "original\n")))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (&rest _)
                       (error "Pinned buffers should not live-refresh"))))
            (codex-ide-session-diff-note-session-updated session))
          (with-current-buffer diff-buffer
            (should (equal (buffer-string) "original\n"))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-refreshes-transcript-current-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-live]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :current-turn-id "turn-1"
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         requested-turns)
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id "turn-1"))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (push turn-id requested-turns)
                       "diff --git a/current.txt b/current.txt\n@@ -1 +1 @@\n-old\n+new")))
            (codex-ide-session-diff-note-session-updated session)
            (should (equal requested-turns '("turn-1")))
            (with-current-buffer diff-buffer
              (should (string-match-p "current\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-transcript-point-changed-refreshes-new-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         requested-turns)
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id "turn-1"))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (push turn-id requested-turns)
                       (format "diff --git a/%s.txt b/%s.txt\n@@ -1 +1 @@\n-old\n+new"
                               turn-id
                               turn-id))))
            (codex-ide-session-diff-transcript-point-changed session "turn-1")
            (should-not requested-turns)
            (codex-ide-session-diff-transcript-point-changed session "turn-2")
            (should (equal requested-turns '("turn-2")))
            (with-current-buffer diff-buffer
              (should (equal codex-ide-session-diff--turn-id "turn-2"))
              (should (string-match-p "turn-2\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-transcript-source-without-turn-shows-empty-state ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-empty]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id nil))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (&rest _)
                       (error "Transcript mode should not fall back to latest turn"))))
            (codex-ide-session-diff-refresh diff-buffer))
          (with-current-buffer diff-buffer
            (should (string-match-p
                     "No prompt at transcript position"
                     (buffer-string)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-diff-open-buffer-errors-without-diff-text ()
  (should-error (codex-ide-diff-open-buffer nil) :type 'user-error)
  (should-error (codex-ide-diff-open-buffer "  \n") :type 'user-error))

(ert-deftest codex-ide-diff-open-combined-turn-buffer-uses-dedicated-buffer-name ()
  (let* ((session-buffer (generate-new-buffer "*codex[test]*"))
         (session (make-instance 'codex-ide-session :buffer session-buffer))
         (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                   (lambda (&optional resolved-session turn-id)
                     (should (eq resolved-session session))
                     (should-not turn-id)
                     "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"))
                  ((symbol-function 'codex-ide-diff-open-buffer)
                   (lambda (diff-text buffer-name &optional _directory)
                     (setq opened (list diff-text buffer-name))
                     nil)))
          (codex-ide-diff-open-combined-turn-buffer)
          (should (equal opened
                         '("diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                           "*codex[test]*-turn-diff"))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-diff-open-combined-turn-buffer-interactive-uses-point-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-point]*"))
         (session (make-instance 'codex-ide-session :buffer session-buffer))
         (opened nil))
    (unwind-protect
        (with-current-buffer session-buffer
          (insert "> first\nresult\n\n> second\nresult\n")
          (goto-char (point-min))
          (let ((first-marker (copy-marker (point) nil)))
            (search-forward "> second")
            (let ((second-marker (copy-marker (match-beginning 0) nil)))
              (codex-ide--record-turn-start session "turn-1" first-marker)
              (codex-ide--record-turn-start session "turn-2" second-marker)
              (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                         (lambda () session))
                        ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                         (lambda (&optional resolved-session turn-id)
                           (should (eq resolved-session session))
                           (should (equal turn-id "turn-2"))
                           "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"))
                        ((symbol-function 'codex-ide-diff-open-buffer)
                         (lambda (diff-text buffer-name &optional _directory)
                           (setq opened (list diff-text buffer-name))
                           nil)))
                (call-interactively #'codex-ide-diff-open-combined-turn-buffer)
                (should (equal opened
                               '("diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                                 "*codex[test-point]*-turn-diff")))))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

;;; codex-ide-diff-view-tests.el ends here
