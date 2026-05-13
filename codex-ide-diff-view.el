;;; codex-ide-diff-view.el --- Diff buffer views for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns dedicated diff-buffer presentation for Codex file changes.
;;
;; It turns Codex-provided patch text into a standalone `codex-ide-diff-mode'
;; buffer derived from `diff-mode' and displays that buffer using codex-ide's
;; normal window policy.  Keeping this separate from transcript control lets the
;; transcript layer stay focused on when a diff should be offered while this
;; module owns how the diff is shown.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'codex-ide-diff-data)
(require 'subr-x)

(declare-function codex-ide-display-buffer "codex-ide-window"
                  (buffer &optional action))
(declare-function codex-ide--session-for-current-project "codex-ide-session" ())
(declare-function codex-ide-session-buffer "codex-ide-core" (session))
(declare-function codex-ide-session-current-turn-id "codex-ide-core" (session))
(declare-function codex-ide-session-directory "codex-ide-core" (session))
(declare-function codex-ide-session-p "codex-ide-core" (object))
(declare-function codex-ide-diff-data-combined-turn-diff-text
                  "codex-ide-diff-data" (session &optional turn-id))
(declare-function codex-ide-diff-data-turn-id-at-point
                  "codex-ide-diff-data" (session &optional point buffer))

(defvar codex-ide--display-buffer-other-window-pop-up-action)

(defvar codex-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "C-c TAB") #'codex-ide-diff-toggle-file-at-point)
    (define-key map (kbd "C-c C-a") #'codex-ide-diff-collapse-all-files)
    (define-key map (kbd "C-c C-e") #'codex-ide-diff-expand-all-files)
    (define-key map (kbd "RET") #'codex-ide-diff-goto-source-at-point)
    (define-key map (kbd "<return>") #'codex-ide-diff-goto-source-at-point)
    map)
  "Keymap used in standalone Codex diff buffers.")

(define-derived-mode codex-ide-diff-mode diff-mode "Codex-Diff"
  "Major mode for standalone Codex diff buffers.

\\<codex-ide-diff-mode-map>
* \\[codex-ide-diff-toggle-file-at-point] toggles the file diff at point.
* \\[codex-ide-diff-collapse-all-files] collapses all file diffs.
* \\[codex-ide-diff-expand-all-files] expands all file diffs.
* \\[codex-ide-diff-goto-source-at-point] jumps to source for the diff line at point.")

(defvar-local codex-ide-session-diff--session nil
  "Codex session associated with the current session diff buffer.")

(defvar-local codex-ide-session-diff-source 'live
  "Diff source shown by the current session diff buffer.
The value is one of `live', `transcript', or `pinned'.")

(defvar-local codex-ide-session-diff--turn-id nil
  "Turn id selected by the current session diff buffer, when any.")

(defvar codex-ide-session-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map codex-ide-diff-mode-map)
    (define-key map (kbd "g") #'codex-ide-session-diff-refresh)
    (define-key map (kbd "l") #'codex-ide-session-diff-follow-live)
    (define-key map (kbd "t") #'codex-ide-session-diff-follow-transcript)
    (define-key map (kbd "p") #'codex-ide-session-diff-pin-current-turn)
    map)
  "Keymap used in canonical Codex session diff buffers.")

(define-derived-mode codex-ide-session-diff-mode codex-ide-diff-mode
  "Codex-Session-Diff"
  "Major mode for a canonical Codex session diff buffer."
  (setq-local mode-line-process
              '("[" (:eval (symbol-name codex-ide-session-diff-source)) "]")))

(defvar codex-ide-diff-inline-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-ide-diff-goto-source-at-point)
    (define-key map (kbd "<return>") #'codex-ide-diff-goto-source-at-point)
    map)
  "Keymap used on expanded inline Codex diff body text.")

(defun codex-ide-diff--title (diff-text)
  "Return a compact title for DIFF-TEXT."
  (when (string-match
         (rx line-start
             "diff --"
             (? "git")
             " "
             (+ nonl))
         diff-text)
    (let ((line (string-trim (match-string 0 diff-text))))
      (cond
       ((string-match (rx line-start "diff --git "
                          (? "a/")
                          (group (+ (not (any " \n")))))
                      line)
        (match-string 1 line))
       ((string-match (rx line-start "diff -- " (group (+ nonl))) line)
        (match-string 1 line))
       (t line)))))

(defun codex-ide-diff--generated-buffer-name (diff-text)
  "Return a fresh buffer name suitable for DIFF-TEXT."
  (generate-new-buffer-name
   (format "*codex diff: %s*"
           (or (codex-ide-diff--title diff-text)
               "changes"))))

(defun codex-ide-diff-buffer-name-for-session (session-buffer)
  "Return the diff buffer name for SESSION-BUFFER."
  (format "%s-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff-combined-buffer-name-for-session (session-buffer)
  "Return the combined-turn diff buffer name for SESSION-BUFFER."
  (format "%s-turn-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff--file-section-header-regexp ()
  "Return the regexp used to find file section headers."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (rx line-start "diff --git ") nil t)
        (rx line-start "diff --git ")
      (rx line-start
          (or "--- "
              "*** "
              "Index: ")))))

(defun codex-ide-diff--collect-file-sections ()
  "Return file sections in the current diff buffer.
Each section is a plist containing `:start', `:body-start', and `:end'."
  (let ((header-regexp (codex-ide-diff--file-section-header-regexp))
        sections)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward header-regexp nil t)
        (goto-char (match-beginning 0))
        (let* ((start (point))
               (body-start (min (point-max)
                                (save-excursion
                                  (forward-line 1)
                                  (point))))
               (end (save-excursion
                      (forward-line 1)
                      (if (re-search-forward header-regexp nil t)
                          (match-beginning 0)
                        (point-max)))))
          (when (> end start)
            (push (list :start start
                        :body-start body-start
                        :end end)
                  sections))
          (goto-char (max (1+ (point)) (line-end-position))))))
    (nreverse sections)))

(defun codex-ide-diff--delete-file-fold-overlays ()
  "Delete all file-fold overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'codex-ide-diff-file-fold t))

(defun codex-ide-diff--line-count-between (start end)
  "Return the number of whole or partial lines between START and END."
  (max 0
       (save-excursion
         (goto-char start)
         (count-lines start end))))

(defun codex-ide-diff--make-file-fold-overlay (body-start end)
  "Hide file diff body from BODY-START to END."
  (when (> end body-start)
    (let* ((line-count (codex-ide-diff--line-count-between body-start end))
           (overlay (make-overlay body-start end nil t nil)))
      (overlay-put overlay 'codex-ide-diff-file-fold t)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'isearch-open-invisible #'delete-overlay)
      (overlay-put overlay
                   'after-string
                   (propertize
                    (format "  ... %d hidden diff %s\n"
                            line-count
                            (if (= line-count 1) "line" "lines"))
                    'face 'shadow))
      overlay)))

(defun codex-ide-diff--file-section-at-point ()
  "Return the file section containing point, or nil."
  (let ((pos (point))
        found)
    (dolist (section (codex-ide-diff--collect-file-sections) found)
      (when (and (<= (plist-get section :start) pos)
                 (< pos (plist-get section :end)))
        (setq found section)))))

(defun codex-ide-diff--fold-overlay-for-section (section)
  "Return the fold overlay for SECTION, if any."
  (cl-find-if
   (lambda (overlay)
     (and (overlay-get overlay 'codex-ide-diff-file-fold)
          (= (overlay-start overlay) (plist-get section :body-start))
          (= (overlay-end overlay) (plist-get section :end))))
   (overlays-in (plist-get section :body-start)
                (plist-get section :end))))

(defun codex-ide-diff-collapse-all-files ()
  "Collapse all file sections in the current Codex diff buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (codex-ide-diff--delete-file-fold-overlays)
  (let ((count 0))
    (dolist (section (codex-ide-diff--collect-file-sections))
      (when (codex-ide-diff--make-file-fold-overlay
             (plist-get section :body-start)
             (plist-get section :end))
        (setq count (1+ count))))
    (unless (> count 0)
      (message "No file diffs to collapse"))
    count))

(defun codex-ide-diff-expand-all-files ()
  "Expand all collapsed file sections in the current Codex diff buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (codex-ide-diff--delete-file-fold-overlays))

(defun codex-ide-diff-toggle-file-at-point ()
  "Toggle the file diff section at point."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (let ((section (codex-ide-diff--file-section-at-point)))
    (unless section
      (user-error "No file diff at point"))
    (if-let* ((overlay (codex-ide-diff--fold-overlay-for-section section)))
        (delete-overlay overlay)
      (codex-ide-diff--make-file-fold-overlay
       (plist-get section :body-start)
       (plist-get section :end)))))

(defun codex-ide-session-diff-buffer-name-for-session (session-buffer)
  "Return the canonical session diff buffer name for SESSION-BUFFER."
  (format "%s-session-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff--strip-path-prefix (path)
  "Return PATH without a leading diff-side prefix."
  (cond
   ((not (stringp path)) nil)
   ((or (string-prefix-p "a/" path)
        (string-prefix-p "b/" path))
    (substring path 2))
   (t path)))

(defun codex-ide-diff--parse-new-start (hunk-line)
  "Return the new-file start line from HUNK-LINE, or nil."
  (when (string-match
         (rx line-start "@@"
             (+ (not (any "+")))
             "+"
             (group (+ digit)))
         hunk-line)
    (string-to-number (match-string 1 hunk-line))))

(defun codex-ide-diff--source-location-for-line (diff-text line-index)
  "Return source location for zero-based LINE-INDEX in DIFF-TEXT.
The returned value is a plist containing `:path' and `:line', or nil when the
diff line has no corresponding source location."
  (let ((lines (split-string diff-text "\n"))
        current-path
        current-new-line
        location)
    (cl-loop
     for line in lines
     for index from 0
     until (> index line-index)
     do
     (cond
      ((string-match
        (rx line-start "diff --git " (? "a/")
            (+ (not (any " \n")))
            (+ space) (? "b/")
            (group (+ (not (any " \n")))))
        line)
       (setq current-path (codex-ide-diff--strip-path-prefix
                           (match-string 1 line)))
       (setq current-new-line nil)
       (when (= index line-index)
         (setq location nil)))
      ((string-match
        (rx line-start "+++" (+ space)
            (group (+ (not (any "\n")))))
        line)
       (let ((path (match-string 1 line)))
         (unless (equal path "/dev/null")
           (setq current-path (codex-ide-diff--strip-path-prefix path))))
       (when (= index line-index)
         (setq location nil)))
      ((string-prefix-p "@@" line)
       (setq current-new-line
             (or (codex-ide-diff--parse-new-start line)
                 current-new-line))
       (when (= index line-index)
         (setq location nil)))
      ((and current-path current-new-line
            (not (string-prefix-p "\\ No newline" line)))
       (let ((target-line current-new-line))
         (cond
          ((string-prefix-p "+" line)
           (setq current-new-line (1+ current-new-line)))
          ((string-prefix-p "-" line)
           nil)
          (t
           (setq current-new-line (1+ current-new-line))))
         (when (= index line-index)
           (setq location
                 (list :path current-path
                       :line (max 1 target-line))))))))
    location))

(defun codex-ide-diff--line-index-at-point ()
  "Return the zero-based line index at point in the current buffer."
  (1- (line-number-at-pos)))

(defun codex-ide-diff-goto-source (diff-text line-index &optional directory)
  "Jump from DIFF-TEXT LINE-INDEX to the corresponding source location.
DIRECTORY is used to resolve relative diff paths."
  (let* ((location (codex-ide-diff--source-location-for-line
                    diff-text
                    line-index))
         (path (plist-get location :path))
         (line (plist-get location :line)))
    (unless (and path line)
      (user-error "No source location for this diff line"))
    (let ((file (expand-file-name path (or directory default-directory))))
      (unless (file-exists-p file)
        (user-error "Source file does not exist: %s" file))
      (find-file-other-window file)
      (goto-char (point-min))
      (forward-line (1- line))
      (back-to-indentation)
      (point))))

(defun codex-ide-diff-goto-source-at-point (&optional pos)
  "Jump to the source location corresponding to the diff line at POS."
  (interactive)
  (let* ((pos (or pos (point)))
         (overlay (get-char-property pos 'codex-ide-diff-overlay))
         (body-start (and (overlayp overlay)
                          (overlay-get overlay :body-start)))
         (diff-text (or (and (overlayp overlay)
                             (overlay-get overlay :result-full-text))
                        (and (overlayp overlay)
                             (overlay-get overlay :display-text))
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max))))
         (directory (or (and (overlayp overlay)
                             (overlay-get overlay :directory))
                        default-directory))
         (line-index (if (and (markerp body-start)
                              (eq (marker-buffer body-start)
                                  (current-buffer)))
                         (save-excursion
                           (goto-char pos)
                           (count-lines (marker-position body-start)
                                        (line-beginning-position)))
                       (save-excursion
                         (goto-char pos)
                         (codex-ide-diff--line-index-at-point)))))
    (codex-ide-diff-goto-source diff-text line-index directory)))

(defun codex-ide-diff-open-buffer (diff-text &optional buffer-name directory)
  "Display DIFF-TEXT in a dedicated `codex-ide-diff-mode' buffer.
When BUFFER-NAME is non-nil, reuse that buffer.
DIRECTORY is used as the buffer's `default-directory' for source jumps.
Return the created buffer."
  (unless (and (stringp diff-text)
               (not (string-empty-p (string-trim diff-text))))
    (user-error "No diff text available"))
  (let ((buffer (if buffer-name
                    (get-buffer-create buffer-name)
                  (generate-new-buffer
                   (codex-ide-diff--generated-buffer-name diff-text)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (when directory
          (setq-local default-directory (file-name-as-directory directory)))
        (erase-buffer)
        (insert (string-trim-right diff-text))
        (insert "\n")
        (codex-ide-diff-mode)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))
    (codex-ide-display-buffer
     buffer
     codex-ide--display-buffer-other-window-pop-up-action)
    buffer))

(defun codex-ide-session-diff--empty-message (source turn-id message)
  "Return empty-state text for SOURCE TURN-ID and MESSAGE."
  (string-join
   (delq nil
         (list (format "# Codex session diff: %s" source)
               (and turn-id (format "# Turn: %s" turn-id))
               (format "# %s" message)))
   "\n"))

(defun codex-ide-session-diff--target-turn-id (source)
  "Return the turn id to render under SOURCE."
  (pcase source
    ('live nil)
    ((or 'transcript 'pinned) codex-ide-session-diff--turn-id)
    (_ nil)))

(defun codex-ide-session-diff--session-buffer-turn-id (session)
  "Return SESSION's turn id at point in its session buffer, if any."
  (when-let* ((session-buffer (and session (codex-ide-session-buffer session))))
    (when (buffer-live-p session-buffer)
      (with-current-buffer session-buffer
        (codex-ide-diff-data-turn-id-at-point
         session
         (point)
         session-buffer)))))

(defun codex-ide-session-diff--render (session source turn-id)
  "Render SESSION diff for SOURCE and TURN-ID in the current buffer."
  (let* ((diff-text
          (if (and (not (eq source 'live))
                   (not turn-id))
              (codex-ide-session-diff--empty-message
               source
               nil
               (pcase source
                 ('transcript "No prompt at transcript position")
                 ('pinned "No pinned turn selected")
                 (_ "No turn selected")))
            (condition-case err
                (codex-ide-diff-data-combined-turn-diff-text session turn-id)
              (user-error
               (codex-ide-session-diff--empty-message
                source
                turn-id
                (error-message-string err))))))
         (directory (and session (codex-ide-session-directory session))))
    (let ((inhibit-read-only t))
      (when directory
        (setq-local default-directory (file-name-as-directory directory)))
      (erase-buffer)
      (insert (string-trim-right diff-text))
      (insert "\n")
      (setq-local buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun codex-ide-session-diff-refresh (&optional buffer)
  "Refresh BUFFER, or the current canonical session diff buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (unless (eq major-mode 'codex-ide-session-diff-mode)
        (user-error "Not in a Codex session diff buffer"))
      (unless (codex-ide-session-p codex-ide-session-diff--session)
        (user-error "No Codex session associated with this diff buffer"))
      (codex-ide-session-diff--render
       codex-ide-session-diff--session
       codex-ide-session-diff-source
       (codex-ide-session-diff--target-turn-id
        codex-ide-session-diff-source)))))

(defun codex-ide-session-diff--buffer-for-session (session)
  "Return the existing canonical diff buffer for SESSION, if any."
  (when-let* ((session-buffer (and session (codex-ide-session-buffer session))))
    (get-buffer (codex-ide-session-diff-buffer-name-for-session
                 session-buffer))))

;;;###autoload
(defun codex-ide-session-diff-open (&optional session)
  "Open or reuse the canonical session diff buffer for SESSION."
  (interactive)
  (let* ((session (or session (codex-ide--session-for-current-project)))
         (session-buffer (and session (codex-ide-session-buffer session))))
    (unless session
      (user-error "No Codex session available"))
    (let ((buffer (get-buffer-create
                   (codex-ide-session-diff-buffer-name-for-session
                    (or session-buffer "*codex*")))))
      (with-current-buffer buffer
        (unless (eq major-mode 'codex-ide-session-diff-mode)
          (codex-ide-session-diff-mode))
        (setq-local codex-ide-session-diff--session session)
        (setq-local codex-ide-session-diff-source
                    (or codex-ide-session-diff-source 'live))
        (codex-ide-session-diff-refresh buffer))
      (codex-ide-display-buffer
       buffer
       codex-ide--display-buffer-other-window-pop-up-action)
      buffer)))

(defun codex-ide-session-diff-follow-live ()
  "Show the latest or currently running turn in this session diff buffer."
  (interactive)
  (setq-local codex-ide-session-diff-source 'live)
  (setq-local codex-ide-session-diff--turn-id nil)
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-follow-transcript (&optional turn-id)
  "Show TURN-ID in this session diff buffer and follow transcript selection."
  (interactive)
  (setq-local codex-ide-session-diff-source 'transcript)
  (setq-local codex-ide-session-diff--turn-id
              (or turn-id
                  (codex-ide-session-diff--session-buffer-turn-id
                   codex-ide-session-diff--session)
                  codex-ide-session-diff--turn-id))
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-pin-current-turn (&optional turn-id)
  "Pin this session diff buffer to TURN-ID."
  (interactive)
  (setq-local codex-ide-session-diff-source 'pinned)
  (setq-local codex-ide-session-diff--turn-id
              (or turn-id
                  (codex-ide-session-diff--session-buffer-turn-id
                   codex-ide-session-diff--session)
                  codex-ide-session-diff--turn-id))
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-transcript-point-changed
    (session turn-id)
  "Notify SESSION's canonical diff buffer that transcript point is at TURN-ID."
  (when-let* ((buffer (codex-ide-session-diff--buffer-for-session session)))
    (with-current-buffer buffer
      (when (and (eq codex-ide-session-diff-source 'transcript)
                 (not (equal codex-ide-session-diff--turn-id turn-id)))
        (setq-local codex-ide-session-diff--turn-id turn-id)
        (codex-ide-session-diff-refresh buffer)))))

(defun codex-ide-session-diff-note-session-updated (session)
  "Refresh SESSION's canonical diff buffer when its source should update."
  (when-let* ((buffer (codex-ide-session-diff--buffer-for-session session)))
    (with-current-buffer buffer
      (when (or (eq codex-ide-session-diff-source 'live)
                (and (eq codex-ide-session-diff-source 'transcript)
                     (equal codex-ide-session-diff--turn-id
                            (codex-ide-session-current-turn-id session))))
        (codex-ide-session-diff-refresh buffer)))))

;;;###autoload
(defun codex-ide-diff-open-combined-turn-buffer (&optional session turn-id)
  "Open the combined diff for SESSION TURN-ID in a standalone diff buffer.
When called interactively with nil TURN-ID, use the last transcript turn at or
above point.  Otherwise, when TURN-ID is nil, prefer the running turn and
otherwise use the most recent completed turn."
  (interactive
   (let ((session (codex-ide--session-for-current-project)))
     (list session
           (codex-ide-diff-data-turn-id-at-point
            session
            (point)
            (current-buffer)))))
  (let* ((session (or session (codex-ide--session-for-current-project)))
         (buffer (and session (codex-ide-session-buffer session)))
         (diff-text (codex-ide-diff-data-combined-turn-diff-text
                     session
                     turn-id)))
    (unless session
      (user-error "No Codex session available"))
    (codex-ide-diff-open-buffer
     diff-text
     (codex-ide-diff-combined-buffer-name-for-session
      (or buffer "*codex*"))
     (and session (codex-ide-session-directory session)))))

(provide 'codex-ide-diff-view)

;;; codex-ide-diff-view.el ends here
