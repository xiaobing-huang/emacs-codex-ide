;;; codex-ide-usage.el --- User-facing usage notifications for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns lightweight user-facing usage notifications.  Protocol
;; handlers update session metadata, header rendering reflects the latest
;; values continuously, and this module optionally surfaces turn-end changes
;; in the transcript.

;;; Code:

(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-header)
(require 'codex-ide-protocol)

(declare-function codex-ide-log-message "codex-ide-log" (session format-string &rest args))
(declare-function codex-ide-transcript-append-metadata-line
                  "codex-ide-transcript"
                  (buffer text &optional face))

(defcustom codex-ide-usage-transcript-notifications t
  "Whether usage changes may be reported in the transcript.
Messages are shown at turn completion and initial snapshots are suppressed."
  :type 'boolean
  :group 'codex-ide)

(defun codex-ide-usage--rate-limits-from-read-response (response)
  "Return the displayable rate-limit snapshot from RESPONSE."
  (alist-get 'rateLimits response))

(defun codex-ide-usage--signed-compact-number (value)
  "Format numeric VALUE with a sign and compact suffix."
  (format "%s%s"
          (if (>= value 0) "+" "-")
          (codex-ide--format-compact-number (abs value))))

(defun codex-ide-usage--format-percent-number (value)
  "Format percentage VALUE without floating-point noise."
  (let* ((rounded (/ (round (* value 100.0)) 100.0))
         (integer (round rounded)))
    (if (>= rounded 1.0)
        (number-to-string integer)
      (format "%.2f" rounded))))

(defun codex-ide-usage--signed-percent (value)
  "Format numeric VALUE as a signed percentage."
  (format "%s%s%%"
          (if (>= value 0) "+" "-")
          (codex-ide-usage--format-percent-number (abs value))))

(defun codex-ide-usage--rate-limit-window-label (window)
  "Return a transcript label for rate limit WINDOW."
  (codex-ide--format-rate-limit-window-label window))

(defun codex-ide-usage--rate-limit-windows (rate-limits)
  "Return displayable rate-limit windows from RATE-LIMITS."
  (delq nil
        (list (alist-get 'primary rate-limits)
              (alist-get 'secondary rate-limits))))

(defun codex-ide-usage--quota-delta-parts (previous current)
  "Return displayable quota delta strings between PREVIOUS and CURRENT snapshots."
  (let ((previous-windows (codex-ide-usage--rate-limit-windows previous))
        (current-windows (codex-ide-usage--rate-limit-windows current))
        parts)
    (while (and previous-windows current-windows)
      (let* ((previous-window (pop previous-windows))
             (current-window (pop current-windows))
             (previous-percent (alist-get 'usedPercent previous-window))
             (current-percent (alist-get 'usedPercent current-window))
             (label (codex-ide-usage--rate-limit-window-label current-window)))
        (when (and (numberp previous-percent)
                   (numberp current-percent)
                   label)
          (let ((delta (- current-percent previous-percent)))
            (when (>= (abs delta) 0.01)
              (push (format "%s/%s"
                            (codex-ide-usage--signed-percent delta)
                            label)
                    parts))))))
    (nreverse parts)))

(defun codex-ide-usage--token-usage-total-tokens (token-usage)
  "Return TOKEN-USAGE's total token count."
  (when-let* ((total (alist-get 'total token-usage)))
    (alist-get 'totalTokens total)))

(defun codex-ide-usage--token-delta-part (previous current)
  "Return a non-zero token usage delta string between PREVIOUS and CURRENT."
  (let ((previous-used (codex-ide-usage--token-usage-total-tokens previous))
        (current-used (codex-ide-usage--token-usage-total-tokens current)))
    (when (and (numberp previous-used)
               (numberp current-used))
      (let ((delta (- current-used previous-used)))
        (unless (= delta 0)
          (format "tokens %s"
                  (codex-ide-usage--signed-compact-number delta)))))))

(defun codex-ide-usage--pending-kinds (session)
  "Return pending usage notification kinds for SESSION."
  (codex-ide--session-metadata-get session :usage-transcript-pending-kinds))

(defun codex-ide-usage--transcript-line (session kinds)
  "Return transcript usage metadata line text for SESSION and KINDS.
Return nil when there is no previous snapshot to compute a delta from."
  (when session
    (let* ((rate-limits
            (codex-ide--session-metadata-get session :rate-limits))
           (token-usage
            (codex-ide--session-metadata-get session :token-usage))
           (previous-rate-limits
            (codex-ide--session-metadata-get
             session
             :usage-transcript-previous-rate-limits))
           (previous-token-usage
            (codex-ide--session-metadata-get
             session
             :usage-transcript-previous-token-usage))
           (quota-deltas
            (and (memq 'quota kinds)
                 (codex-ide-usage--quota-delta-parts
                  previous-rate-limits
                  rate-limits)))
           (token-delta
            (and (memq 'context kinds)
                 (codex-ide-usage--token-delta-part
                  previous-token-usage
                  token-usage)))
           (headline-parts
            (delq nil
                  (list
                   token-delta
                   (and quota-deltas
                        (format "quota %s"
                                (string-join quota-deltas " ")))))))
      (when headline-parts
        (format "Usage updated: %s" (string-join headline-parts "; "))))))

(defun codex-ide-usage--record-transcript-snapshot (session kinds)
  "Record SESSION's current usage snapshots for KINDS."
  (when (memq 'quota kinds)
    (codex-ide--session-metadata-put
     session
     :usage-transcript-previous-rate-limits
     (copy-tree (codex-ide--session-metadata-get session :rate-limits))))
  (when (memq 'context kinds)
    (codex-ide--session-metadata-put
     session
     :usage-transcript-previous-token-usage
     (copy-tree (codex-ide--session-metadata-get session :token-usage)))))

(defun codex-ide-usage-append-transcript-notification (&optional session)
  "Append SESSION's pending usage summary, if any.
This is intended to run at turn completion so usage metadata does not interrupt
agent replies."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((kinds (and session (codex-ide-usage--pending-kinds session)))
         (line (codex-ide-usage--transcript-line session kinds))
         (buffer (and session (codex-ide-session-buffer session))))
    (when session
      (codex-ide--session-metadata-put
       session
       :usage-transcript-pending-kinds
       nil))
    (when (and codex-ide-usage-transcript-notifications
               line
               (buffer-live-p buffer))
      (codex-ide--session-metadata-put
       session
       :usage-transcript-last-line
       line)
      (codex-ide-transcript-append-metadata-line
       buffer
       line
       'codex-ide-usage-notification-face))
    (when kinds
      (codex-ide-usage--record-transcript-snapshot session kinds))))

(defun codex-ide-usage-clear-transcript-notification (&optional session)
  "Clear SESSION's pending transcript usage notification."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when session
    (codex-ide--session-metadata-put
     session
     :usage-transcript-pending-kinds
     nil)))

(defalias 'codex-ide-usage-cancel-transcript-notification
  #'codex-ide-usage-clear-transcript-notification)

(defalias 'codex-ide-usage-cancel-minibuffer-notification
  #'codex-ide-usage-clear-transcript-notification)

(defun codex-ide-usage-note-updated (&optional session kind)
  "Record that SESSION's usage metadata changed.
KIND is either `quota' or `context'.  When KIND is nil, include both."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when (and codex-ide-usage-transcript-notifications session)
    (let* ((pending (codex-ide-usage--pending-kinds session))
           (kinds (if kind
                      (delete-dups (cons kind pending))
                    '(quota context))))
      (codex-ide--session-metadata-put
       session
       :usage-transcript-pending-kinds
       kinds))))

(defun codex-ide-usage-refresh-rate-limits (&optional session)
  "Refresh SESSION's account rate limits from the app-server."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when (and session
             (process-live-p (codex-ide-session-process session)))
    (codex-ide-log-message session "Refreshing account rate limits")
    (codex-ide--request-async
     session
     "account/rateLimits/read"
     nil
     (lambda (result error)
       (if error
           (codex-ide-log-message
            session
            "Account rate limits refresh failed: %S"
            error)
         (if-let* ((rate-limits
                    (codex-ide-usage--rate-limits-from-read-response result)))
             (progn
               (codex-ide--session-metadata-put
                session
                :rate-limits
                rate-limits)
               (codex-ide-log-message
                session
                "Account rate limits refreshed: used=%s%% plan=%s"
                (alist-get 'usedPercent (alist-get 'primary rate-limits))
                (or (alist-get 'planType rate-limits) "unknown"))
               (codex-ide--update-header-line session)
               (codex-ide--schedule-live-usage-refresh session)
               (codex-ide-usage-note-updated session 'quota))
           (codex-ide-log-message
            session
            "Account rate limits refresh returned no rateLimits")))))))

;;;###autoload
(defun codex-ide-refresh-usage (&optional session)
  "Refresh usage information for SESSION from the app-server."
  (interactive)
  (codex-ide-usage-refresh-rate-limits
   (or session (codex-ide--get-default-session-for-current-buffer))))

(provide 'codex-ide-usage)

;;; codex-ide-usage.el ends here
