;;; codex-ide-usage-tests.el --- Tests for codex-ide usage notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for turn-end user-facing usage notifications.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-core)
(require 'codex-ide-usage)

(ert-deftest codex-ide-usage-note-updated-accumulates-pending-kinds ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (let ((codex-ide-usage-transcript-notifications t))
              (codex-ide-usage-note-updated session 'quota)
              (codex-ide-usage-note-updated session 'context)
              (should (equal
                       (codex-ide--session-metadata-get
                        session
                        :usage-transcript-pending-kinds)
                       '(context quota))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-renders-latest-delta ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (decoded-now (decode-time (current-time)))
         (day (decoded-time-day decoded-now))
         (month (decoded-time-month decoded-now))
         (year (decoded-time-year decoded-now))
         (reset-one (floor (float-time (encode-time 0 25 13 day month year))))
         (reset-two (floor (float-time (encode-time 0 52 13 day month year))))
         appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (let ((codex-ide-usage-transcript-notifications t))
              (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                         (lambda (_buffer text face)
                           (push (list text face) appended))))
                (codex-ide--session-metadata-put
                 session
                 :usage-transcript-previous-rate-limits
                 `((primary . ((usedPercent . 0)
                                (windowDurationMins . 300)
                                (resetsAt . ,reset-one)))))
                (codex-ide--session-metadata-put
                 session
                 :usage-transcript-previous-token-usage
                 '((total . ((totalTokens . 100)))))
                (codex-ide--session-metadata-put
                 session
                 :rate-limits
                 `((primary . ((usedPercent . 1)
                                (windowDurationMins . 300)
                                (resetsAt . ,reset-one)))
                   (planType . "prolite")))
                (codex-ide-usage-note-updated session 'quota)
                (codex-ide--session-metadata-put
                 session
                 :rate-limits
                 `((primary . ((usedPercent . 2)
                                (windowDurationMins . 300)
                                (resetsAt . ,reset-two)))
                   (planType . "prolite")))
                (codex-ide--session-metadata-put
                 session
                 :token-usage
                 '((total . ((totalTokens . 500)))
                   (last . ((totalTokens . 500)))
                   (modelContextWindow . 1000)))
                (codex-ide-usage-note-updated session 'context)
                (codex-ide-usage-append-transcript-notification session)
                (should
                 (equal appended
                        '(("Usage updated: tokens +400; quota +2%/5h"
                           codex-ide-usage-notification-face))))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-suppresses-initial-snapshot ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                         (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(context quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               '((primary . ((usedPercent . 2)
                              (windowDurationMins . 300)))))
              (codex-ide--session-metadata-put
               session
               :token-usage
               '((total . ((totalTokens . 500)))
                 (modelContextWindow . 1000)))
              (codex-ide-usage-append-transcript-notification session)
              (should-not appended)
              (should (equal
                       (codex-ide--session-metadata-get
                        session
                        :usage-transcript-previous-rate-limits)
                       '((primary . ((usedPercent . 2)
                                      (windowDurationMins . 300))))))
              (should (equal
                       (codex-ide--session-metadata-get
                        session
                        :usage-transcript-previous-token-usage)
                       '((total . ((totalTokens . 500)))
                         (modelContextWindow . 1000)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-skips-unchanged-summary ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir))
             (rate-limits
              '((primary . ((usedPercent . 1)
                             (windowDurationMins . 300))))))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                        (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-rate-limits
               rate-limits)
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               rate-limits)
              (codex-ide-usage-append-transcript-notification session)
              (should-not appended))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-renders-minimum-quota-delta ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                         (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-rate-limits
               '((primary . ((usedPercent . 1.0)
                              (windowDurationMins . 300)))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               '((primary . ((usedPercent . 1.01)
                              (windowDurationMins . 300)))))
              (codex-ide-usage-append-transcript-notification session)
              (should
               (equal appended
                      '(("Usage updated: quota +0.01%/5h"
                         codex-ide-usage-notification-face)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-skips-tiny-quota-delta ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                         (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-rate-limits
               '((primary . ((usedPercent . 1.0)
                              (windowDurationMins . 300)))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               '((primary . ((usedPercent . 1.009)
                              (windowDurationMins . 300)))))
              (codex-ide-usage-append-transcript-notification session)
              (should-not appended))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-renders-large-quota-delta-without-decimals ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                         (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-rate-limits
               '((primary . ((usedPercent . 1.0)
                              (windowDurationMins . 300)))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               '((primary . ((usedPercent . 2.25)
                              (windowDurationMins . 300)))))
              (codex-ide-usage-append-transcript-notification session)
              (should
               (equal appended
                      '(("Usage updated: quota +1%/5h"
                         codex-ide-usage-notification-face)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-transcript-notification-renders-combined-deltas ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        appended)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir)))
        (unwind-protect
            (cl-letf (((symbol-function 'codex-ide-transcript-append-metadata-line)
                       (lambda (_buffer text face)
                         (push (list text face) appended))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-rate-limits
               '((primary . ((usedPercent . 8)
                              (windowDurationMins . 300)))
                 (secondary . ((usedPercent . 2)
                               (windowDurationMins . 10080)))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-previous-token-usage
               '((total . ((totalTokens . 1000)))))
              (codex-ide--session-metadata-put
               session
               :usage-transcript-pending-kinds
               '(context quota))
              (codex-ide--session-metadata-put
               session
               :rate-limits
               '((primary . ((usedPercent . 9)
                              (windowDurationMins . 300)))
                 (secondary . ((usedPercent . 2)
                               (windowDurationMins . 10080)))))
              (codex-ide--session-metadata-put
               session
               :token-usage
               '((total . ((totalTokens . 23100)))
                 (last . ((totalTokens . 22100)))
                 (modelContextWindow . 258400)))
              (codex-ide-usage-append-transcript-notification session)
              (should
               (equal appended
                      '(("Usage updated: tokens +22.1k; quota +1%/5h"
                         codex-ide-usage-notification-face)))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-refresh-rate-limits-stores-response-and-refreshes-ui ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        requested-method
        requested-params
        updated
        scheduled
        noted)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir
                       :process 'fake-process)))
        (unwind-protect
            (cl-letf (((symbol-function 'process-live-p)
                       (lambda (_process) t))
                      ((symbol-function 'codex-ide-log-message)
                       (lambda (&rest _) nil))
                      ((symbol-function 'codex-ide--request-async)
                       (lambda (_session method params callback)
                         (setq requested-method method
                               requested-params params)
                         (funcall
                          callback
                          '((rateLimits
                             . ((primary . ((usedPercent . 7)
                                            (windowDurationMins . 300)))
                                (planType . "prolite"))))
                          nil)))
                      ((symbol-function 'codex-ide--update-header-line)
                       (lambda (_session)
                         (setq updated t)))
                      ((symbol-function 'codex-ide--schedule-live-usage-refresh)
                       (lambda (_session)
                         (setq scheduled t)))
                      ((symbol-function 'codex-ide-usage-note-updated)
                       (lambda (_session kind)
                         (setq noted kind))))
              (codex-ide-usage-refresh-rate-limits session)
              (should (equal requested-method "account/rateLimits/read"))
              (should (null requested-params))
              (should (equal
                       (codex-ide--session-metadata-get session :rate-limits)
                       '((primary . ((usedPercent . 7)
                                     (windowDurationMins . 300)))
                         (planType . "prolite"))))
              (should updated)
              (should scheduled)
              (should (eq noted 'quota)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest codex-ide-usage-refresh-rate-limits-ignores-dead-process ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        requested)
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (generate-new-buffer "*codex-usage-test*"))
             (session (make-codex-ide-session
                       :buffer buffer
                       :directory project-dir
                       :process 'fake-process)))
        (unwind-protect
            (cl-letf (((symbol-function 'process-live-p)
                       (lambda (_process) nil))
                      ((symbol-function 'codex-ide--request-async)
                       (lambda (&rest _)
                         (setq requested t))))
              (codex-ide-usage-refresh-rate-limits session)
              (should-not requested))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(provide 'codex-ide-usage-tests)

;;; codex-ide-usage-tests.el ends here
