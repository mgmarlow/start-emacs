;;; my-package-refresh.el --- Refresh package-archive-contents regularly   -*- lexical-binding: t; -*-

;;; Commentary:

;; The main purpose of this file is to demonstrate how to write lisp
;; "modules" that you can add to your Emacs configuration. Here we
;; define a module, `my-package-refresh.el', that helps keep your
;; package archives up to date. By default Emacs will only re-fetch
;; package information from the internet when
;; `package-refresh-contents' is manually invoked, whereas this code
;; will fetches every `my-package-automatic-refresh-threshold' hours
;; (default 7 days).
;;
;; Kudos to Andrey Listopadov for the original implementation:
;; https://andreyor.st/posts/2022-07-15-refresh-package-contents-automatically/
;;
;; Pro tip: "M-x auto-insert" fills in a bunch of the Emacs Lisp
;; boilerplate for you when defining new Emacs Lisp files.

;;; Code:

(defcustom my-package-last-refresh-date nil
  "Date and time when package lists have been refreshed.

  This variable is then used to check whether
  `package-refresh-contents' call is needed before calling
  `package-install'. The value of this variable is updated when
  `package-refresh-contents' is called.

  See `package-refresh-hour-threshold' for the amount of time needed to
  trigger a refresh."
  :type 'string
  :group 'package)

(defcustom my-package-automatic-refresh-threshold (* 24 7)
  "Amount of hours since last `package-refresh-contents' call
  needed to trigger automatic refresh before calling `package-install'.

Defaults to once a week."
  :type 'number
  :group 'package)

(define-advice package-install (:before (&rest _) my--package-refresh-contents-maybe)
  (when (or (null my-package-last-refresh-date)
            (> (/ (float-time
                   (time-subtract (date-to-time (format-time-string "%Y-%m-%dT%H:%M"))
                                  (date-to-time my-package-last-refresh-date)))
                  3600)
               my-package-automatic-refresh-threshold))
    (package-refresh-contents)))

(define-advice package-refresh-contents (:after (&rest _) my--update-package-refresh-date)
  (customize-save-variable 'my-package-last-refresh-date
                           (format-time-string "%Y-%m-%dT%H:%M")))

(provide 'my-package-refresh)
;;; my-package-refresh.el ends here
