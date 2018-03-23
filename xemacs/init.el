(defvar emacs-base (expand-file-name "emacs" "~"))

(add-to-list 'load-path
	     (expand-file-name "elisp" emacs-base))

;;; Define a variable to indicate whether we're running XEmacs/Lucid Emacs.
;;; (You do not have to defvar a global variable before using it --
;;; you can just call `setq' directly like we do for `emacs-major-version'
;;; below.  It's clearer this way, though.)

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defvar running-emacs (not running-xemacs))

(defun load-ignore-error (lib)
  (if (eq 'error
	  (condition-case nil
	      (load lib)
	    (error 'error)))
      (message (format "Load %s error." lib))
    (message (format "Load %s done." lib))))

(load-ignore-error "utility")

(if running-xemacs
    (load-ignore-error "cron"))

(load-ignore-error (format "%s" (car (last (split-string (symbol-name system-type) "/")))))

(load-ignore-error "sample")

(if running-xemacs
    (load-ignore-error "toolbar"))

(load-ignore-error "cust")

(load-ignore-error "look-feel")

(load-ignore-error "column-marker")

(load-ignore-error "lang")

(load-ignore-error "csemi")

(unless
    (load-ignore-error "history-emacs"))

(load-ignore-error (user-login-name))
(load-ignore-error "machine");; Machine dependent

;;(load-ignore-error "MorganStanley")

(if running-xemacs
    (load-ignore-error "navigator"))

(if running-xemacs
    (load-ignore-error "clip"))

(load-ignore-error "prologue")

(load-ignore-error "finalize")

(load-ignore-error (system-name))

;;(load-ignore-error "VMware")

(load-ignore-error "MemVerge")

;;(calendar)
;;(mark-diary-entries)

(if (locate-file "p4" exec-path)
    (load-ignore-error "p4")
  (message "p4 cannot be found"))
