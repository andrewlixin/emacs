;;
;;	File:		cust.el
;;
;;	Description:	XEmacs customization file
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;;	Created on:	2005/01/06 14:03:38 中国标准时间 Thursday January  6
;;
;;	Last modifed on: $Date: 2018/02/28 19:39:27 $
;;
;;	$RCSfile: cust.el,v $
;;
;;	$Revision: 1.67 $
;;
;;	$Log: cust.el,v $
;;	Revision 1.67  2018/02/28 19:39:27  xinli
;;	Add support for MemVerge.
;;
;;	Revision 1.66  2017/10/05 20:03:17  xinli
;;	Add function set-title-prefix to change frame title.
;;
;;	Revision 1.65  2016/11/08 21:05:31  xinli
;;	Add short cut key for copy.
;;
;;	Revision 1.64  2016/09/14 23:58:15  xinli
;;	Make dynamic abbrev expansion copied verbatim always.
;;
;;	Revision 1.63  2016/04/21 16:14:42  xinli
;;	Add menu item 'Reload All Buffers'.
;;
;;	Revision 1.62  2014/12/16 12:17:35  xinli
;;	*** empty log message ***
;;
;;	Revision 1.61  2014/05/30 16:45:19  xinli
;;	Add shortcut for paste command.
;;
;;	Revision 1.60  2014/02/05 23:58:24  xinli
;;	For VMware code base, compile using iscons and set code search path.
;;
;;	Revision 1.59  2013/07/31 18:37:52  xinli
;;	Change regulare expression in function find-same-file-in-branch to
;;	allow digits in branch names.
;;
;;	Revision 1.58  2013/07/26 19:57:45  xinli
;;	Add function find-same-file-in-branch.
;;
;;	Revision 1.57  2013/07/12 20:15:40  xinli
;;	Add function kill-all-buffers and add menu item for it,
;;	for XEMacs and Emacs.
;;
;;	Revision 1.56  2013/07/01 20:14:02  xinli
;;	Start XEmacs gnu server only when running XEmacs, not Emacs.
;;
;;	Revision 1.55  2013/07/01 20:11:31  xinli
;;	Start XEmacs gnu server.
;;
;;	Revision 1.54  2013/06/25 22:55:20  xinli
;;	Fix function delete-trailing-whitespace, which would delete blank lines.
;;
;;	Revision 1.53  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.52  2013/06/12 20:47:19  xinli
;;	Replace buggy whitespace with own function delete-trailing-whitespace for XEmacs
;;
;;	Revision 1.51  2013/06/11 21:11:04  xinli
;;	Add whitespace check in write file hook for XEmacs.
;;
;;	Revision 1.50  2013/05/03 21:42:01  xinli
;;	Move cua-mode to emacs, and define "C-x C-c" to save-buffers-kill-emacs for XEmacs.
;;
;;	Revision 1.49  2013/05/03 20:20:22  xinli
;;	Variable Info-directory-list is only accessible on XEmacs.
;;
;;	Revision 1.48  2013/04/12 17:19:58  xinli
;;	1. Make scratch buffer not killable in emacs
;;	2. Don't confirm when killing a buffer which is not modified.
;;
;;	Revision 1.47  2013/03/28 19:52:45  xinli
;;	Remove trailing whitespace and add a before-save-hook to remove
;;	trailing whitespace.
;;
;;	Revision 1.46  2013/03/26 18:30:23  xinli
;;	Port to emacs 24.3
;;
;;	Revision 1.45  2012/10/18 22:27:31  xinli
;;	*** empty log message ***
;;
;;	Revision 1.44  2012/08/23 21:20:13  xinli
;;	*** empty log message ***
;;
;;	Revision 1.43  2012/01/30 08:27:30  lixin
;;	Define own version of function directory-files to work with both XEmacs and emacs.
;;
;;	Revision 1.42  2012/01/12 00:29:26  lixin
;;	Map key 'alt-/' to word completion for emacs.
;;
;;	Revision 1.41  2011/04/02 01:53:54  lixin
;;	Summary: Add copy/paste key binding for Mac OS X.
;;
;;	Revision 1.40  2011/04/01 09:51:42  lixin
;;	Remove kill-buffer hook to detect whether scratch buffer is killed.
;;
;;	Revision 1.39  2011/04/01 09:36:33  lixin
;;	Port to emacs 23 on windows.
;;
;;	Revision 1.38  2011/03/31 05:56:36  lixin
;;	Add port to Mac OS X 10.6.7 and emacs 23.3
;;
;;	Revision 1.37  2009/03/30 00:24:33  lixin
;;	Correct one typo.
;;
;;	Revision 1.36  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.35  2007/07/09 06:59:13  xinxli
;;	no message
;;
;;	Revision 1.34  2007/07/09 06:52:44  xinxli
;;	no message
;;
;;	Revision 1.33  2006/08/30 03:36:43  gxlu
;;	Add existence check for ${HOME}/opt.
;;
;;	Revision 1.32  2005/07/08 06:34:57  lixin
;;	On Windows platform: feature mule is not provided,
;;	but mode-line has multibyte-status indication.
;;
;;	Revision 1.31  2005/07/08 06:02:23  lixin
;;	Check existence of crypt feature before loading.
;;	Change redefinition of default-modeline-format for non-mule environment.
;;
;;	Revision 1.30  2005/06/17 06:49:41  lixin
;;	Add function toggle-final-newline.
;;
;;	Revision 1.29  2005/01/06 05:57:44  lixin
;;	Add CVS Header and search javac compiler under JAVA_HOME\bin.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization of Specific Packages		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.

(when running-xemacs
  (require 'auto-save)
  (setq auto-save-directory (expand-file-name "autosave" emacs-base)
	auto-save-directory-fallback auto-save-directory
	auto-save-hash-p nil
	ange-ftp-auto-save t
	ange-ftp-auto-save-remotely nil
	;; now that we have auto-save-timeout, let's crank this up
	;; for better interactive response.
	auto-save-interval 2000
	auto-save-timeout 120
	auto-save-visited-file-name t
	))

;;; ********************
;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.

(when (featurep 'completer)
  (require 'completer))

(locate-library "completer")

;;; ********************
;;; Load crypt, which is a package for automatically decoding and reencoding
;;; files by various methods - for example, you can visit a .Z or .gz file,
;;; edit it, and have it automatically re-compressed when you save it again.

(when (featurep 'crypt)
  (require 'crypt)
  (setq crypt-encryption-type 'pgp;; default encryption mechanism
	crypt-confirm-password t;; make sure new passwords are correct
	;; crypt-never-ever-decrypt t
	;; if you don't encrypt anything, set this to
	;; tell it not to assume that "binary" files
	;; are encrypted and require a password.
	))

;;; ********************
;;; Edebug is a source-level debugger for emacs-lisp programs.
;;;
;;(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;;; ********************
;;; fast-lock is a package which speeds up the highlighting of files
;;; by saving information about a font-locked buffer to a file and
;;; loading that information when the file is loaded again.  This
;;; requires a little extra disk space be used.
;;;
;;; Normally fast-lock puts the cache file (the filename appended with
;;; .flc) in the same directory as the file it caches.  You can
;;; specify an alternate directory to use by setting the variable
;;; fast-lock-cache-directories.

;;(require 'fast-lock)
;;(add-hook 'font-lock-mode-hook 'turn-on-fast-lock)

;; XEmacs version of function directory-files has one more parameter than emacs
(defun directory-files-my (DIRECTORY &optional FULL MATCH NOSORT FILES-ONLY)
  (if running-xemacs
      (directory-files DIRECTORY FULL MATCH NOSORT FILES-ONLY)
    (directory-files DIRECTORY FULL MATCH NOSORT)))

(unless (fboundp 'temp-directory)
  (defun temp-directory ()
    "Return the pathname to the directory to use for temporary files."
    (interactive)
    (if (eq system-type 'windows-nt)
	nil
      (format "%s%s%s" "/tmp" system-path-separator (user-login-name)))))

(setq fast-lock-cache-directories
      (list (concat (temp-directory) system-path-separator "fontlock")))

(defun delete-font-lock-cache ()
  "Delete font lock cache files."
  (interactive)
  (when (and (boundp 'fast-lock-cache-directories)
	     (listp fast-lock-cache-directories)
	     (stringp (car fast-lock-cache-directories)))
    (if (not (file-directory-p (car fast-lock-cache-directories)))
	(dired-create-directory (car fast-lock-cache-directories))
      (let* ((dir (car fast-lock-cache-directories))
	     (files (directory-files-my dir t ".*\\.flc$" nil t)))
	(mapc 'delete-file files)))))

(when running-xemacs
  (add-hook 'kill-emacs-hook 'delete-font-lock-cache))

;;; Use jit-lock instead of lazy-lock if it's available.
(cond ((featurep 'jit-lock)
       (require 'jit-lock)
       (setq font-lock-support-mode 'jit-lock-mode)
       (setq jit-lock-stealth-time 16
	     jit-lock-defer-contextually t
	     jit-lock-stealth-nice 0.5)
       (setq-default font-lock-multiline t))

      ((featurep 'lazy-lock)
;;; ********************
;;; lazy-lock is a package which speeds up the highlighting of files
;;; by doing it "on-the-fly" -- only the visible portion of the
;;; buffer is fontified.  The results may not always be quite as
;;; accurate as using full font-lock or fast-lock, but it's *much*
;;; faster.  No more annoying pauses when you load files.

;; I personally don't like "stealth mode" (where lazy-lock starts
;; fontifying in the background if you're idle for 30 seconds)
;; because it takes too long to wake up again on my piddly Sparc 1+.
;;(setq lazy-lock-stealth-time nil)
       (require 'lazy-lock)
       (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
       (setq lazy-lock-minimum-size 10)
       ))

;;; ********************
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>

(cond (running-xemacs
       (require 'func-menu)
       (define-key global-map 'f9 'function-menu)
       (add-hook 'find-file-hooks 'fume-add-menubar-entry)
       (define-key global-map "\C-cl" 'fume-list-functions)
       (define-key global-map "\C-cg" 'fume-prompt-function-goto)

       ;; The Hyperbole information manager package uses (shift button2) and
       ;; (shift button3) to provide context-sensitive mouse keys.  If you
       ;; use this next binding, it will conflict with Hyperbole's setup.
       ;; Choose another mouse key if you use Hyperbole.
       ;;(define-key global-map '(shift button3) 'mouse-function-menu)

       ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
       (setq fume-max-items 25
	     fume-fn-window-position 3
	     fume-auto-position-popup t
	     fume-display-in-modeline-p t
	     fume-menubar-menu-location nil
	     fume-buffer-name "*Function List*"
	     fume-no-prompt-on-valid-default nil)
       ))

;;; ********************
;;; MH is a mail-reading system from the Rand Corporation that relies on a
;;; number of external filter programs (which do not come with emacs.)
;;; Emacs provides a nice front-end onto MH, called "mh-e".
;;;
;; Bindings that let you send or read mail using MH
;;(global-set-key "\C-xm" 'mh-smail)
;;(global-set-key "\C-x4m" 'mh-smail-other-window)
;;(global-set-key "\C-cr" 'mh-rmail)

;; Customization of MH behavior.
(setq mh-delete-yanked-msg-window t)
(setq mh-yank-from-start-of-msg 'body)
(setq mh-summary-height 11)

;; Use lines like the following if your version of MH
;; is in a special place.
;;(setq mh-progs "/usr/dist/pkgs/mh/bin.svr4/")
;;(setq mh-lib "/usr/dist/pkgs/mh/lib.svr4/")

;;; ********************
;;; resize-minibuffer-mode makes the minibuffer automatically
;;; resize as necessary when it's too big to hold its contents.

(when running-xemacs
  (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))

;;; ********************
;;; W3 is a browser for the World Wide Web, and takes advantage of the very
;;; latest redisplay features in XEmacs.  You can access it simply by typing
;;; 'M-x w3'; however, if you're unlucky enough to be on a machine that is
;;; behind a firewall, you will have to do something like this first:

;;(setq w3-use-telnet t
;;      ;;
;;      ;; If the Telnet program you use to access the outside world is
;;      ;; not called "telnet", specify its name like this.
;;      w3-telnet-prog "itelnet"
;;      ;;
;;      ;; If your Telnet program adds lines of junk at the beginning
;;      ;; of the session, specify the number of lines here.
;;      w3-telnet-header-length 4
;;      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		Customization made by Lixin		    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-default-c-compiler ()
  "Switch defult c compiler"
  (interactive)
  (let ((new-compiler nil))
    (setq new-compiler (read-string (format "Current is \"%s\", RET means no change:" default-c-compiler)))
    (when (and (not (null new-compiler))
	       (> (length new-compiler) 0))
      (setq default-c-compiler new-compiler))))

(defun toggle-default-c++-compiler ()
  "Switch defult c++ compiler"
  (interactive)
  (let ((new-compiler nil))
    (setq new-compiler (read-string (format "Current is \"%s\", RET means no change:" default-c++-compiler)))
    (when (and (not (null new-compiler))
	       (> (length new-compiler) 0))
      (setq default-c++-compiler new-compiler))))

(cond ((equal system-type 'windows-nt)
       (if (locate-file "javac.exe" exec-path)
	   (setq default-java-compiler "javac.exe"
		 default-java-compiler-flag "-g")
	 (let* ((java_home (getenv "JAVA_HOME"))
		(javac nil))
	   (when java_home
	     (setq javac (format "%s%s%s%s%s" (getenv "JAVA_HOME")
				 system-path-separator "bin"
				 system-path-separator "javac.exe"))
	     (if (file-exists-p javac)
		 (setq default-java-compiler javac
		       default-java-compiler-flag "-g"))))));; Microsoft Windows NT system
      (t
       (if (executable-find "javac")
	   (setq default-java-compiler "javac"
		 default-java-compiler-flag "-g")
	 (let* ((java_home (getenv "JAVA_HOME"))
		(javac nil))
	   (when java_home
	     (setq javac (format "%s%s%s%s%s" (getenv "JAVA_HOME")
				 system-path-separator "bin"
				 system-path-separator "javac"))
	     (if (file-exists-p javac)
		 (setq default-java-compiler javac
		       default-java-compiler-flag "-g")))))));; Various UNIX platforms

(defun set-default-c-compiler-flag ()
  "Set default c compiler flag"
  (interactive)
  (let ((new-falg nil))
    (setq new-falg (read-string (format "Current is \"%s\", RET means no change:" default-c-compiler-flag)))
    (when (and (not (null new-falg))
	       (> (length new-falg) 0))
      (setq default-c-compiler-flag new-falg))))

(defun set-default-c++-compiler-flag ()
  "Set default c++ compiler flag"
  (interactive)
  (let ((new-falg nil))
    (setq new-falg (read-string (format "Current is \"%s\", RET means no change:" default-c++-compiler-flag)))
    (when (and (not (null new-falg))
	       (> (length new-falg) 0))
      (setq default-c++-compiler-flag new-falg))))

(defun set-default-java-compiler-flag ()
  "Set default java compiler flag"
  (interactive)
  (let ((new-falg nil))
    (setq new-falg (read-string (format "Current is \"%s\", RET means no change:" default-java-compiler-flag)))
    (when (and (not (null new-falg))
	       (> (length new-falg) 0))
      (setq default-java-compiler-flag new-falg))))

;;Load function byte-compile-file, compile
(let ((fun (symbol-function 'byte-compile-file)))
  (when (listp fun)
    (load (nth 1 fun))))

(let ((fun (symbol-function 'compile)))
  (when (listp fun)
    (load (nth 1 fun))))

(defvar compile-function (symbol-function 'compile)
  "Indicate which function is used to compile")

;;(defvar compile-command nil
;;"Indicate which command is used to compile")

(defvar default-compiler nil
  "Defalut compiler for certain programming language")

(defvar default-compiler-flag nil
  "Defalut flags for default-compiler")

(make-variable-buffer-local 'default-compiler)
(make-variable-buffer-local 'default-compiler-flag)

(defvar remote-build-host nil
  "Indicate the host on which remote build process is done")

(defvar remote-build-user nil
  "Indicate the user to login to remote server to buld")

(defvar remote-build-identity nil
  "The identity file used by SSH")

(defun toggle-remote-build ()
  "Toggle remote build feature on and off."
  (interactive)
  (if remote-build-host
      (progn
	(setq remote-build-host nil)
	(message "remote build is turned off"))
    (progn
      (setq remote-build-host (read-string "Enter remote server: "))
      (when (= 0 (length remote-build-host))
	(setq remote-build-host nil)
	(message "remote build is turned off"))
      (setq remote-build-user (read-string "Enter user to build: "))
      )))

(when (equal system-type 'windows-nt)
  (if (locate-file "nmake.exe" exec-path)
      (setq-default compile-command "nmake -k ")))

(make-variable-buffer-local 'compile-function)
(make-variable-buffer-local 'compile-command)

;;(defvar make-notification-user (user-login-name) "Notify user when make is finised")
(defvar make-notification-user nil "Notify user when make is finised")

(defun toggle-make-notification ()
  (interactive)
  (if make-notification-user
      (progn (setq make-notification-user nil)
	     (message "Turn make notification off"))
    (let ((input (read-string (format "Nofity who? (default to %s):" (user-login-name)))))
      (if (= 0 (length input))
	  (setq make-notification-user (user-login-name))
	(setq make-notification-user input)))))

(defun post-compile (proc msg)
  ;; compilation-sentinel is defined in compile.el
  ;; and possibly located in ${xemacs-packages}/lisp/xemacs-base,
  ;; where ${xemacs-packages} is the place xemacs packages are installed.
  "Delete compilation window if it is successful."
  (compilation-sentinel proc msg)
  (when (and (string-match "finished.*" msg)
	     (= 0 (process-exit-status proc)))
    (delete-window (get-buffer-window (find-buffer-by-name "*compilation*")))
    (setq compilation-search-path nil))
  (when (and (boundp 'make-notification-user)
	     (not (null make-notification-user)))
    (pop-up-message (format "make on %s under %s %s" (system-name)
			    (if (and (> (length default-directory) 1)
				     (= (aref default-directory (1- (length default-directory))) ?/))
				(substring default-directory 0 (1- (length default-directory)))
			      default-directory)
			    msg))))

(defun save-and-compile ()
  "Save file and compile."
  (interactive)
  (if (null compile-command)
      (message "Don't know how to build.")
    ;;(save-buffer)

    (apply compile-function (list compile-command))
    (let ((process (get-buffer-process "*compilation*")))
      (when process
	(if local-compilation-search-path
	    (setq compilation-search-path local-compilation-search-path))
	(set-process-sentinel process 'post-compile)))))

(defun make-clean ()
  "Make clean."
  (interactive)
  (let ((old-compile-command compile-command))
    (if (null compile-command)
	(message "Don't know how to clean.")
      (apply compile-function (list (concat default-make " clean")))
      (let ((process (get-buffer-process "*compilation*")))
	(when process
	  (set-process-sentinel process 'post-compile)))
      (setq compile-command old-compile-command))))

(defun kill-current-buffer ()
  "Kill curent buffer."
  (interactive)
  (let ((cancel nil))
    (if (not (buffer-file-name))
	(kill-buffer (buffer-name));; no file associated with buffer
      (if (not (buffer-modified-p))
	  (kill-buffer (buffer-name));; buffer(file) not modified
	(progn
	  (if (should-use-dialog-box-p)
	      (make-dialog-box 'question
			       :title "Kill buffer confirmation"
			       :modal t
			       :question (format "Buffer %s modified, save or not?" (buffer-file-name))
			       :buttons '(["%_Yes" (save-buffer) t]
					  ["%_No" (set-buffer-modified-p nil) t]
					  ["%_Cancel" (setq cancel t) t]))
	    (let ((input (read-string (format "Buffer %s modified, save or not? (y, n, c, yes, no, cancel) "
					      (buffer-file-name)))))
	      (setq input (downcase input))
	      (if (or (string-equal input "y")
		      (string-equal input "yes"))
		  (save-buffer)
		(if (or (string-equal input "n")
			(string-equal input "no"))
		    (set-buffer-modified-p nil)
		  (setq cancel t)))))
	  (unless cancel
	    (kill-buffer (buffer-name))))))))

(defun edit-directory ()
  "List the directory under which current file resides."
  (interactive)
  (when buffer-file-name
    (dired (dirname buffer-file-name))))

(when running-xemacs
  (add-menu-button '("File") ["%_Edit Directory" edit-directory t] "Exit XEmacs")
  (add-menu-button nil ["%_Compile" save-and-compile t]))

(global-set-key [f8] 'save-and-compile)

(if (or running-xemacs window-system)
    (global-set-key [(shift f8)] 'make-clean)
  (global-set-key [f18] 'make-clean))

(when (or running-xemacs window-system)
  (global-set-key [(control f8)]
    '(lambda ()
       (interactive)
       (if (or (null default-compiler)
	       (null default-compiler-flag))
	   (message "No compile command.")
	 (let ((old-compile-command compile-command))
	   (apply compile-function
		  (list (format "%s %s %s"
				default-compiler
				default-compiler-flag
				(basename buffer-file-name))))
	   (let ((process (get-buffer-process "*compilation*")))
	     (when process
	       (set-process-sentinel process 'post-compile)))
	   (setq compile-command old-compile-command))))))

(when running-xemacs
  (add-menu-button nil ["%_Kill" kill-current-buffer t])
  (add-menu-button nil ["%_Switch" switch-to-other-buffer t]))

(global-set-key "\C-l" 'switch-to-other-buffer)

(defvar current-line 1 "current line number")
(make-variable-buffer-local 'current-line)
(defvar top-button-flag 0 "Whether Top or Botton is clicked")
(make-variable-buffer-local 'top-button-flag)

(defun beginning-of-buffer-restore ()
  "Go to beginning of current buffer, or to the place where we last was."
  (interactive)
  (unless (and (= top-button-flag 0)
	       (= (line-number) 1))
    (if (= top-button-flag 1)
	(goto-line current-line)
      (setq current-line (line-number))
      (beginning-of-buffer))
    (setq top-button-flag (mod (1+ top-button-flag) 2))))

(defun end-of-buffer-restore ()
  "Go to the end of current buffer, or to the place where we last was."
  (interactive)
  (unless (and (= top-button-flag 0)
	       (= (line-number) (count-lines (point-min) (point-max))))
    (if (= top-button-flag 1)
	(goto-line current-line)
      (setq current-line (line-number))
      (end-of-buffer))
    (setq top-button-flag (mod (1+ top-button-flag) 2))))

(when running-xemacs
  (add-menu-button nil ["%_Top" beginning-of-buffer-restore t])
  (add-menu-button nil ["%_Bot" end-of-buffer-restore t]))

(cond ((eq system-type 'darwin)
       (global-set-key [C-f10] 'undo)
       (global-set-key [C-f11] 'delete-other-windows)
       (global-set-key [C-H-help] 'kill-ring-save)
       (global-set-key [H-S-help] 'yank))

      ((or running-xemacs window-system)
       (global-set-key [(control f11)] '(lambda ()
					  (interactive)
					  (other-window 1)
					  (delete-other-windows)
					  (kill-current-buffer))))

      ((unless window-system
	 (global-set-key (kbd "\e <f11>") 'other-window)))

      (t (global-set-key [f11] 'delete-other-windows)))

;;(global-set-key [(control shift f11)] '(lambda ()
;;(interactive)
;;(delete-other-windows)
;;(kill-current-buffer))))

(cond ((eq system-type 'darwin)
       (global-set-key [C-f12] 'kill-current-buffer))
      (t (global-set-key [f12] 'kill-current-buffer)))

;; push is used instead of pushnew, because Emacs doesn't support pushnew
(push '("\\.C$" . c++-mode )    auto-mode-alist);; :test 'equal)
(push '("\\.cpp$" . c++-mode )  auto-mode-alist);; :test 'equal)
(push '("\\.cxx$" . c++-mode )  auto-mode-alist);; :test 'equal)
(push '("\\.cc$" . c++-mode )   auto-mode-alist);; :test 'equal)
(push '("\\.CC$" . c++-mode )   auto-mode-alist);; :test 'equal)
(push '("\\.h$" . c++-mode)     auto-mode-alist);; :test 'equal)
(push '("\\.hh$" . c++-mode)    auto-mode-alist);; :test 'equal)

(push '("\\.c$" . c-mode )      auto-mode-alist);; :test 'equal)
(push '("\\.i$" . c-mode )      auto-mode-alist);; :test 'equal)
(push '("\\.l$" . c-mode )      auto-mode-alist);; :test 'equal)
(push '("\\.y$" . c-mode )      auto-mode-alist);; :test 'equal)

;(push '("\\.el$" . lisp-interaction-mode ) auto-mode-alist);; :test 'equal)
(push '("\\.el$" . emacs-lisp-mode) auto-mode-alist)

;;(push '("\\.cs$" . csharp-mode) auto-mode-alist)
(push '("\\.cs$" . cscope-list-entry-mode ) auto-mode-alist)

(if (fboundp 'jde-mode)
    (push '("\\.java$" . jde-mode ) auto-mode-alist);; :test 'equal)
  (push '("\\.java$" . java-mode ) auto-mode-alist));; :test 'equal))

(unless running-xemacs
  (load-ignore-error "markdown-mode")

  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

  (setq markdown-command "pandoc"))

(line-number-mode 1)
(column-number-mode 1)
(setq-default max-lisp-eval-depth 5000)
(setq-default blink-matching-paren-distance 5000)

(setq display-time-mail-file nil)

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-echo-area t)
;;(setq display-time-display-time-background "transparent")
(display-time)

(if running-xemacs
    (if (>= emacs-major-version 20)
	(setq delete-key-deletes-forward t)
      (load-library "delbackspace"))
  (if window-system
      (normal-erase-is-backspace-mode t)))

(when running-xemacs
  (load-default-sounds))
(setq native-sound-only-on-console t)
(setq minibuffer-max-depth nil)

(setq frame-title-prefix "")

(setq frame-icon-title-format (concat (system-name) ":" (user-login-name) "   %b"))
(setq frame-title-format (concat frame-title-prefix (user-login-name) "@" (system-name)  "   %f"))

(defun set-title-prefix ()
  (interactive)
  (let* ((prefix nil))
    (setq prefix (read-string "Enter new prefix: "))
    (setq frame-title-prefix (concat prefix "   "))
    (setq frame-title-format (concat frame-title-prefix
				     (user-login-name) "@" (system-name)  "   %f"))))

(setq dired-listing-switches "-al")
;;Prohibit listing object and back files
;;(if (locate-file "lls" exec-path)
;;    (setq dired-ls-program "lls"))
;;/bin/ls -la $* | egrep -v -e "^*(\.o|~)$" for UNIX
;; ls -la %1% | egrep -v -e "^*(\.o|~)$" for Windows NT

(when (and running-xemacs (>= emacs-major-version 21))
  (global-set-key [kp-subtract] 'kill-primary-selection)
  (global-set-key [kp-add]      'yank-clipboard-selection)
  (global-set-key [kp-enter]    'copy-primary-selection))

(when (and running-xemacs (< emacs-major-version 21))
  (global-set-key 'kp_subtract 'x-kill-primary-selection)
  (global-set-key 'kp_add      'x-yank-clipboard-selection)
  (global-set-key 'kp_enter    'x-copy-primary-selection))

(defun kill-line-forward ()
  "Kill characters before cursor in current line."
  (interactive)
  (kill-line 0))

(when running-xemacs
  (global-set-key "\C-b" 'kill-line-forward))

(defun set-all-font-size (&optional size)
  "Set all fonts to a specified size."
  (interactive)
  (when running-xemacs
    (if (not size)
	(setq size (string-to-number (read-string "Enter font size:"))))
    (cond ((< size 10)
	   (message "Too small"))
	  ((> size 30)
	   (message "Too big"))
	  (t (mapc (lambda ( face )
		     (custom-set-face-font-size
		      face (format "%dpt" size))) (face-list))))))

;;set all fonts to specified size on certain X server
(let ((display (getenv "DISPLAY")))
  (if (and (boundp 'display-font-server)
	   (stringp display-font-server)
	   (boundp 'display-font-size)
	   (numberp display-font-size)
	   (stringp display)
	   (string-match display-font-server display))
      (set-all-font-size display-font-size)))

;;set mode for *scratch* buffer to lisp-interaction-mode
(setq initial-major-mode 'lisp-interaction-mode)

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq font-lock-maximum-size nil)

(if (featurep 'mule)
    (set-language-environment "Chinese-GB"))

;;Prohibit XEmacs from backing up files
(setq backup-inhibited t)

;;Automatic reload updated TAGS and build completion table
(setq tags-auto-read-changed-tag-file t
      tags-build-completion-table t)

;;GDB
(when running-xemacs
  (require 'gdb)
  (if (>= emacs-major-version 21)
      (require 'gdb-highlight)
    (load "gdb-highlight"))

  ;; There is a bug in gdb-highlight in line 897, it should be
  ;; (t (goto-char end) and delete following two lines.
  (require 'gdbsrc)

  (defun gdb-goto-source ()
    "Go to source buffer when debugging with GDB."
    (interactive)
    (pop-to-buffer gdbsrc-last-src-buffer))

  (define-key gdb-mode-map "\C-d" 'gdb-goto-source)

  (defun load-gdb ()
    "Load GDB debugger when editing C/C++ source files."
    (interactive)
    (let* ((program (buffer-file-name))
	   (suffix (suffix-name program))
	   (core "core"))
      (when (and (or (eq major-mode 'c-mode)
		     (eq major-mode 'c++-mode))
		 (and program suffix))
	(setq program (basename program (format ".%s" suffix)))
	(if (equal system-type 'windows-nt)
	    (setq program (format "%s.exe" program)));;append .exe for NT
	(if (file-exists-p ".gdbinit")
	    (if (file-exists-p core)
		(gdb "dummy" core)
	      (gdb "dummy"))
	  (if (and (file-exists-p program)
		   (file-executable-p program))
	      (if (file-exists-p core)
		  (gdb program core)
		(gdb program))
	    (message (format "No executable program for %s" (buffer-file-name))))))))

;;Make face bold and unitalic for gdb-highlight mode
  (require 'gdb-highlight)
  (let ((faces '(gdb-breakpoint-number-face
		 gdb-breakpoint-enabled-face
		 gdb-breakpoint-keep-face
		 gdb-function-name-face
		 gdb-function-location-face
		 gdb-variable-name-face
		 gdb-type-name-face)))
    (while faces
      (let ((face (car faces)))
	(when (boundp face)
	  (make-face-unitalic face)
	  (make-face-bold face)))
      (setq faces (cdr faces))))

  (require 'cc-mode)
  (define-key c-mode-map 'f5 'load-gdb)
  (define-key c-mode-map '(shift f5) 'gdbsrc-mode)

  (define-key c++-mode-map 'f5 'load-gdb)
  (define-key c++-mode-map '(shift f5) 'gdbsrc-mode)

  (require 'dired)
  (define-key dired-mode-map 'f5 'load-gdb))

;;Bookmark
(require 'bookmark)
(if (and (boundp 'bookmark-default-file)
	 (file-exists-p bookmark-default-file))
    (bookmark-load bookmark-default-file))

(unless (functionp 'switch-to-other-buffer)
  (defun switch-to-other-buffer (arg)
    "Switch to the previous buffer.  With a numeric arg, n, switch to the nth
most recent buffer.  With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
    (interactive "p")
    (if (eq arg 0)
	(bury-buffer (current-buffer)))
    (switch-to-buffer
     (if (<= arg 1) (other-buffer (current-buffer))
       (nth (1+ arg) (buffer-list))))))

(unless (functionp 'switch-to-other-buffer)
  (defun switch-to-other-buffer ()
    "Switch to previous buffer."
    (interactive)
    (switch-to-buffer)))
;;(let ((buffer (nth 1 (buffer-list))))
;;(if buffer
;;(switch-to-buffer (buffer-name buffer))))));; not working now

;;Make "*scratch*" buffer unkillable
(when running-xemacs
  (define-error 'KILL-SCRATCH "Buffer *scratch* should not be killed.")
  (add-hook 'kill-buffer-hook
	    '(lambda ()
	       (when (string= (buffer-name) "*scratch*")
		 (switch-to-other-buffer 0)
		 (if (>= emacs-major-version 21)
		     (error 'KILL-SCRATCH)
		   (error (nth 3 (symbol-plist 'KILL-SCRATCH))))))))

(defun set-library ()
  "Set additional libraries to be used with compile command."
  (interactive)
  (let ((lib))
    (if (null compile-command)
	(message "No compile command.")
      (setq lib (read-string (format "%s:" compile-command)))
      (when (not (string= lib ""))
	(setq compile-command (format "%s %s " compile-command lib))))))

;;If true, then dragging out a region
;;with the mouse selects rectangles
(setq mouse-track-rectangle-p nil)

;;Maximum number of entries which may
;;appear on the "Buffers" menu.
;;nil means indefinite.
(setq buffers-menu-max-size nil)

;;Any time you execute a command with execute which has a
;;shorter keybinding, you will be shown the alternate binding
;;after the command executes.
(setq-default teach-extended-commands-p t)

;;How long to pause after displaying a keybinding before
;;after executing.The value is measured in seconds.
(setq-default teach-extended-commands-timeout 60)

(setq-default debug-on-error nil)
(setq-default debug-on-quit nil)
(setq-default lpr-switches nil)
(setq-default ps-print-color-p t)
(setq-default ps-paper-type 'a4)
(setq-default get-frame-for-buffer-default-instance-limit nil)

(if (functionp 'show-temp-buffer-in-current-frame)
    (setq-default temp-buffer-show-function 'show-temp-buffer-in-current-frame))

;;when function `mouse-yank' is called,
;;the the cursor will be moved to the
;;location of the pointer click before
;;text is inserted.
(setq-default mouse-yank-at-point t)

;;`query-replace' should preserve case in replacements.
(setq-default case-replace t)

;; Don't use the specialized Info toolbar.
(setq Info-inhibit-toolbar t)

(when running-xemacs
  (let ((window-height (/ (window-displayed-height) 2 )))
    (if (< window-height 4)
	(setq compilation-window-height 4)
      (setq compilation-window-height (-  window-height 4)))))

;; 'compile' command saves all modified buffers without asking.
(setq compilation-ask-about-save nil)

;;- Non-nil means save the current buffer without asking
(setq mode-compile-always-save-buffer-p t)

;;Always use sound to indicate compilation completion.
(setq compilation-always-signal-completion t)

;;Set this to non-nil for displaying the index in a completion buffer.
(setq imenu-always-use-completion-buffer-p t)

;; Mouse motion over the compilation/grep
;;buffer may initiate parsing of the error messages or grep hits.
(setq compilation-mouse-motion-initiate-parsing t)

(setq compilation-read-command nil)

(when (and running-xemacs (>= emacs-major-version 21))
  (set-specifier vertical-divider-always-visible-p t))

;; Should XEmacs always display vertical dividers between windows.

;;ignore case when searching
(setq-default case-fold-search t)

(setq-default overwrite-mode nil)

(when running-xemacs
  (require 'pending-del)
;;(pending-delete-on nil)
;;Turn on pending delete minor mode unconditionally.
  (turn-on-pending-delete)
;;Turn off pending delete minor mode unconditionally.
;;(turn-off-pending-delete)
  )

(setq-default zmacs-regions t)

;;move menubar "Apps" and "Options" to "Tools",
;;and be careful not to remove more than once
(when running-xemacs
  (let ((apps (delete-menu-item '("Apps")))
	(options (delete-menu-item '("Options"))))
    (if apps
	(add-menu-button '("Tools") apps))
    (if options
	(add-menu-button '("Tools") options))))

(defun open-manual-page ()
  "Browse manual pages in XEmacs."
  (interactive)
  (manual-entry
   ;;(let* ((fmh "-A-Za-z0-9_.:")
   (let* ((fmh "A-Za-z0-9_")
	  (default (save-excursion
		     (buffer-substring
		      (progn
			(re-search-backward "\\sw" nil t)
			(skip-chars-backward fmh) (point))
		      (progn (skip-chars-forward fmh) (point)))))
	  (page (read-string
		 (if (equal default "") "Manual entry: "
		   (concat "Manual entry: (default " default ") ")))))
     (if (equal page "" ) default page))))

(global-set-key "\C-xm" 'open-manual-page)

(setq display-warning-minimum-level 'error)

;;(setq completion-ignored-extensions
;;  '("CVS/" ".o" ".obj" ".elc" "~" ".bin" ".lbin" ".dvi" ".class"))

(when (or (null user-full-name)
	  (= (length user-full-name) 0))
  (setq user-full-name "Li Xin"))

(defun insert-c-prologue ()
  "Generate prologue for C/C++/JAVA source files."
  (let ((f-name (buffer-file-name)))
    (if (or (string-match "\\.cpp$" f-name)
	    (string-match "\\.cxx$" f-name)
	    (string-match "\\.cc$" f-name)
	    (string-match "\\.c$" f-name)
	    (string-match "\\.C$" f-name)
	    (string-match "\\.h$" f-name)
	    (string-match "\\.hh$" f-name)
	    (string-match "\\.java$" f-name))

	(insert (code-prologue f-name)))))

(add-hook 'find-file-not-found-hooks 'insert-c-prologue)

(defun insert-shell-prologue ()
  "Generate prologue for shell scripts"
  (interactive)
  (let ((mode nil)
	(auto-mode-list auto-mode-alist)
	(current-mode nil))
    (while (and auto-mode-list (not mode))
      (setq current-mode (car auto-mode-list)
	    auto-mode-list (cdr auto-mode-list))
      (if (string-match (car current-mode) (buffer-file-name))
	  (setq mode (cdr current-mode))))

    (when (eq mode 'sh-mode)
      (let ((file (buffer-file-name)))
	(insert (shell-prologue file)))))
  nil)

(add-hook 'find-file-not-found-hooks 'insert-shell-prologue)

(defun remove-or-convert-trailing-ctl-M ()
  "Remove ^M's from the file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\^M" nil t)
	(= (following-char) ?\^J) )
    (progn (goto-char (point-min))
	   (perform-replace "\^M" "" nil nil nil)
	   (pop-mark) )
    (message "No transformation.") )
  (progn (goto-char (point-min))
	 (perform-replace "\^M" "\^J" nil nil nil)
	 (pop-mark) )
  )

(defun toggle-mouse-select-rectangle ()
  "Toggle whether mouse dragging selects rectangle or simple start/end regions."
  (interactive)
  (setq mouse-track-rectangle-p (not mouse-track-rectangle-p)))

(defun indent-untabify ()
  "First indent and then untabify current buffer."
  (interactive)
  (save-excursion
    (c-indent)
    (untabify 1 (point-max))))

(defun indent-untabify-file ( file )
  "First indent and then untabify a specified file."
  (interactive)
  (let ((buffer (find-file file)))
    (when buffer
      (indent-untabify)
      (save-buffer)
      (kill-buffer buffer))))

(defun batch-process (dir match fun)
  "Call fun on each file under directory 'dir' that matches the criteria 'match'."
  (interactive)
  (let ((files (directory-files-my dir t match nil t)))
    (mapc fun files)))

;;(batch-process "/home/lixin/src" ".*\\.\\(cpp\\|h\\)" 'indent-untabify-file)

(defun reload-file ()
  (let ((file buffer-file-name)
	(curr-point (point))
	(input))
    (when file
      (when (buffer-modified-p)
	(if (should-use-dialog-box-p)
	    (make-dialog-box 'question
			     :title "File changed"
			     :modal t
			     :question (format "File %s changed, \n%ssave or not." (buffer-file-name) (generate-tab-space))
			     :buttons '(["%_Yes" (save-buffer) t]
					["%_No" (set-buffer-modified-p nil) t]))
	  (setq input (read-string (format "File %s changed, save or not ? (y, n, yes, no) "
					   (buffer-file-name))))
	  (setq input (downcase input))
	  (if (or (string-equal input "y")
		  (string-equal input "yes"))
	      (save-buffer)
	    (set-buffer-modified-p nil))))
      (when (kill-buffer (current-buffer))
	(find-file file)
	(goto-char curr-point)
	(message "File %s reloaded." (buffer-file-name))))))

(defun reload-directory ()
  (let ((dir (dired-current-directory)))
    (when dir
      (if (and (> (length dir) 1)
	       (eq (aref dir (1- (length dir)))
		   (aref system-path-separator 0)))
	  (setq dir (substring dir 0 (1- (length dir)))))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (find-file dir)
      (message "Directory %s reloaded." dir))))

(defun reload ()
  (interactive)
  (cond ((eq major-mode 'dired-mode)
	 (reload-directory))
	((eq major-mode 'Info-mode);; Info mode, no action
	 (message "No need to reload"))
	(t (if (buffer-file-name)
	       (reload-file)
	     (message "No file associated with this buffer.")))))

(if (or running-xemacs window-system)
    (global-set-key [f4] 'reload)
  (global-set-key [f14] 'reload))

(when running-xemacs
  (add-menu-button nil ["%_Reload" reload t]))

;; Excerpted from man.el
(defun get-text-around-cursor ()
  (interactive)
  (save-excursion
    (buffer-substring
     (progn
       (if (not (eobp))
	   (forward-char))
       (if (re-search-backward "\\sw\\|\\s_" nil t)
	   (forward-char))
       (re-search-backward
	"\\(\\sw\\|\\s_\\)([0-9]+[A-Za-z]*\\="
	(point-at-bol) t)
       (skip-syntax-backward "w_")
       (point))
     (progn
       (skip-syntax-forward "w_")
       (re-search-forward "\\=([0-9]+[A-Za-z]*)" nil t)
       (point)))))

(defun grep-at-cursor ()
  (interactive)
  (let ((pattern (get-text-around-cursor)))
    (if (or (null pattern)
	    (< (length pattern) 1))
	(call-interactively (quote grep))
      (let ((grep-command (cons (concat grep-command pattern " *")
				(+ (length grep-command)
				   (length pattern)
				   2))))
	(call-interactively (quote grep))))))

(global-set-key "\C-xg" 'grep-at-cursor)
(global-set-key [kp-tab] '(lambda ()
			    (interactive)
			    (insert "\t")))

(defun should-use-dialog-box-p ()
  (if (and (fboundp 'make-dialog-box)
	   (fboundp 'make-dialog-box-internal)
	   (< (car (load-average)) 0));; 100))
      t
    nil))

(defun for-each-file-in-directory (directory pattern fun)
  "Call fun on each file under a directory, the function is feeded the file name as the sole argument"
  (interactive)
  (if (not (file-directory-p directory))
      (message (format "'%s' is not directory" directory))
    (let ((files (directory-files-my directory t pattern nil t)))
      (if (null files)
	  (message (format "No %s file found under %s" pattern directory))
	(mapcar fun files)))))

(defun untabify-buffer ()
  "Untabify buffer"
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun format-cpp-file-internal (file &optional indent untabify dos2unix)
  "Pretty format CPP/C source or header file, including indenting, untabifying and removing ^M if necessary"
  (interactive)
  (when (find-file file)
    (when indent
      (c-indent))
    (when untabify
      (mark-whole-buffer)
      (untabify-buffer))
    (when dos2unix
      (remove-or-convert-trailing-ctl-M))

    (save-buffer)
    (kill-buffer (current-buffer))))

(defun format-cpp-file (file)
  "Pretty forat CPP/C source or header file, including indenting, untabifying and removing ^M"
  (interactive)
  (format-cpp-file-internal file t t t))

(when running-xemacs
  (require 'speedbar)
  (add-menu-button '("Tools")
		   ["Speedbar" speedbar-frame-mode
		    :style toggle
		    :selected (and (boundp 'speedbar-frame)
				   (frame-live-p speedbar-frame)
				   (frame-visible-p speedbar-frame))]
		   "--"))

;;(define-key global-map [control shift f9] 'fume-list-functions)
;;(define-key global-map [control f9] 'fume-prompt-function-goto)

;; Substitutes a visual signal for the audible bell.
;; 'top-bottom  Flash only the top and bottom lines of the selected frame.
(if (eq system-type 'windows-nt)
    (setq visible-bell nil)
  (setq visible-bell 'top-bottom))

(defun hungry-delete-space-internal (reg)
  "Delete all characters according to regular expression reg after point"
  (save-excursion
    (when (looking-at reg)
      (let ((begin (match-beginning 0 ))
	    (end (match-end 0)))
	(delete-char (- end begin))))))

(defun starving-delete-space ()
  "Delete all white spaces, tabs and carriage returns after point"
  (interactive)
  (hungry-delete-space-internal "\\(\\\n\\| \\|\t\\)+"))

(defun hungry-delete-space ()
  "Delete all whilte spaces and tabs after point on current line"
  (interactive)
  (hungry-delete-space-internal "\\( \\|\t\\)+"))

(defun hungry-backspace-space-internal (reg)
  "Delete all characters according to regular expression reg before point"
  (let* ((point (point))
	 (start (1- point))
	 (done nil))
    (while (and (not done)
		(> start 0))
      (goto-char start)
      (if (looking-at reg)
	  (decf start)
	(setq done t)))
    (when (and done
	       start)
      (goto-char (incf start))
      (delete-char (- point start)))))

(defun starving-backspace-space ()
  "Delete all while spaces and tabs before point on current line"
  (interactive)
  (save-excursion
    (hungry-backspace-space-internal "\\(\\\n\\| \\|\t\\)+")))

(defun hungry-backspace-space ()
  "Delete all while spaces, tabs and carriage returns before point"
  (interactive)
  (save-excursion
    (hungry-backspace-internal-space "\\( \\|\t\\)+")))

;;(define-key global-map [control delete] 'hungry-delete-space)
;;(define-key global-map [control shift delete] 'starving-delete-space)
;;(define-key global-map [control backspace] 'hungry-backspace-space)
;;(define-key global-map [control shift backspace] 'starving-backspace-space)

(when (< emacs-major-version 21)
  (require 'info))

(when (and running-xemacs (file-directory-p (expand-file-name "opt" "~")))
  (let* ((packages (directory-files-my (expand-file-name "opt" "~") t "\\([a-z]\\|[A-Z]\\)" nil "subdir"))
	 (package nil))
    (while packages
      (setq package (car packages)
	    packages (cdr packages))
      (setq package (expand-file-name "info" package))
      (if (file-directory-p package)
	  (setq Info-directory-list (append Info-directory-list
					    (list package)))))))

(when (executable-find "rman");; oracle has an executable named 'rman'
  (require 'man)
  (setq Manual-use-rosetta-man nil))

(defun html-tag ()
  "Read html tag from input, insert <tag></tag>, and point after first '>'."
  (interactive)
  (let* ((tag (read-string "Input HTML tag:"))
	 (length (length tag)))
    (when (> length 0)
      (insert (format "<%s></%s>" tag tag))
      (goto-char (- (point) (+ 3 length))))))

;;(define-key  idl-mode-map [control t] 'html-tag)

;; move line and column before mode and make modeline more compact
(when running-xemacs
  (let ((mule (length default-modeline-format))
	(modeline-format nil))
    (if (eq mule 14)
	(setq mule 1)
      (setq mule 0))
    (setq modeline-format
	  (list
	   (nth (- 2 mule)  default-modeline-format)
	   (nth (- 11 mule) default-modeline-format)
	   (nth (- 12 mule) default-modeline-format)
	   (nth (- 3  mule) default-modeline-format)
	   (nth (- 5 mule)  default-modeline-format)
	   (nth (- 6 mule)  default-modeline-format)
	   (nth (- 7 mule)  default-modeline-format)
	   (nth (- 8 mule)  default-modeline-format)
	   (nth (- 9 mule)  default-modeline-format)
	   (nth (- 10 mule) default-modeline-format)
	   (nth (- 13 mule) default-modeline-format)
	   (nth (- 14 mule) default-modeline-format)))
    (if (eq mule 0)
	(setq default-modeline-format
	      (cons (nth 0 default-modeline-format)
		    (cons (nth 1 default-modeline-format)
			  modeline-format)))
      (setq default-modeline-format (cons (nth 0 default-modeline-format)
					  modeline-format)))))

(setq modeline-scrolling-method 'scrollbar)

;; Always end a file with a newline
(setq require-final-newline t)

(defun toggle-final-newline ()
  "Toggle final newline."
  (interactive)
  (cond ((equal require-final-newline t)
	 (setq require-final-newline "query")
	 (message "A newline will automatically be added at the end of file if necessary when saved."))
	((equal require-final-newline "query")
	 (setq require-final-newline nil)
	 (message "User will be asked whether to add a newline when there isn't one."))
	((equal require-final-newline nil)
	 (setq require-final-newline t)
	 (message "Newline will not be added."))))

(when window-system
  (mwheel-install))

(when running-xemacs
  (let ((domain (system-name))
	(mail-address (user-login-name)))
    (when (and (not (null domain))
	       (search "." domain))
      (setq domain (substring domain (1+ (search "." domain)))))
    (if (and (not (null domain))
	     (search "." domain))
	(setq mail-address (format "%s@%s" mail-address domain)))

    (custom-set-variables
     '(user-mail-address mail-address
			 '(query-user-mail-address nil)))))

(unless (or running-xemacs (eq system-type 'darwin))
  (global-set-key [\e /] 'dabbrev-expand)
  (global-set-key (kbd "\e <home>") 'beginning-of-buffer)
  (global-set-key (kbd "\e <end>") 'end-of-buffer)
  (global-set-key (kbd "\e g") 'goto-line))

;; The expansion is always copied verbatim.
(setq case-replace nil)

;; set After-save-alist to nil to disable running "chmod a+x $f" when file in sh-mode is saved.
(setq After-save-alist nil)

(define-key isearch-mode-map "\M- " 'isearch-complete)

(setq diary-file "~/emacs/diary")

(setq calendar-latitude  31.2477)
(setq calendar-longitude 121.4726)
(setq calendar-location-name "Shanghai")

(global-set-key [f6] '(lambda ()
			(interactive)
			(calendar)
			(mark-diary-entries)))

;;(auto-fill-mode 1)
;;(add-hook 'find-file-hooks '(lambda () (auto-fill-mode 1)))

;; Use in eshell
(defalias 'vi   'find-file)
(defalias 'vim  'find-file)

;;(require 'highlight-current-line)
;;(highlight-current-line-on t)

;; To customize the background color
;;(set-face-background 'highlight-current-line-face "light yellow")

;;(setq semantic-load-turn-everything-on t)
;;(load "semantic-load.el")

;; Following settings are for emacs 23.3.1

(when (not running-xemacs)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end]  'end-of-line)
  (define-key global-map (kbd "s-/") 'dabbrev-expand)
  )
;; Log a note when a TODO task is moved to the DONE state.
(setq org-log-done 'note)

;; Use all files under ${HOME}/org directory to display agenda.
(let ((todo-file-directory (expand-file-name "org" "~/")))
  (if (file-exists-p todo-file-directory)
      (setq org-agenda-files
	    (directory-files-my todo-file-directory t "[a-zA-Z0-9].org"))))

(global-set-key "\C-ca" 'org-agenda)

(custom-set-variables
 '(column-number-mode t)
 '(display-time-mode t)
 '(show-paren-mode t)
 '(scroll-bar-mode (quote right))
 '(size-indication-mode t)
 )

(unless running-xemacs
  (custom-set-variables
   '(cua-mode t nil (cua-base))))

(unless (functionp 'delete-trailing-whitespace)
  (defun delete-trailing-whitespace()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
	(replace-match "" nil nil)))))

;; White space
(if running-xemacs
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun don-not-kill-scratch ()
  (let ((buffer-name (buffer-name (current-buffer))))
    (if (not (string= buffer-name "*scratch*"))
	t
      (message "Don't kill scratch buffer")
      nil)))

(setq kill-buffer-query-functions (append (list 'don-not-kill-scratch) kill-buffer-query-functions))

(global-set-key [(control x) (k)] 'kill-this-buffer)

(when running-xemacs
  (global-set-key [(control x) (control c)] 'save-buffers-kill-emacs))

;; Start XEmacs gnu server and use currently
;; selected frame to display all edited files.
(when running-xemacs
  (setq gnuserv-frame t)
  (gnuserv-start)
  (setq gnuserv-kill-quietly t))

(defun kill-all-buffers ()
  (interactive)
  (mapcar '(lambda (arg)
	     (unless (string= (buffer-name arg) "*scratch*")
	       (if (or (eq (buffer-file-name arg) nil)
		       (not (buffer-modified-p arg)))
	       (kill-buffer arg))))
	  (buffer-list)))

(if running-xemacs
    (add-menu-button (list "Buffers")
		     '["Kill All Buffers" kill-all-buffers t])
  (define-key global-map [menu-bar buffer kill-all-buffers]
    '("Kill All Buffers" . kill-all-buffers)))

(defun reload-all-buffers ()
  (interactive)
  (mapcar '(lambda (arg)
	     (let ((file-name (buffer-file-name arg))
		   (input))
	       (when file-name
		 (set-buffer arg)
		 (while (buffer-modified-p)
		   (setq input (read-string (message "File '%s' has changed, save? (yes or no) "  file-name)))
		   (if (or (string-case= input "yes") (string-case= input "y"))
		       (save-buffer)
		     (set-buffer-modified-p nil)))
		 (kill-buffer arg)
		 (find-file file-name))))
	  (buffer-list)))

(if running-xemacs
    (add-menu-button (list "Buffers")
		     '["Reload All Buffers" reload-all-buffers t])
  (define-key global-map [menu-bar buffer reload-all-buffers]
    '("Reload All Buffers" . reload-all-buffers)))

(defun lock-window ()
  (interactive)
  (let ((status (window-dedicated-p (selected-window))))
    (setq status (not status))
    (set-window-dedicated-p (selected-window) status)
    (message (format "Lock status %s: %s" (buffer-name)
		     (if status "Enabled" "Disabled")))))

(if running-xemacs
    (add-menu-button (list "Buffers")
		     '["Lock Buffer" lock-window t]))

(defun find-same-file-in-branch ()
  (interactive)
  (let ((file-name (buffer-file-name))
	(new-file-name)
	(idx))
    (when file-name
      (setq new-file-name (replace-in-string
			   file-name
			   "\\(/dbc/pa-dbc[0-9]+/[a-z]+/work/\\)[a-z0-9\\-]+\\(/.*\\)" "\\1\\2"
			   nil))
      (setq new-file-name (read-from-minibuffer
			   "Find file: "
			   (if (string= new-file-name file-name)
			       new-file-name
			     (setq idx (string-match "//" new-file-name))
			     (append (list new-file-name) (1+ idx)))))
      (find-file new-file-name))))

(global-set-key [(meta f3)] 'find-same-file-in-branch)

(global-set-key [(meta c)] 'copy-primary-selection)
(global-set-key [(meta z)] 'copy-primary-selection)
(global-set-key [(meta v)] 'yank-clipboard-selection)
(global-set-key [(meta b)] 'kill-primary-selectionection)

(defun resolve-file-name ()
  (when (stringp buffer-file-name)
    (setq buffer-file-name (file-truename buffer-file-name))))

(add-hook 'find-file-hooks 'resolve-file-name)
