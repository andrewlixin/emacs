;;
;;	File:		sample.el
;;
;;	Description:	Sample customization provided with XEmacs
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2016/10/13 01:33:57 $
;;
;;	$RCSfile: sample.el,v $
;;
;;	$Revision: 1.8 $
;;
;;	$Log: sample.el,v $
;;	Revision 1.8  2016/10/13 01:33:57  xinli
;;	Switch to P4 edit buffer if it exists instead of creating a new one.
;;	
;;	Revision 1.7  2014/08/29 21:08:10  xinli
;;	Make shortcut 'F1' switch to window in non-current frame.
;;
;;	Revision 1.6  2013/06/21 20:04:56  xinli
;;	Define function p4revert and bind to Control-F10.
;;
;;	Revision 1.5  2013/06/12 22:56:04  xinli
;;	Define function p4edit and bind to F10.
;;
;;	Revision 1.4  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.3  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.2  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization			    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable the commands `narrow-to-region' ("C-x n n") and
;; `eval-expression' ("M-ESC", or "ESC ESC").  Both are useful
;; commands, but they can be confusing for a new user, so they're
;; disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;; Make the sequence "C-x w" execute the `what-line' command,
;; which prints the current line number in the echo area.
(global-set-key "\C-xw" 'what-line)

;; set up the function keys to do common tasks to reduce Emacs pinky
;; and such.

;; Make F1 switch to other window in current or other frame
(global-set-key [f1] (lambda () (interactive) (other-window 1 `visible)))

;; Make F2 be `undo'
(global-set-key [f2] 'undo)
;; Make F3 be `find-file'
;; Note: it does not currently work to say
;;   (global-set-key [f3] "\C-x\C-f")
;; The reason is that macros can't do interactive things properly.
;; This is an extremely longstanding bug in Emacs.  Eventually,
;; it will be fixed. (Hopefully ..)
(if (or running-xemacs window-system)
    (global-set-key [f3] 'find-file)
  (global-set-key [f3] 'find-file))

(global-set-key [(control f3)] 'dired)
(when (eq system-type 'windows-nt)
  (global-set-key [(control f3)] 'toolbar-dired))

;; Make F4 be "mark", F5 be "copy", F6 be "paste"
;; Note that you can set a key sequence either to a command or to another
;; key sequence.
;;(global-set-key [f4] 'set-mark-command)
;;(global-set-key [f5] "\M-w")
;;(global-set-key [f6] "\C-y")

;; Shift-F4 is "pop mark off of stack"
(global-set-key [(shift f4)] (lambda () (interactive) (set-mark-command t)))

;; Make F7 be `save-buffer'
(global-set-key [f7] 'save-buffer)

;; Make F8 be "start macro", F9 be "end macro", F10 be "execute macro"
(global-set-key [f8] 'start-kbd-macro)
(global-set-key [f9] 'end-kbd-macro)
;;(global-set-key [f10] 'call-last-kbd-macro)

;; Here's an alternative binding if you don't use keyboard macros:
;; Make F8 be `save-buffer' followed by `delete-window'.
;;(global-set-key [f8] "\C-x\C-s\C-x0")

(cond (running-xemacs
       ;;
       ;; Code for any version of XEmacs/Lucid Emacs goes here
       ;;

       ;; Change the values of some variables.
       ;; (t means true; nil means false.)
       ;;
       ;; Use the "Describe Variable..." option on the "Help" menu
       ;; to find out what these variables mean.
       (setq find-file-use-truenames nil
	     find-file-compare-truenames t
	     minibuffer-confirm-incomplete nil
	     complex-buffers-menu-p nil
	     next-line-add-newlines nil
	     mail-yank-prefix "> "
	     kill-whole-line t
	     )

       ;; When running ispell, consider all 1-3 character words as correct.
       (setq ispell-extra-args '("-W" "3"))

       ;; Change the way the buffer name is displayed in the
       ;; modeline.  The variable for this is called
       ;; 'modeline-buffer-identification but was called
       ;; 'mode-line-buffer-identification in older XEmacsen.
       (if (boundp 'modeline-buffer-identification)
	   ;; Note that if you want to put more than one form in the
	   ;; `THEN' clause of an IF-THEN-ELSE construct, you have to
	   ;; surround the forms with `progn'.  You don't have to
	   ;; do this for the `ELSE' clause.
	   (progn
	     (setq-default modeline-buffer-identification '(" %17b"))
	     (setq modeline-buffer-identification '(" %17b")))
	 (setq-default mode-line-buffer-identification '(" %17b"))
	 (setq mode-line-buffer-identification '(" %17b")))


       (cond ((or (not (fboundp 'device-type))
		  (equal (device-type) 'x))
	      ;;
	      ;; Code which applies only when running emacs under X goes here.
	      ;; (We check whether the function `device-type' exists
	      ;; before using it.  In versions before 19.12, there
	      ;; was no such function.  If it doesn't exist, we
	      ;; simply assume we're running under X -- versions before
	      ;; 19.12 only supported X.)

	      ;; Remove the binding of C-x C-c, which normally exits emacs.
	      ;; It's easy to hit this by mistake, and that can be annoying.
	      ;; Under X, you can always quit with the "Exit Emacs" option on
	      ;; the File menu.
	      (global-set-key "\C-x\C-c" nil)

	      ;; Uncomment this to enable "sticky modifier keys" in 19.13
	      ;; and up.  With sticky modifier keys enabled, you can
	      ;; press and release a modifier key before pressing the
	      ;; key to be modified, like how the ESC key works always.
	      ;; If you hold the modifier key down, however, you still
	      ;; get the standard behavior.  I personally think this
	      ;; is the best thing since sliced bread (and a *major*
	      ;; win when it comes to reducing Emacs pinky), but it's
	      ;; disorienting at first so I'm not enabling it here by
	      ;; default.

	      ;;(setq modifier-keys-are-sticky t)

	      ;; If we're running on display 0, load some nifty sounds that
	      ;; will replace the default beep.  But if we're running on a
	      ;; display other than 0, which probably means my NCD X terminal,
	      ;; which can't play digitized sounds, do two things: reduce the
	      ;; beep volume a bit, and change the pitch of the sound that is
	      ;; made for "no completions."
	      ;;
	      ;; (Note that sampled sounds only work if XEmacs was compiled
	      ;; with sound support, and we're running on the console of a
	      ;; Sparc, HP, or SGI machine, or on a machine which has a
	      ;; NetAudio server; otherwise, you just get the standard beep.)
	      ;;
	      ;; (Note further that changing the pitch and duration of the
	      ;; standard beep only works with some X servers; many servers
	      ;; completely ignore those parameters.)
	      ;;
	      (cond ((string-match ":0" (getenv "DISPLAY"))
		     (load-default-sounds))
		    (t
		     (setq bell-volume 40)
		     (setq sound-alist
			   (append sound-alist '((no-completion :pitch 500))))
		     ))

	      ;; Make `C-x C-m' and `C-x RET' be different (since I tend
	      ;; to type the latter by accident sometimes.)
	      (define-key global-map [(control x) return] nil)

	      ;; Change the pointer used when the mouse is over a modeline
	      (set-glyph-image modeline-pointer-glyph "leftbutton")

	      ;; Change the pointer used during garbage collection.
	      ;;
	      ;; Note that this pointer image is rather large as pointers go,
	      ;; and so it won't work on some X servers (such as the MIT
	      ;; R5 Sun server) because servers may have lamentably small
	      ;; upper limits on pointer size.
	      ;;(if (featurep 'xpm)
	      ;;   (set-glyph-image gc-pointer-glyph
	      ;;	 (expand-file-name "trash.xpm" data-directory)))

	      ;; Here's another way to do that: it first tries to load the
	      ;; pointer once and traps the error, just to see if it's
	      ;; possible to load that pointer on this system; if it is,
	      ;; then it sets gc-pointer-glyph, because we know that
	      ;; will work.  Otherwise, it doesn't change that variable
	      ;; because we know it will just cause some error messages.
	      (if (featurep 'xpm)
		  (let ((file (expand-file-name "recycle.xpm" data-directory)))
		    (if (condition-case error
			    ;; check to make sure we can use the pointer.
			    (make-image-instance file nil
						 '(pointer))
			  (error nil))	; returns nil if an error occurred.
			(set-glyph-image gc-pointer-glyph file))))

	      ;; Change the behavior of mouse button 2 (which is normally
	      ;; bound to `mouse-yank'), so that it inserts the selected text
	      ;; at point (where the text cursor is), instead of at the
	      ;; position clicked.
	      ;;
	      ;; Note that you can find out what a particular key sequence or
	      ;; mouse button does by using the "Describe Key..." option on
	      ;; the Help menu.
	      (setq mouse-yank-at-point t)

	      ;; When editing C code (and Lisp code and the like), I often
	      ;; like to insert tabs into comments and such.  It gets to be
	      ;; a pain to always have to use `C-q TAB', so I set up a more
	      ;; convenient binding.  Note that this does not work in
	      ;; TTY frames.
	      (define-key global-map [(shift tab)] 'self-insert-command)

	      ;; LISPM bindings of Control-Shift-C and Control-Shift-E.
	      ;; Note that "\C-C" means Control-C, not Control-Shift-C.
	      ;; To specify shifted control characters, you must use the
	      ;; more verbose syntax used here.
	      (define-key emacs-lisp-mode-map [(control C)] 'compile-defun)
	      (define-key emacs-lisp-mode-map [(control E)] 'evaldefun)

	      ;; If you like the FSF Emacs binding of button3 (single-click
	      ;; extends the selection, double-click kills the selection),
	      ;; uncomment the following:

	      ;; Under 19.13, the following is enough:
	      ;;(define-key global-map 'button3 'mouse-track-adjust)

	      ;; But under 19.12, you need this:
	      ;;(define-key global-map 'button3
	      ;;    (lambda (event)
	      ;;      (interactive "e")
	      ;;      (let ((default-mouse-track-adjust t))
	      ;;        (mouse-track event))))

	      ;; Under both 19.12 and 19.13, you also need this:
	      ;;(add-hook 'mouse-track-click-hook
	      ;;          (lambda (event count)
	      ;;            (if (or (/= (event-button event) 3)
	      ;;                    (/= count 2))
	      ;;                nil ;; do the normal operation
	      ;;              (kill-region (point) (mark))
	      ;;              t ;; don't do the normal operations.
	      ;;              )))
	      ))

       ))

;;; Older versions of emacs did not have these variables
;;; (emacs-major-version and emacs-minor-version.)
;;; Let's define them if they're not around, since they make
;;; it much easier to conditionalize on the emacs version.

(if (and (not (boundp 'emacs-major-version))
	 (string-match "^[0-9]+" emacs-version))
    (setq emacs-major-version
	  (string-to-int (substring emacs-version
				    (match-beginning 0) (match-end 0)))))
(if (and (not (boundp 'emacs-minor-version))
	 (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version))
    (setq emacs-minor-version
	  (string-to-int (substring emacs-version
				    (match-beginning 1) (match-end 1)))))

;;; Define a function to make it easier to check which version we're
;;; running.

(defun running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
	   (>= emacs-minor-version minor))))

;; Make F10 be `p4 edit'
(defun p4edit ()
  "Call 'p4 edit' on current buffer file"
  (interactive)
  (let ((file (buffer-file-name))
	(p4command "/build/toolchain/lin64/perforce-r13.1/p4")
	(outbuf nil)
	(process nil))
    (when (and file (file-executable-p p4command))
      (setq outbuf (generate-new-buffer "p4edit"))
      (setq process (start-process "p4edit" outbuf p4command "edit" file))
      (while (eq (process-status process) 'run)
	(sleep-for 1))
      (kill-buffer outbuf))))

(global-set-key [f10] 'p4edit)

(defun p4revert ()
  "Call 'p4 revert' on current buffer file"
  (interactive)
  (let ((file (buffer-file-name))
	(p4command "/build/toolchain/lin64/perforce-r13.1/p4")
	(outbuf nil)
	(process nil))
    (when (and file (file-executable-p p4command))
      (setq outbuf (generate-new-buffer "p4revert"))
      (setq process (start-process "p4revert" outbuf p4command "revert" file))
      (while (eq (process-status process) 'run)
	(sleep-for 1))
      (kill-buffer outbuf))))

(global-set-key [(control f10)] 'p4revert)
