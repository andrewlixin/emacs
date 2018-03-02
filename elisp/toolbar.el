;;
;;	File:		toolbar.el
;;
;;	Description:	Toolbar customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: toolbar.el,v $
;;
;;	$Revision: 1.7 $
;;
;;	$Log: toolbar.el,v $
;;	Revision 1.7  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.6  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.5  2005/01/06 06:26:24  lixin
;;	Add CVS header
;;

(defun my-toolbar-print ()
  "Confirm to print"
  (interactive)
  (let ((prompt (format "Are you sure to print buffer %s ?" (buffer-name)))
	(input))
    (if (should-use-dialog-box-p)
	(make-dialog-box 'question
			 :title "Print confirmation"
			 :modal t
			 :question prompt
			 :buttons '(["%_Yes" (lpr-buffer) t]
				    ["%_No" (message "Quit") t]))
      (setq input (read-string (format "%s (y, n, yes, no) " prompt)))
      (setq input (downcase input))
      (if (or (string-equal input "y")
	      (string-equal input "yes"))
	  (lpr-buffer)
	(message "Quit")))))

(defun my-toolbar-switch ()
  (interactive)
  (switch-to-buffer
   (buffer-name
    (nth 1 (buffer-list) ))))

(defun toolbar-switch ()
  (interactive)
  (call-interactively 'my-toolbar-switch))

(defun toolbar-print ()
  (interactive)
  (call-interactively 'my-toolbar-print))

(if (fboundp 'mail)
    (defun toolbar-mail ()
      (interactive)
      (mail))
  (defun toolbar-mail ()
    (interactive)
    (if (should-use-dialog-box-p)
	(make-dialog-box 'question
			 :title "Info"
			 :modal t
			 :question "No appropriate mail function"
			 :buttons '(["%_OK" (nop) t]))
      (error "No appropriate mail function to use."))))

(defun xpm-file ( file-name )
  (expand-file-name file-name (expand-file-name "pic" emacs-base)))

(defvar switch::toolbar-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list (xpm-file "switch.xpm")))
  "A `switch' icon set")

(defvar delf::toolbar-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list (xpm-file "del.xpm")))
  "A `delf' icon set")

(defvar shell::toolbar-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list (xpm-file "shell.xpm")))
  "A `shell' icon set")

(defvar elsex::toolbar-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list (xpm-file "recycle.xpm")))
  "A `elsex' icon set")

(defun toolbar-shell ()
  (interactive)
  ;;(call-interactively 'shell)
  (eshell)
  )

(defun defile ()
  "Function used in the user-added menu 'Delete File'
Delete the file in current buffer and kill the buffer
Prompt for assurance form user"
  (interactive)
  (cond ((null (buffer-file-name))
	 (message "No file associated with buffer %s" (buffer-name)))
	(t (if (should-use-dialog-box-p)
	       (make-dialog-box 'question
				:title "File delete confirmation"
				:modal t
				:question (format "Are you sure to delete file\n%s%s ?"
						  (generate-tab-space)
						  buffer-file-name)
				:buttons '(["%_Yes" (progn (delete-file buffer-file-name)
							   (kill-buffer (buffer-name))) t]
					   ["%_No" (message "Quit") t]))


	     (let ((input))
	       (setq input (read-string (format "Are you sure to delete file %s? (y, n, yes, no) " buffer-file-name)))
	       (setq input (downcase input))
	       (if (or (string-equal input "n")
		       (string-equal input "no"))
		   (message "Quit")
		 (delete-file buffer-file-name)
		 (kill-buffer (buffer-name))))))))


(defvar switch::toolbar
  (if (featurep 'toolbar)
      '([switch::toolbar-icon
	 toolbar-switch
	 t
	 "Switch buffer"])))

(defvar delf::toolbar
  (if (featurep 'toolbar)
      '([delf::toolbar-icon
	 defile
	 t
	 "Delete file in current buffer"])))

(defvar shell::toolbar
  (if (featurep 'toolbar)
      '([shell::toolbar-icon
	 toolbar-shell
	 t
	 "Shell"])))

(defvar elsex::toolbar
  (if (featurep 'toolbar)
      '([elsex::toolbar-icon
	 eval-print-last-sexp
	 t
	 "Evaluate lastest expression"]
	)))

(setq initial-toolbar-spec
      (append initial-toolbar-spec
	      '([elsex::toolbar-icon eval-print-last-sexp t
	     ;;'([elsex::toolbar-icon eval-last-sexp t
				     "Evaluate lastest expression"]
		[shell::toolbar-icon toolbar-shell t "Shell"]
		[delf::toolbar-icon defile t  "Delete file in buffer"]
		)))

(set-specifier default-toolbar initial-toolbar-spec)

(load "sun-eos-toolbar")
