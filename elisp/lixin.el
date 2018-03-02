;;
;;	File:		lixin.el
;;
;;	Description:	Personal customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: lixin.el,v $
;;
;;	$Revision: 1.6 $
;;
;;	$Log: lixin.el,v $
;;	Revision 1.6  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.5  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

(when (string-case= (system-name) (user-login-name)) ;; Normally PC
    (set-all-font-size 14))

(let ((cygwin-path (locate-file "cygwin1.dll" exec-path))
      (include "")
      (index 0))
  (when cygwin-path
    (setq cygwin-path (split-string cygwin-path "\\\\")
	  index (- (length cygwin-path) 3))
    (while (>= index 0)
      (setq include (format "%s\\%s" (nth index cygwin-path) include))
      (setq index (1- index)))
    ))

(defun clear-temporary-file-and-directory ()
  "Remove files and subdirectories in temporary directory."
  (interactive)
  (let ((temp (temp-directory)))
    (when temp
      (remove-directory-recursive temp)
      (unless (file-directory-p temp)
	(dired-create-directory temp)))))

(clear-temporary-file-and-directory)
