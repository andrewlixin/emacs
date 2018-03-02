;;
;;	File:		windows-nt.el
;;
;;	Description:	Customization for Microsoft Windows platform
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/07/03 18:10:11 $
;;
;;	$RCSfile: windows-nt.el,v $
;;
;;	$Revision: 1.8 $
;;
;;	$Log: windows-nt.el,v $
;;	Revision 1.8  2013/07/03 18:10:11  xinli
;;	Call replace-in-string in XEmacs, call replace-regexp-in-string in Emacs.
;;	
;;	Revision 1.7  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.6  2012/10/29 23:56:40  xinli
;;	Replace replace-in-string with replace-regexp-in-string.
;;
;;	Revision 1.5  2011/04/01 09:36:33  lixin
;;	Port to emacs 23 on windows.
;;
;;	Revision 1.4  2005/01/06 06:26:24  lixin
;;	Add CVS header
;;

(defvar system-path-separator "\\"
  "Default file separator for common Windows platforms")

;;A list of the directories in which X bitmap files may be found.
;;(setq mswindows-bitmap-file-path (expand-file-name "pic" emacs-base))
(setq x-bitmap-file-path (list (expand-file-name "pic" emacs-base)))

;;Overload find-file and find-file-noselect
;;with pathname transformation under cygwin environment

;;Insert following three lines in gdb.el line 373
;;(if (equal system-type 'windows-nt)
;;    (setq first-colon second-colon
;;	  second-colon (string-match ":" string (1+ second-colon))))

(when (and (locate-file "cygwin1.dll" exec-path) running-xemacs)
  (fset 'find-file-orig (symbol-function 'find-file))
  (fset 'find-file-noselect-orig (symbol-function 'find-file-noselect))

  (defun cygwin-transform-file-name ( filename )
    (let (( index )
	  (path filename))

      (when (string-match "\\([a-zA-Z]:\\\\\\|/\\)cygdrive" path)
	(setq index (match-end 0))
	(setq path (substring filename index))
	(aset path 0 (aref path 1))
	(aset path 1 ?:))
      path))

  (defun cygwin-find-file-noselect ( filename &optional nowarn rawfile )
    (let ((path (cygwin-transform-file-name filename)))
      (find-file-noselect-orig path nowarn rawfile)))

  (defun cygwin-find-file ( filename &optional CODESYS )
    (interactive "FFind file:\nZCoding Ssytem:") ;
    (let ((path (cygwin-transform-file-name filename)))
      (find-file-orig path CODESYS)))

  (fset 'find-file 'cygwin-find-file)
  (fset 'find-file-noselect 'cygwin-find-file-noselect))

(defun dired-remote-build ()
  "Set compile-command properly if remote-build is on"
  (interactive)
  (when (and (boundp 'remote-build) remote-build)
    (let ((dir dired-directory))
      (setq dir (if running-xemacs
		    (replace-in-string dir "\\\\" "/")
		  (replace-regexp-in-string dir "\\\\" "/")))
      (if (and (> (length dir) 2)
	       (eq (aref dir 1) ?:))
	  (setq dir (substring dir 3)))
      (setq compile-command (format "rsh %s cd %s ; make " remote-build dir)))))

(add-hook 'dired-after-readin-hook 'dired-remote-build)

(if (locate-file "gcc.exe" exec-path)
    (setq default-c-compiler "gcc.exe"
	  default-c++-compiler "g++.exe"
	  default-c-compiler-flag "-g -Wall -W"
	  default-c++-compiler-flag "-g -Wall -W"
	  default-make "make")
  (setq default-c-compiler "cl"
	default-c++-compiler "cl"
	default-c-compiler-flag "/Zi"
	default-c++-compiler-flag "/Zi"
	default-make "nmake"))
