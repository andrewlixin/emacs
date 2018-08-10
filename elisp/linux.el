;;
;;	File:		linux.el
;;
;;	Description:	Customization for Linux platform
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: linux.el,v $
;;
;;	$Revision: 1.4 $
;;
;;	$Log: linux.el,v $
;;	Revision 1.4  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.3  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

;; For Linux system
(defvar system-path-separator "/"
  "Default file separator for common UNIX platforms")

;;A list of the directories in which X bitmap files may be found.
(setq x-bitmap-file-path (list (expand-file-name "pic" emacs-base)))

(setq default-c-compiler "gcc"
      default-c-compiler-flag "-g -Wall -fno-diagnostics-color"
      default-c++-compiler "g++"
      default-c++-compiler-flag "-g -Wall -std=c++11 -pthread -fno-diagnostics-color"
      default-make "make")

(require 'ansi-color)
(defun my-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)
