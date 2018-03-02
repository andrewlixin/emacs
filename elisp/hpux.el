;;
;;	File:		hpux.el
;;
;;	Description:	Customization for HP-UX platform
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: hpux.el,v $
;;
;;	$Revision: 1.7 $
;;
;;	$Log: hpux.el,v $
;;	Revision 1.7  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.6  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

;; For HP-UX system
(defvar system-path-separator "/"
  "Default file separator for common UNIX platforms")

;;A list of the directories in which X bitmap files may be found.
(setq x-bitmap-file-path (list (expand-file-name "pic" emacs-base)))

(if (locate-file "aCC" exec-path)
    (setq default-c-compiler "cc"
	  default-c++-compiler "aCC"
	  default-c-compiler-flag "-Aa -g +DAportable -z +w1"
	  default-c++-compiler-flag "-Aa -g0 +DAportable -z -ext +d")
  (setq default-c-compiler "gcc"
	default-c++-compiler "g++"
	default-c-compiler-flag "-g -Wall -W"
	default-c++-compiler-flag "-g -Wall -W" ))

(setq default-make "make")
