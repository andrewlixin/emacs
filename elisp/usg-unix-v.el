;;
;;	File:		usg-unix-v.el
;;
;;	Description:	Customization for SUN Solaris platform
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: usg-unix-v.el,v $
;;
;;	$Revision: 1.5 $
;;
;;	$Log: usg-unix-v.el,v $
;;	Revision 1.5  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.4  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.3  2005/01/06 06:26:24  lixin
;;	Add CVS header
;;

;; For SUN system
(defvar system-path-separator "/"
  "Default file separator for common UNIX platforms")

;;A list of the directories in which X bitmap files may be found.
(setq x-bitmap-file-path (list (expand-file-name "pic" emacs-base)))

(if (locate-file "gcc" exec-path)
    (setq default-c-compiler "gcc"
	  default-c++-compiler "g++"
	  default-c-compiler-flag "-g -Wall -W"
	  default-c++-compiler-flag "-g -Wall -W" )
  (setq default-c-compiler "cc"
	default-c++-compiler "CC"))

(setq default-make "gmake")
