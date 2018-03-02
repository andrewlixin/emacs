;;
;;	File:		machine.el
;;
;;	Description:	Machine dependent customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: machine.el,v $
;;
;;	$Revision: 1.4 $
;;
;;	$Log: machine.el,v $
;;	Revision 1.4  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.3  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

(let ((system-name (system-name)))
  (cond ((string-case= system-name "lixin") ;; Normally PC
	 (set-all-font-size 14))

	((string-case= system-name "dnm-redhat")
	 (set-all-font-size 16))

	((string-case= system-name "noah")
	 (set-all-font-size 16))

	((string-case= system-name "panda.gxlu.com.cn")
	 (set-all-font-size 16))))
