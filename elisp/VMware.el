;;
;;	File:		VMware.el
;;
;;	Description:	Programming language settings for VMware
;;
;;	Copyright:	Copyright (c) 2014
;;
;;	Author:		Xin Li
;;
;; 	Created on:
;;
;;	Last modifed on: $Date: 2014/02/05 23:54:55 $
;;
;;	$RCSfile: VMware.el,v $
;;
;;	$Revision: 1.2 $
;;
;;	$Log: VMware.el,v $
;;	Revision 1.2  2014/02/05 23:54:55  xinli
;;	Increase font-lock-maximum-size to 10 times of its default.
;;
;;	Revision 1.1  2014/02/03 19:19:01  xinli
;;	Add support for VMware.
;;

(setq font-lock-maximum-size 256000) ; default

(setq font-lock-maximum-size (* 10 font-lock-maximum-size))

(setq default-c-compiler "iscons"
      default-c-compiler-flag ""
      default-c++-compiler "iscons"
      default-c++-compiler-flag ""
      default-make "make")

(defun insert-function-header ()
  (interactive)
  (let ((header "
/*
 *-----------------------------------------------------------------------------
 *
 * Function name --
 *
 *      Function description.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *-----------------------------------------------------------------------------
 */
"))
    (insert-string header)))

(setq-default c-basic-offset 3)
