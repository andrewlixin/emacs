;;
;;	File:		finalize.el
;;
;;	Description:	Get rid of "*Compile-Log*" and "*Compile-Log-Show*" buffers when XEmacs starts
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: finalize.el,v $
;;
;;	$Revision: 1.3 $
;;
;;	$Log: finalize.el,v $
;;	Revision 1.3  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.2  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

(when (get-buffer "*Compile-Log*")
  (kill-buffer "*Compile-Log*"))
(when (get-buffer  "*Compile-Log-Show*")
  (kill-buffer "*Compile-Log-Show*"))
