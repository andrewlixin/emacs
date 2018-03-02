;;
;;	File:		navigator.el
;;
;;	Description:	Navigation toolbar customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: navigator.el,v $
;;
;;	$Revision: 1.3 $
;;
;;	$Log: navigator.el,v $
;;	Revision 1.3  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.2  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

(defvar navigate-history nil
  "List recording navigation history, including file and point")
(defvar navigate-history-index 0
  "Current index pointed to navigate-history")

(defun navigator-backward ()
  "Go to file and point backward"
  (interactive)
  (when (and navigate-history
	     (>= navigate-history-index 0))
    (if (>= navigate-history-index (length navigate-history))
	(decf navigate-history-index))
    (let* ((navigate (nth navigate-history-index navigate-history))
	   (file (car navigate))
	   (point (cadr navigate))
	   (current-file (buffer-file-name))
	   (current-point (point)))

      (when (and current-file current-point
		 (string= current-file file)
		 (= current-point point))
	(decf navigate-history-index)
	(when (>= navigate-history-index 0)
	  (setq navigate (nth navigate-history-index navigate-history)
		file (car navigate)
		point (cadr navigate))))

      (find-file file)
      (goto-char point))))

(defun navigator-forward ()
  "Go to file and point forward"
  (interactive)
  (when (and navigate-history-index
	     (< navigate-history-index (length navigate-history)))
    (if (<= navigate-history-index -1)
	(incf navigate-history-index))
    (let* ((navigate (nth navigate-history-index navigate-history))
	   (file (car navigate))
	   (point (cadr navigate))
	   (current-file (buffer-file-name))
	   (current-point (point)))

      (when (and current-file current-point
		 (string= current-file file)
		 (= current-point point))
	(incf navigate-history-index)
	(when (< navigate-history-index (length navigate-history))
	  (setq navigate (nth navigate-history-index navigate-history)
		file (car navigate)
		point (cadr navigate))))

      (find-file file)
      (goto-char point))))

(defun clear-navigator ()
  "Clear all navigation information"
  (interactive)
  (setq navigate-history-index 0)
  (setq navigate-history nil))

(defun navigator-add ()
  "Add current file and point to navigation history"
  (interactive)
  (let ((file (buffer-file-name))
	(point (point)))
    (when (and file (> point 0))
      ;;(pushnew (list file point) navigate-history :test 'equal)
      (if navigate-history
	  (setq navigate-history (append navigate-history (list (list file point))))
	(pushnew (list file point) navigate-history))
      (enable-menu-item '("<<<"))
      (enable-menu-item '(">>>")))))

(add-menu-button nil ["<<<" navigator-backward t])
(add-menu-button nil [".%_.." navigator-add t])
(add-menu-button nil [">>>" navigator-forward t])
(disable-menu-item '("<<<"))
(disable-menu-item '(">>>"))
