;;
;;	File:		clip.el
;;
;;	Description:	Clipboard function
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: clip.el,v $
;;
;;	$Revision: 1.5 $
;;
;;	$Log: clip.el,v $
;;	Revision 1.5  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.4  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.3  2005/01/06 06:26:21  lixin
;;	Add CVS header
;;

(defvar max-clip 45 "Max number of clip items")
(defvar clip-index 0 "Pointer to free clip slot")
(defvar clip-item-vector "Clip item slots")
(defvar clip-menu-title "Clip" "Clip menu text")
(defvar max-clip-length 150 "Maximun characters in one clip item")
(defvar max-display-length 25 "Number of characters to display")

(require 'history "history")

(defun make-clip-item-label ( string )
  "Generate clip item label"
  (if (> (length string) (+ 3 max-display-length))
      (format "%s..." (substring string 0 max-display-length))
    string))

(defun scan-clip (clip-item)
  "Scan the clip item slots"
  (let ((index (mod (- clip-index 1) max-clip))
	(res 0))
    (while (and (aref clip-item-vector index)
		(not (eq index clip-index))
		(< res max-clip))
      (if (string= clip-item (aref clip-item-vector index))
	  (setq res max-clip));;already in list
      (setq index (mod (- index 1) max-clip)))
    ;;return nil if clip-item is in list, non-nil otherwise.
    (cond ((>= res max-clip) nil)
	  (t t))))

(defun add-to-clip ( string-to-paste )
  "Add one item"
  (interactive)
  (let ((string string-to-paste))
    (if (and (> (length string) 0)
	     (<= (length string ) max-clip-length)
	     (scan-clip string))
	(progn
	  (if (aref clip-item-vector clip-index)
	      ;;delete current menu item
	      (delete-menu-item
	       (list clip-menu-title
		     ;;(aref clip-item-vector clip-index))))
		     (make-clip-item-label (aref clip-item-vector clip-index)))))
	  (setf (aref clip-item-vector clip-index) string)
	  (let ((before (aref clip-item-vector
			      (mod (- clip-index 1) max-clip))))
	    (if before
		(setq before (make-clip-item-label before))
	      (setq before separator))

	    (add-menu-button (list clip-menu-title)
			     ;;(vector string (list 'insert-clip-item string) t)
			     (vector (make-clip-item-label string) (list 'insert-clip-item string) t)
			     before))
	  (setq clip-index (mod (+ clip-index 1) max-clip))))))

(defun insert-clip-item ( string )
  "Paste from the clip menu to current buffer"
  (if (null string)
      (message "Program error")
    (insert-string string)
    (if (>= emacs-major-version 21)
	(own-clipboard string);;Paste the given string to the X Clipboard.
      (x-own-clipboard string))))

(defun clear-clip ()
  "Clear all clip menu items"
  (interactive)
  (let ((index (mod (- clip-index 1) max-clip))
	(input))
    (if (aref clip-item-vector index)
	(if (should-use-dialog-box-p)
	    (make-dialog-box 'question
			     :title "Clear confirmation"
			     :modal t
			     :question "Are you sure to clear all the clipboard items ?"
			     :buttons '(["%_Yes" (clear-clip-1 ) t]
					["%_No" (message "Abort") t]))
	  (setq input (read-string "Are you sure to clear all the clipboard itesm? (y, n, yes, no) "))
	  (setq input (downcase input))
	  (if (or (string-equal input "y")
		  (string-equal input "yes"))
	      (clear-clip-internal)
	    (message "Abort"))))))

(defun clear-clip-internal ()
  "Clear all clip menu items"
  (interactive)
  (let ((index 0))
    (while (< index max-clip)
      (when (aref clip-item-vector index)
	;;(delete-menu-item (list clip-menu-title (aref clip-item-vector index)))
	(delete-menu-item (list clip-menu-title (make-clip-item-label (aref clip-item-vector index))))
	(setf (aref clip-item-vector index) nil))
      (incf index))
    (setq clip-index 0)))

(defun clip-init (count)
  "Initialize clip data structures"
  (add-submenu nil (list clip-menu-title)
	       (if man-with-history
		   history-menu-title
		 man-menu-title))
  (add-menu-button (list clip-menu-title) separator)
  (add-menu-button (list clip-menu-title) '["%_Clear..." clear-clip t])
  (setq clip-item-vector (make-vector max-clip nil)))

(defun my-toolbar-cut-function  ()
  "Customize cut function in toolbar"
  (interactive)
  (when (>= emacs-major-version 21)
    (kill-primary-selection)
    (if (<= emacs-minor-version 1)
	(add-to-clip (x-get-clipboard))
      (add-to-clip (get-clipboard))))
  (when (< emacs-major-version 21)
    (x-kill-primary-selection)
    (add-to-clip (x-get-clipboard))))

(defun my-toolbar-copy-function ()
  "Customize copy function in toolbar"
  (interactive)
  (when (>= emacs-major-version 21)
    (copy-primary-selection)
    (if (<= emacs-minor-version 1)
	(add-to-clip (x-get-clipboard))
    (add-to-clip (get-clipboard))))
  (when (< emacs-major-version 21)
    (x-copy-primary-selection)
    (add-to-clip (x-get-clipboard))))

(setq toolbar-copy-function 'my-toolbar-copy-function)
(setq toolbar-cut-function 'my-toolbar-cut-function)

(clip-init max-clip)

;;(clear-clip)
(provide 'clip)

