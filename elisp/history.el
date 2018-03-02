;;
;;	File:		history.el
;;
;;	Description:	Most recently accessed file list function
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: history.el,v $
;;
;;	$Revision: 1.5 $
;;
;;	$Log: history.el,v $
;;	Revision 1.5  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.4  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.3  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

(defcustom separator "-----"
  "*Separator between groups of menu items."
  :type 'string)

(defvar max-history 45 "Max number of history items")
(defvar history-index 0 "Pointer to free history slot")
(defvar history-file-vector "History item slots")

(defcustom history-menu-title "History"
  "*Title of menu under which history of recently opened files are listed."
  :type 'string)

(defvar max-man-history 25 "Max number of manual history items")
(defvar man-index 0 "Pointer to free manual history slot")
(defvar man-item-vector "Manual history item slots")
(defcustom man-menu-title "Man pages"
  "*Title of menu under which history of recently maned pages are listed."
  :type 'string)

(defcustom  man-with-history t
  "*If true, history of manual pages is under the history menu."
  :type 'boolean)

(defcustom history-db (format "%s%s%s" emacs-base system-path-separator "xemacs-history")
  "*File to save history information."
  :type 'string)

(defcustom save-file t
  "*If true, save info of recently opened files\
 and restore the history on next startup."
  :type 'boolean)

(defcustom save-man t
  "*If true, save info of recently maned pages\
 and restore the history on next startup."
  :type 'booleam)

(defun scan-man-history (man-item)
  "Scan the history item slots"
  (let ((index (mod (- man-index 1) max-man-history))
	(res 0))
    (while (and (aref man-item-vector index)
		(not (eq index man-index))
		(< res max-man-history))
      ;;(if (string= man-item (aref man-item-vector index))
      ;;(setq res max-man-history
      (if (equal system-type 'windows-nt)
	  (if (string-case= man-item (aref man-item-vector index))
	      (setq res max-man-history))
	(if (string= man-item (aref man-item-vector index))
	    (setq res max-man-history)))
      (setq index (mod (- index 1) max-man-history)))
    ;;return nil if man-item is in list, non-nil otherwise.
    (cond ((>= res max-man-history) nil)
	  (t t))))

(defun man-menu-path ( &optional leaf)
  "Manual history menu name"
  (let ((path (list man-menu-title)))
    (if man-with-history
	(setq path (append (list history-menu-title) path)))
    (if leaf
	(setq path (append path (list leaf))))
    path))

(defun add-to-man-history ( string )
  "Add one item to manual history menu"
  (interactive)
  (if (scan-man-history string)
      (progn
	(if (aref man-item-vector man-index)
	    ;;delete current menu item
	    (delete-menu-item (man-menu-path (aref man-item-vector man-index))))
	(setf (aref man-item-vector man-index) string)
	(let ((before (aref man-item-vector
			    (mod (- man-index 1) max-man-history))))
	  ;;previous is not null
	  (if before
	      (add-menu-button (man-menu-path)
			       (vector string (list 'open-man-page string) t) before)
	    (add-menu-button (man-menu-path)
			     (vector string (list 'open-man-page string) t) separator)))

	(setq man-index (mod (+ man-index 1) max-man-history)))))

(defun open-man-page ( page )
  "Open manual page"
  (interactive)
  (if page
      (manual-entry page)))

(defun clear-man ()
  "Clear all manual items"
  (interactive)
  (let ((index (mod (- man-index 1) max-man-history))
	(input))
    (if (aref man-item-vector index)
	(if (should-use-dialog-box-p)
	    (make-dialog-box 'question
			     :title "Clear confirmation"
			     :modal t
			     :question "Are you sure to clear all the manual items?"
			     :buttons '(["%_Yes" (clear-man-internal ) t]
					["%_No" (message "Abort") t]))
	  (setq input (read-string "Are you sure to clear all the manual items? (y, n, yes, no) "))
	  (setq input (downcase input))
	  (if (or (string= input "y")
		  (string= input "yes"))
	      (clear-man-internal)
	    (message "Abort"))))))

(defun clear-man-internal ()
  "Clear all manual items"
  (interactive)
  (let ((index 0))
    (while (< index max-man-history)
      (if (aref man-item-vector index)
	  (progn
	    (delete-menu-item (man-menu-path (aref man-item-vector index)))
	    (setf (aref man-item-vector index) nil)))
      (incf index))
    (setq man-index 0)))

(defun man-init (count)
  "Initialize manual history menu"
  (setq man-item-vector (make-vector max-man-history nil))
  (setq man-index 0)

  (if man-with-history
      (list man-menu-title separator '["%_Save" toggle-save-man :style toggle :selected save-man]
	    '["%_Clear..." clear-man t])
    (progn
      (add-submenu nil (list man-menu-title)  history-menu-title)
      (add-menu-button (list man-menu-title) separator)
      (add-menu-button (list man-menu-title) '["Save" toggle-save-man :style toggle :selected save-man])
      (add-menu-button (list man-menu-title) '["Clear..." clear-man t])
      )))

(defun make-history-item-label ( filename index )
  "General history item label"
  (format "%d: %s" index filename))

(defun scan-history (buffer-file-name)
  "Scan history item slots"
  (let ( (index (mod (- history-index 1) max-history))
	 (res 0))
    (while (and (aref history-file-vector index)
		(not (eq index history-index))
		(< res max-history))
      (if (equal system-type 'windows-nt)
	  (if (string-case= buffer-file-name (aref history-file-vector index))
	      (setq res max-history))
	(if (string-equal buffer-file-name (aref history-file-vector index))
	    (setq res max-history)))
      ;;(if (string-equal buffer-file-name (aref history-file-vector index))
      ;;(setq res max-history))
      ;;already in list
      (if (and (< res max-history)
	       (string-equal (basename buffer-file-name)
			     (basename (aref history-file-vector index))))
	  ;;same file name but not same pathname, change menu item label
	  (let ((oldname (aref history-file-vector index)))
	    (incf res)
	    (if (car (find-menu-item current-menubar
				     (list history-menu-title
					   (make-history-item-label (basename oldname) index))))
		(relabel-menu-item (list history-menu-title
					 (make-history-item-label (basename oldname) index))
				   (make-history-item-label oldname index)))))
      (setq index (mod (- index 1) max-history)))
    ;;return nil if pathname buffer-file-name is
    ;;already in list, an integer of zero if
    ;;pathname buffer-file-name isn't in list and
    ;;there's no one with the same filename, otherwise
    ;;a positive integer is return.
    (cond ((>= res max-history) nil)
	  (t res))))

(defun add-to-history ()
  "Add one item to history menu"
  (cond ((eq major-mode 'dired-mode);;dired mode
	 (add-to-history-internal;;remove the trailing '/' or '\'  character if it isn't the root directory
	  (let* ((dir (dired-current-directory))
		 (len (length dir)))
	    (substring  dir 0 (if (> len 1) (decf len) len)))))

	((eq major-mode 'Info-mode) nil);;info mode, no action

	((eq major-mode 'Manual-mode);;manual mode, add to manual history
	 (add-to-man-history
	  (substring (buffer-name) 5)));;remove the preceding "Man: " 5 characters

	(t (let ((path (buffer-file-name)));;other mode
	     (if path
		 (add-to-history-internal path))))))

(defun add-to-history-internal ( file-to-kill )
  "Add one item to history menu"
  (let ((samename (scan-history file-to-kill))
	(displayname file-to-kill))
    (if samename
	;;not in list
	(progn
	  (cond ((eq samename 0)
		 ;;no same filename
		 (if (string= (basename file-to-kill) "")
		     (setq displayname system-path-separator)
		   (setq displayname (basename file-to-kill))))
		;;at lease one with the same filename
		(t (setq displayname file-to-kill)))
	  (if (aref history-file-vector history-index)
	      (progn
		;;delete current menu item
		(delete-menu-item (list history-menu-title
					(make-history-item-label (aref history-file-vector
								       history-index) history-index)))
		(delete-menu-item (list history-menu-title
					(make-history-item-label
					 (basename (aref history-file-vector
							 history-index)) history-index)))))

	  (setf (aref history-file-vector history-index) file-to-kill)
	  (let* ((last (mod (- history-index 1) max-history))
		 (before (aref history-file-vector last)))
	    (if (and before
		     (not (car (find-menu-item current-menubar
					       (list history-menu-title
						     (make-history-item-label before last))))))
		(setq before (basename before)))

	    (if before
		(progn
		  (setq before (make-history-item-label before (mod (- history-index 1) max-history)))
		  (add-menu-button
		   (list history-menu-title)
		   (vector (make-history-item-label displayname history-index)
			   (list 'open-history-item file-to-kill) t)
		   before))
	      (progn
		(add-menu-button
		 (list history-menu-title)
		 (vector (make-history-item-label displayname history-index)
			 (list 'open-history-item file-to-kill) t)
		 separator)
		(if man-with-history
		    (add-menu-button (list history-menu-title) separator
				     (make-history-item-label displayname history-index))))))
	  (setq history-index (mod (+ history-index 1) max-history))))))

(defun open-history-item ( pathname )
  "Open the file in history menu"
  (interactive)
  (cond ((not (file-exists-p pathname))
	 (if (should-use-dialog-box-p)
	     (make-dialog-box 'question
			      :title "Question"
			      :modal t
			      :question (format "File \"%s\" doesn't exist, open it now ?" pathname)
			      :buttons '(["%_Yes" (find-file pathname) t]
					 ["%_No" (message "Abort") t]))
	   (let ((input))
	     (setq input (read-string (format "File \"%s\" doesn't exist, open it now ?" pathname)))
	     (setq input (downcase input))
	     (if (or (string-equal input "y")
		     (string-equal input "yes"))
		 (find-file pathname)
	       (message "Abort")))));;file does't exist

	((file-readable-p pathname)
	 (find-file pathname));;file exists and is readable
	(t (if (should-use-dialog-box-p)
	       (make-dialog-box 'question
				:title "Information"
				:question (format "File \"%s\" exists, but is not readable." pathname)
				:buttons '(["%_Yes" (message "Abort") t]))
	     (message (format "File \"%s\" exists, but is not readable." pathname))))))

(defun clear-history ()
  "Clear all history items"
  (interactive)
  (let ((index (mod (- history-index 1) max-history))
	(input))
    (if (aref history-file-vector index)
	(if (should-use-dialog-box-p)
	    (make-dialog-box 'question
			     :title "Clear confirmation"
			     :question "Are you sure to clear all the history items?"
			     :buttons '(["%_Yes" (clear-history-internal ) t]
					["%_No" (message "Abort") t]))
	  (setq input (read-string "Are you sure to clear all the history items? (y, n, yes, no) "))
	  (setq input (downcase input))
	  (if (or (string= input "y")
		  (string= input "yes"))
	      (clear-history-internal)
	    (message "Abort"))))))

(defun clear-history-internal ()
  "Clear all history items"
  (let ((index 0))
    (while (< index max-history)
      (if (aref history-file-vector index)
	  (progn
	    (delete-menu-item
	     (list history-menu-title
		   (make-history-item-label (aref history-file-vector index) index)))
	    (delete-menu-item
	     (list history-menu-title
		   (make-history-item-label (basename (aref history-file-vector index)) index)))
	    (setf (aref history-file-vector index) nil)))

      (incf index)))
  (setq history-index 0)
  (if man-with-history
      (progn
	(delete-menu-item (list history-menu-title separator))
	(add-menu-button (list history-menu-title) separator "%_Save"))))

(defun save-history ()
  "Save history items to file"
  (let ( (buff)
	 marker)
    (condition-case nil
	(setq buff (find-file-noselect (expand-file-name history-db)))
      (file-error 'error))
    (when buff
      (save-excursion
	(set-buffer buff)
	(erase-buffer)
	(setq marker (point-marker)))

      (let ((standard-output marker))
	(princ ";;Following is generated by XEmacs, please don't edit.\n")

	;;Save list of recently opened files
	(if save-file
	    (let ((index (mod (+ history-index 1) max-history)))
	      (while (not (= index history-index))
		(if (aref history-file-vector index)
		    (princ (format "%s\n" (aref history-file-vector index))))
		(setq index (mod (+ index 1) max-history )))))

	;;Save list of recently maned pages
	(if save-man
	    (let ((index (mod (+ man-index 1) max-man-history)))
	      (while (not (= index man-index))
		(if (aref man-item-vector index)
		    (princ (format "%s\n" (aref man-item-vector index))))
		(setq index (mod (+ index 1) max-man-history)))))

	(set-marker marker nil)
	(save-excursion
	  (set-buffer buff)
	  (condition-case nil
	      (save-buffer)
	    (file-error 'error)))))))

(defun load-history ()
  "Load history from file"
  (let* ( (buff (find-file-noselect  (expand-file-name history-db)))
	  (item-list nil)
	  (temp nil))

    (save-excursion
      (set-buffer buff)
      (setq temp (buffer-substring))
      (kill-buffer buff))

    (while temp
      (if (string-match "\n" temp)
	  (let* ((end (match-end 0))
		 (line (trim (substring temp 0 (- end  1)))))
	    (setq temp (substring temp  end))
	    (if ( and (> (length line) 0)
		      (not (= (aref line 0) ?\n )))
		(setq item-list (append item-list (list line)))))
	(progn
	  (if (> (length temp) 0)
	      (setq item-list (append item-list (list temp))))
	  (setq temp nil))))

    (let* ((len (length item-list))
	   (index 0))
      (while (< index len)
	(let* ((item (nth index item-list))
	       (c (aref item 0)))
	  (cond ((eq c ?\; ) );;comment
		((or (eq (aref item 0) ?\/);;pathname on UNIX
		     (and (eq system-type 'windows-nt)
			  (>= (length item) 2)
			  (eq (aref item 1) ?:)));;pathname on Windows
		 (add-to-history-internal item))
		(t (add-to-man-history item))))
	(incf index)))))

(defun toggle-save-file (&optional arg)
  "Toggle save history setting"
  (interactive "_P")
  (setq save-file (not save-file)))

(defun toggle-save-man (&optional arg)
  "Toggle save manual menu setting"
  (interactive "_P")
  (setq save-man (not save-man)))

(defun history-init (count)
  "Initialize history menu"
  (add-submenu nil (list history-menu-title) "Help")
  (if man-with-history
      (add-submenu (list history-menu-title) (man-init max-man-history))
    (man-init max-man-history))
  (add-menu-button (list history-menu-title) separator)
  (add-menu-button (list history-menu-title) '["Save" toggle-save-file :style toggle :selected save-file])
  (add-menu-button (list history-menu-title) '["Clear..." clear-history t])

  (setq history-file-vector (make-vector max-history nil))
  (load-history))

(history-init max-history)

(add-hook 'kill-buffer-hook 'add-to-history)

(defun save-history-before-exit ( )
  "Save history before XEmacs exit"
  (let* ((bufferlist (buffer-list))
	 (len (length bufferlist))
	 (buffer nil)
	 (index 0))
    (while (< index len)
      (setq buffer (nth index bufferlist)
	    index (1+ index))
      (when (buffer-file-name buffer)
	(add-to-history-internal (buffer-file-name buffer))))
    (save-history)))

(add-hook 'kill-emacs-hook 'save-history-before-exit)

(provide 'history)
