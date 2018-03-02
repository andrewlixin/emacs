;;
;;	File:		csemi.el
;;
;;	Description:	c-mode map function
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/10/23 19:09:28 $
;;
;;	$RCSfile: csemi.el,v $
;;
;;	$Revision: 1.15 $
;;
;;	$Log: csemi.el,v $
;;	Revision 1.15  2013/10/23 19:09:28  xinli
;;	Disable running inside Emacs.
;;	
;;	Revision 1.14  2013/08/30 17:31:58  xinli
;;	*** empty log message ***
;;
;;	Revision 1.13  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.12  2013/03/28 17:33:21  xinli
;;	Replace last-command-char with last-command-event.
;;
;;	Revision 1.11  2011/04/01 09:36:33  lixin
;;	Port to emacs 23 on windows.
;;
;;	Revision 1.10  2010/01/22 13:11:19  lixin
;;	Remove space after close parenthsis.
;;
;;	Revision 1.9  2010/01/21 04:16:11  lixin
;;	Change for EMC-VMWare joint project Aurora.
;;
;;	Revision 1.8  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.7  2005/07/08 06:19:15  lixin
;;	Change backward-delete-char to delete-backward-char, and supply first argument for early XEmacs version.
;;	Supply first argument to delete-char for early XEmacs version.
;;
;;	Revision 1.6  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

(defun c-electric-semi&comma (arg)
  "Insert a comma or semicolon.
When the auto-newline feature is turned on, as evidenced by the \"/a\"
or \"/ah\" string on the mode line, a newline might be inserted.  See
the variable `c-hanging-semi&comma-criteria' for how newline insertion
is determined.

When semicolon is inserted, the line is re-indented unless a numeric
arg is supplied, point is inside a literal, or there are
non-whitespace characters on the line following the semicolon, or
`c-syntactic-indentation' is nil.

Based on the value of `c-cleanup-list', this function cleans up commas
following brace lists and semicolons following defuns."
  (interactive "*P")
  (let* ((lim (c-most-enclosing-brace (c-parse-state)))
	 (literal (c-in-literal lim))
	 (here (point)))
    ;; shut this up
    ;;(c-echo-syntactic-information-p nil))
    (if (or literal
	    arg
	    (not (looking-at "[ \t]*$")))
	(self-insert-command (prefix-numeric-value arg))

      ;; do some special stuff with the character
      (if (not literal)
	  (progn
	    (beginning-of-line)
	    (if (looking-at "[ \t]*$")
		(progn
		  (forward-char -1)
		  (skip-chars-backward " \t\n")
		  (if (not (eq (char-before) ?\;))
		      (self-insert-command (prefix-numeric-value arg)))
		  (forward-char (- here (point))))
	      (forward-char (- here (point)))
	      (self-insert-command (prefix-numeric-value arg)))))
      (c-indent-line)
      ;;(self-insert-command (prefix-numeric-value arg))
      ;; do all cleanups and newline insertions if c-auto-newline is
      ;; turned on
      (if (not c-auto-newline)
	  ;;(if c-syntactic-indentation
	  ;;(c-indent-line))
	  (not t)
	;; clean ups
	(let ((pos (- (point-max) (point))))
	  (if (and (or (and
			(eq last-command-event ?,)
			(memq 'list-close-comma c-cleanup-list))
		       (and
			(eq last-command-event ?\;)
			(memq 'defun-close-semi c-cleanup-list)))
		   (progn
		     (forward-char -1)
		     (skip-chars-backward " \t\n")
		     (eq (char-before) ?}))
		   ;; make sure matching open brace isn't in a comment
		   (not (c-in-literal lim)))
	      (delete-region (point) here))
	  (goto-char (- (point-max) pos)))
	;; re-indent line
	;;(if c-syntactic-indentation
	;;(c-indent-line))
	;; check to see if a newline should be added
	(let ((criteria c-hanging-semi&comma-criteria)
	      answer add-newline-p)
	  (while criteria
	    (setq answer (funcall (car criteria)))
	    ;; only nil value means continue checking
	    (if (not answer)
		(setq criteria (cdr criteria))
	      (setq criteria nil)
	      ;; only 'stop specifically says do not add a newline
	      (setq add-newline-p (not (eq answer 'stop)))
	      ))
	  (if add-newline-p
	      (progn (newline)
		     (c-indent-line)))
	  )))))

(defun c-semi&comma-newline ()
  "Controls newline insertion after semicolons.
    If a comma was inserted, no determination is made."
  (if (not (eq last-command-event ?\;))
      nil;; continue checking
    (if ( and (eq (char-before) ?\;)
	      (condition-case nil
		  (save-excursion
		    (up-list -1)
		    (not (eq (char-after) ?\()))
		(error t)))
	t
      'stop)))

(defun c-assign ()
  "Add white space around '=' and overloaded op= operators."
  (interactive)
  (let ((cbefore (char-before (point)))
	(cbeforetwo (char-before (1- (point))))
	(list "/*+-%^~&|<>=!."));; . is for PHP

    (if (= (point) 1)
	(insert-string "=");;beginning of buffer
      (when (and (= cbefore ?\ )
		 (not (null cbeforetwo))
		 (search (format "%c" cbeforetwo) list))
	(delete-backward-char 1)
	(setq cbefore (char-before (point))
	      cbeforetwo (char-before (1- (point)))))

      (if (search (format "%c" cbefore) list)
	  (if (and (not (null  cbeforetwo))
		   (search (format "%c" cbeforetwo) " \t"))
	      (insert-string "= ")
	    (goto-char (1- (point)))

	    (when (equal cbefore cbeforetwo);; // deal with case like <<=, >>=
	      (goto-char (1- (point))))

	    (insert-string " ")

	    (when (equal cbefore cbeforetwo);; // deal with case like <<=, >>=
	      (goto-char (1+ (point))))

	    (goto-char (1+ (point)))
	    (insert-string "= "))
	(insert-string " = ")))))

(defun c-add ()
  "Add whitespace around '+' "
  (interactive)
  (let ((cbefore (char-before (point)))
	(cbeforetwo (char-before (1- (point))))
	(list "+%^~&|<>=!."));; . is for PHP

    (if (= (point) 1)
	(insert-string "+");;beginning of buffer
      (when (and (= cbefore ?\ )
		 (not (null cbeforetwo))
		 (search (format "%c" cbeforetwo) list))
	(delete-backward-char 1)
	(setq cbefore (char-before (point))
	      cbeforetwo (char-before (1- (point)))))

      (if (search (format "%c" cbefore) list)
	  (if (and (not (null  cbeforetwo))
		   (search (format "%c" cbeforetwo) " \t"))
	      (insert-string "+ ")
	    (goto-char (1- (point)))
	    (insert-string " ")
	    (goto-char (1+ (point)))
	    (insert-string "+ "))
	(insert-string " + ")))))

(defun c-substract ()
  "Add whitespace around '-' "
  (interactive)
  (let ((cbefore (char-before (point)))
	(cbeforetwo (char-before (1- (point))))
	(list "-=!"))

    (if (= (point) 1)
	(insert-string "-");;beginning of buffer
      (when (and (= cbefore ?\ )
		 (not (null cbeforetwo))
		 (search (format "%c" cbeforetwo) list))
	(delete-backward-char 1)
	(setq cbefore (char-before (point))
	      cbeforetwo (char-before (1- (point)))))
      (if (search (format "%c" cbefore) list)
	  (if (and (not (null  cbeforetwo))
		   (search (format "%c" cbeforetwo) " \t"))
	      (if (eq cbefore ?-)
		  (insert-string "- ")
		(insert-string " - "))
	    (insert-string " - "))
	(insert-string " - ")))))

(defun c-right-arrow ()
  "Delete whitespace surrounding '-' in '->' denotation."
  (interactive)
  (let ((cbefore (char-before (point)))
	(cbefore-two (char-before (1- (point) )))
	(cbefore-three (char-before (- (point) 2))))
    (when (and (equal cbefore  ?\ )
	       (equal cbefore-two ?-)
	       (equal cbefore-three ?\ ))
      (goto-char (- (point) 3))
      (delete-char 1)
      (goto-char (1+ (point)))
      (delete-char 1))
    (insert-char ?> 1)))

(defun c-left-parenthesis ()
  "Leave a space between if, while, for and opening parenthesis."
  (interactive)
  (let ((point (point))
	(wordbefore nil))
    (if (equal (char-before) ?\ )
	(insert "(")
      (backward-word)
      (setq wordbefore (buffer-substring (point) point))
      (goto-char point)
      (if (or (string= wordbefore "if")
	      (string= wordbefore "while")
	      (string= wordbefore "for"))
	  (insert " (")
      (insert "(")))))

(defun c-left-brace ()
  (interactive)
  (let ((cbefore (char-before (point))))
    (if (not (= cbefore ?\ ))
	(insert " "))
    (insert "{"))
  (newline)
  (c-indent-line))

(defun get-to-beginning-of-line ()
  (let ((point (point))
	(point-bol))
    (save-excursion
      (beginning-of-line)
      (setq point-bol (point))
      (buffer-substring point-bol point))))

(defun c-right-brace ()
  (interactive)
  (let ((to-beginning-of-line (get-to-beginning-of-line)))
    (if (not (string-match "^ *$" to-beginning-of-line))
	(newline)))
  (insert "}")
  (c-indent-line)
  (newline)
  (c-indent-line))

(when running-xemacs
  (mapcar (lambda (map)
	    (define-key map (kbd "=") 'c-assign)
	    (define-key map (kbd "+") 'c-add)
	    (define-key map (kbd "-") 'c-substract)
	    (define-key map (kbd ">") 'c-right-arrow)
	    (define-key map (kbd "(") 'c-left-parenthesis)
	    (define-key map (kbd "{") 'c-left-brace)
	    (define-key map (kbd "}") 'c-right-brace)
	    ;; more definitions go here
	    )
	  (list c-mode-map c++-mode-map java-mode-map)))
