;;	File:		utility.el
;;
;;	Description:	Useful lisp functions
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: utility.el,v $
;;
;;	$Revision: 1.6 $
;;
;;	$Log: utility.el,v $
;;	Revision 1.6  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.5  2009/01/05 00:44:05  lixin
;;	Change default tab size from 8 to 4.
;;
;;	Revision 1.4  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.3  2005/01/06 06:26:24  lixin
;;	Add CVS header
;;

(defun nop ()
  "Do nothing."
  ())

;;(defun dirname (string)
;;  (interactive)
;;  (if string
;;      (let ((start 0)
;;	    (pattern (format "\\%s" system-path-separator)))
;;	(while (string-match pattern string start)
;;	  (setq start (match-end 0)))
;;	(substring string 0 (1- start)))
;;    nil;;else
;;    ))

(defun basename (string &optional suffix)
  "Strip directory and suffix from filenames."
  (interactive)
  (if string
      (let ((start 0)
	    (end (length string))
	    (pattern (format "\\%s" system-path-separator)))

	(while (string-match pattern string start)
	  (setq start (match-end 0)))
	(if suffix
	    (let ((index (- end (length suffix))))
	      (if (string-match suffix string index)
		  (setq end index))))
	(substring string start end))

    nil;;else
    ))

;;(defun basename (pathname &optional suffix)
;;  (interactive)
;;  (if (string-match (format ".*\\%s" system-path-separator) pathname)
;;      (setq pathname (substring pathname (match-end 0))))
;;  (if (and suffix (string-match pathname (format "%s$" suffix)))
;;      (substring pathname 0 (match-beginning 0)))
;;  pathname)

(defun dirname ( pathname )
  "Strip non-directory suffix from file name."
  (interactive)
  (if (not (string-match (format ".*\\%s" system-path-separator) pathname))
      "."
    (let* ((dir (substring pathname 0 (match-end 0)))
	   (index (length dir)))
      (while (and (> index 0)
		  (char-equal (aref dir (decf index)) ?\/)))
      (substring dir 0 (incf index)))))

(defun suffix-name ( string )
  "Return suffix part of file name."
  (interactive)
  (if string
      (let ((index (1- (length string))))
	(while (and (>= index 0)
		    (not (char-equal (aref string index) ?\.)))
	  (setq index (1- index)))
	(if (= index -1)
	    ""
	  (substring string (1+ index))))
    nil;;else
    ))

(defun trim (string)
  "Remove leading or trailing whitespaces."
  (interactive)
  (let ((result string))
    (if (string-match "^ *"  result)
	(setq result (substring result (match-end 0))))
    ;; This doesn't work because "string" may span to multiple lines
    ;;(if (string-match " *$" result)
    ;;(setq  result (substring result 0 (match-beginning 0))))))
    (let ((len (- (length result) 1)))
      (while (and (>= len 0)
		  (= (aref result len) ?\ ));;?\ stands for space character
	(decf len))
      (if (>= len 0)
	  (substring result 0 (+ len 1))
	""))))

(defun remove-or-convert-trailing-ctl-M ()
  "Propose to remove or convert trailing ^M from a file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\^M" nil t)
	;; a ^M is found
	(if (or (= (preceding-char) ?\^J)
		(= (following-char) ?\^J) )
	    ;; Must find a way to display the buffer before this question
	    (if (y-or-n-p "Remove trailing ^M ? ")
		(progn (goto-char (point-min))
		       (perform-replace "\^M" "" nil nil nil)
		       (pop-mark) )
	      (message "No transformation.") )
	  (if (y-or-n-p "Convert ^M into ^J ? ")
	      (progn (goto-char (point-min))
		     (perform-replace "\^M" "\^J" nil nil nil)
		     (pop-mark) )
	    (message "No transformation.") ) )
      ;;(message "No ^M in this file !")
      )))

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
	(delete-char 1))))

;;(add-hook 'find-file-hooks 'remove-or-convert-trailing-ctl-M)

(defun delete-sp-at-bol ()
  "Delete consecutive whitespace at the beginning of current line."
  (interactive)
  (let* ((maxch (buffer-size (current-buffer )))
	 (lines (count-lines 1 maxch))
	 (line 1))
    (if ( > maxch 0 )
	(save-excursion
	  (while ( <= line lines)
	    (goto-line line)
	    (let (( p-b (point-at-bol))
		  ( p-e))
	      (beginning-of-line-text)
	      (setq p-e (point))
	      (kill-region p-b p-e))
	    (incf line))))))

(defun delete-sp-at-eol ()
  "Delete consecutive whitespaces at the end of curent line."
  (interactive)
  (let* ((maxch (buffer-size (current-buffer )))
	 (lines (count-lines 1 maxch))
	 (line 1))
    (if ( > maxch 0 )
	(save-excursion
	  (while ( <= line lines)
	    (goto-line line)
	    (let ((bol (point-at-bol))
		  (eol (point-at-eol)))
	      (goto-char eol)
	      (skip-chars-backward " \t" bol)
	      (kill-region (point) eol))
	    (incf line))))))

(defun remove-directory-recursive (path)
  "Remove a directory and it's subdirectories recursively."
  (interactive)
  (when (file-exists-p path)
    (set-file-modes path 511);;0777
    (if (file-directory-p path)
	(let ((files (directory-files
		      path
		      t "\\([^\\.].*\\)\\|\\(\\.[^\\.].*\\)\\|\\(\\.\\..+\\)")))
	  (mapc 'remove-directory-recursive files)
	  (condition-case nil (delete-directory path)(error 'error)))
      (condition-case nil
	  (delete-file path)(error 'error)))))

(defun string-case= (str1 str2)
  "Compare two strings ignoring their cases."
  (interactive)
  (let ((s1 (downcase str1))
	(s2 (downcase str2)))
    (string= s1 s2)))

(defvar default-tab-width 4)

(defvar tab-size 4 "Default tab size.")

(defun generate-tab-space ()
  "Generate spaces for a tab character."
  (interactive)
  (format "%*s" tab-size ""))

(unless (functionp 'cl-position)
  (defun cl-position (cl-item cl-seq cl-start &optional cl-end cl-from-end)
    (if (listp cl-seq)
	(let ((cl-p (nthcdr cl-start cl-seq)))
	  (or cl-end (setq cl-end 8000000))
	  (let ((cl-res nil))
	    (while (and cl-p (< cl-start cl-end) (or (not cl-res) cl-from-end))
	      (if (cl-check-test cl-item (car cl-p))
		  (setq cl-res cl-start))
	      (setq cl-p (cdr cl-p) cl-start (1+ cl-start)))
	    cl-res))
      (or cl-end (setq cl-end (length cl-seq)))
      (if cl-from-end
	  (progn
	    (while (and (>= (setq cl-end (1- cl-end)) cl-start)
			(not (cl-check-test cl-item (aref cl-seq cl-end)))))
	    (and (>= cl-end cl-start) cl-end))
	(while (and (< cl-start cl-end)
		    (not (cl-check-test cl-item (aref cl-seq cl-start))))
	  (setq cl-start (1+ cl-start)))
	(and (< cl-start cl-end) cl-start)))))

(unless (fboundp 'cl-check-test)
  (defmacro cl-check-test (item x)
    (list 'cl-check-test-nokey item (list 'cl-check-key x))))

(unless (fboundp 'cl-check-test-nokey)
  (defmacro cl-check-test-nokey (item x)
    (list 'cond
	  (list 'cl-test
		(list 'eq (list 'not (list 'funcall 'cl-test item x))
		      'cl-test-not))
	  (list 'cl-if
		(list 'eq (list 'not (list 'funcall 'cl-if x)) 'cl-if-not))
	  (list 't (list 'if (list 'numberp item)
			 (list 'equal item x) (list 'eq item x))))))

(unless (functionp 'mismatch)
  (defun mismatch (cl-seq1 cl-seq2 &rest cl-keys)
    "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
    (cl-parsing-keywords (:test :test-not :key :from-end
				(:start1 0) :end1 (:start2 0) :end2) ()
      (or cl-end1 (setq cl-end1 (length cl-seq1)))
      (or cl-end2 (setq cl-end2 (length cl-seq2)))
      (if cl-from-end
	  (progn
	    (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
			(cl-check-match (elt cl-seq1 (1- cl-end1))
					(elt cl-seq2 (1- cl-end2))))
	      (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	    (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
		 (1- cl-end1)))
	(let ((cl-p1 (and (listp cl-seq1) (nthcdr cl-start1 cl-seq1)))
	      (cl-p2 (and (listp cl-seq2) (nthcdr cl-start2 cl-seq2))))
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		      (cl-check-match (if cl-p1 (car cl-p1)
					(aref cl-seq1 cl-start1))
				      (if cl-p2 (car cl-p2)
					(aref cl-seq2 cl-start2))))
	    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)
		  cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       cl-start1))))))

(unless (fboundp 'cl-check-match)
  (defmacro cl-check-match (x y)
    (setq x (list 'cl-check-key x) y (list 'cl-check-key y))
    (list 'if 'cl-test
	  (list 'eq (list 'not (list 'funcall 'cl-test x y)) 'cl-test-not)
	  (list 'if (list 'numberp x)
		(list 'equal x y) (list 'eq x y)))))

(unless (fboundp 'cl-parsing-keywords)
  (defmacro cl-parsing-keywords (kwords other-keys &rest body)
    (cons
     'let*
     (cons (mapcar
	    (function
	     (lambda (x)
	       (let* ((var (if (consp x) (car x) x))
		      (mem (list 'car (list 'cdr (list 'memq (list 'quote var)
						       'cl-keys)))))
		 (if (eq var :test-not)
		     (setq mem (list 'and mem (list 'setq 'cl-test mem) t)))
		 (if (eq var :if-not)
		     (setq mem (list 'and mem (list 'setq 'cl-if mem) t)))
		 (list (intern
			(format "cl-%s" (substring (symbol-name var) 1)))
		       (if (consp x) (list 'or mem (car (cdr x))) mem)))))
	    kwords)
	   (append
	    (and (not (eq other-keys t))
		 (list
		  (list 'let '((cl-keys-temp cl-keys))
			(list 'while 'cl-keys-temp
			      (list 'or (list 'memq '(car cl-keys-temp)
					      (list 'quote
						    (mapcar
						     (function
						      (lambda (x)
							(if (consp x)
							    (car x) x)))
						     (append kwords
							     other-keys))))
				    '(car (cdr (memq (quote :allow-other-keys)
						     cl-keys)))
				    '(error "Bad keyword argument %s"
					    (car cl-keys-temp)))
			      '(setq cl-keys-temp (cdr (cdr cl-keys-temp)))))))
	    body)))))

(unless (fboundp 'cl-check-key)
  (defmacro cl-check-key (x)
    (list 'if 'cl-key (list 'funcall 'cl-key x) x)))

(unless (functionp 'search)
  (defun search (cl-seq1 cl-seq2 &rest cl-keys)
    "Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
    (cl-parsing-keywords (:test :test-not :key :from-end
				(:start1 0) :end1 (:start2 0) :end2) ()
      (or cl-end1 (setq cl-end1 (length cl-seq1)))
      (or cl-end2 (setq cl-end2 (length cl-seq2)))
      (if (>= cl-start1 cl-end1)
	  (if cl-from-end cl-end2 cl-start2)
	(let* ((cl-len (- cl-end1 cl-start1))
	       (cl-first (cl-check-key (elt cl-seq1 cl-start1)))
	       (cl-if nil) cl-pos)
	  (setq cl-end2 (- cl-end2 (1- cl-len)))
	  (while (and (< cl-start2 cl-end2)
		      (setq cl-pos (cl-position cl-first cl-seq2
						cl-start2 cl-end2 cl-from-end))
		      (apply 'mismatch cl-seq1 cl-seq2
			     :start1 (1+ cl-start1) :end1 cl-end1
			     :start2 (1+ cl-pos) :end2 (+ cl-pos cl-len)
			     :from-end nil cl-keys))
	    (if cl-from-end (setq cl-end2 cl-pos) (setq cl-start2 (1+ cl-pos))))
	  (and (< cl-start2 cl-end2) cl-pos))))))

(defun find-buffer-by-name (name)
  "Find existing buffer by name, nil if not found."
  (interactive)
  (let ((buffer-list (buffer-list))
	(buffer nil)
	(buffer-name nil))
    (while (and buffer-list (not buffer))
      (setq buffer (car buffer-list)
	    buffer-name (buffer-name buffer)
	    buffer-list (cdr buffer-list))
      (unless (string= name buffer-name)
	(setq buffer nil)))
    buffer))
