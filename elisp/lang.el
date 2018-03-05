;;
;;	File:		lang.el
;;
;;	Description:	Programming language modes customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2018/02/28 19:39:27 $
;;
;;	$RCSfile: lang.el,v $
;;
;;	$Revision: 1.29 $
;;
;;	$Log: lang.el,v $
;;	Revision 1.29  2018/02/28 19:39:27  xinli
;;	Add support for MemVerge.
;;
;;	Revision 1.28  2014/02/05 23:58:24  xinli
;;	For VMware code base, compile using iscons and set code search path.
;;
;;	Revision 1.27  2014/02/04 02:42:43  xinli
;;	Don't update cscope database.
;;
;;	Revision 1.26  2013/07/03 18:10:10  xinli
;;	Call replace-in-string in XEmacs, call replace-regexp-in-string in Emacs.
;;
;;	Revision 1.25  2013/06/27 00:39:01  xinli
;;	Tune auto-newline for brance, comma and semicolon.
;;
;;	Revision 1.24  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;
;;	Revision 1.23  2013/04/26 22:37:40  xinli
;;	Set python indent offset to 3. It's VMware standard.
;;
;;	Revision 1.22  2013/04/17 16:41:11  xinli
;;	Add xcscope support.
;;
;;	Revision 1.21  2013/03/28 20:02:30  xinli
;;	Add imenu to C,C++,java and lisp mode.
;;
;;	Revision 1.20  2012/10/29 23:56:40  xinli
;;	Replace replace-in-string with replace-regexp-in-string.
;;
;;	Revision 1.19  2012/01/30 08:26:49  lixin
;;	Disable xcscope and c-sharp.
;;
;;	Revision 1.18  2010/01/21 04:54:36  lixin
;;	Change indent from tab to three space.
;;
;;	Revision 1.17  2010/01/21 04:16:11  lixin
;;	Change for EMC-VMWare joint project Aurora.
;;
;;	Revision 1.16  2009/07/31 09:30:33  lixin
;;	Fix one typo.
;;
;;	Revision 1.15  2009/07/31 02:22:37  lixin
;;	Fix bug in remote build.
;;
;;	Revision 1.14  2008/12/04 12:43:36  lixin
;;	Reload TAG file if modified.
;;
;;	Revision 1.13  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.12  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

(defun c-indent ()
  "Indent C/C++/JAVA source code."
  (interactive)
  (c-indent-region (point-min) (point-max)))

;;c-indent-region provides the same function and much faster:)
;;(defun c-indent ()
;; "Indent C/C++/JAVA source code."
;;  (interactive)
;;  (let ((maxch) (lines) (line))
;;    (progn (setq maxch (buffer-size (current-buffer )))
;;	   (if ( and ( > maxch 0 )  c-buffer-is-cc-mode)
;;	       (save-excursion
;;		 (setq lines (count-lines 1 maxch))
;;		 (setq line 1)
;;		 (while ( <= line lines)
;;		   (message "Goto line %d" line)
;;		   (goto-line line)
;;		   (c-indent-command)
;;		   (incf  line))
;;		 (message "Indent whole complete."))
;;	     (message "Not c-like syntax or empty file.")))))

(defun access-label-offset (langelem)
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((relpos (cdr langelem))
	     (indent-point (point))
	     (state (c-parse-state))
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     ;; boc is the point of beginning of the enclosing class
	     (boc (aref inclass-p 0)))
	(widen)

	(goto-char relpos)
	(let ((col (current-column)))
	  (if (and (> col 0)
		   (<= relpos indent-point))
	      0
	    (+ c-basic-offset
	       (save-excursion
		 (goto-char boc)
		 (current-column)))))))))

(defvar header-file-in-tag nil "Header files in TAG file")
(defvar tag-file-timestamp 0 "Timestamp of TAG file")

(defun file-last-modified-timestamp (file)
  (let ((last-modified (nth 5 (file-attributes file))))
    (+ (* (car last-modified) 65535) (nth 1 last-modified))))

(defun load-header-file-in-tag (tag-file)
  (interactive)
  (let ((buffer (find-file tag-file))
	(point 1)
	(line)
	(last-modified (nth 5 (file-attributes tag-file)))
	(index 0))
    (when buffer
      (goto-char point buffer)
      (setq header-file-in-tag nil)
      (setq tag-file-timestamp (file-last-modified-timestamp tag-file))
      (while (setq point (search-forward "\014" nil t 1 buffer))
	(goto-char (+ point 1))
	(setq line (buffer-substring (point-at-bol) (point-at-eol) buffer))
	(string-match "\\(.*\\),[0-9]*" line)
	(push (match-string 1 line) header-file-in-tag))
      (kill-buffer buffer))))

(defun open-c-header ()
  (interactive)
  (let* ((start (point-at-bol))
	 (end   (point-at-eol))
	 (line (buffer-substring start end))
	 (tag-file-name  (expand-file-name "TAGS" "~/"))
	 (filename))
    (when (string-match "^\\( \\|\t\\)*#\\( \\|\t\\)*include\\( \\|\t\\)*\\(<\\|\"\\)\\(.*\\)\\(>\\|\"\\)\\( \\|\t\\)*$" line)
      (setq filename (match-string 5 line))
      (when (and ;;(not header-file-in-tag)
	     (file-exists-p tag-file-name)
	     (> (file-last-modified-timestamp tag-file-name) tag-file-timestamp))
	(message "Loading TAGS file ...")
	(load-header-file-in-tag tag-file-name))
      (when header-file-in-tag
	(let ((header-file (find filename header-file-in-tag :test (lambda (str1 str2)
								     (let ((len1 (length str1))
									   (len2 (length str2)))
								       (if (> len1 len2)
									   nil
									 (string= str1 (substring str2 (- len2 len1)))))))))
	  (if header-file
	      (find-file header-file)
	    (message (format "Header file %s not found in TAGS" filename))))))))

;; Deprecated, use open-c-header
(defun open-header ()
  "Open header file included in current line using C '#include' directive."
  (interactive)
  (let* ((start  (point-at-bol))
	 (end (point-at-eol))
	 (content (buffer-substring start end))
	 (search-list '("/usr/include/"))
	 (filename)
	 (pathname)
	 (found))
    (if (string-match "^ *# *include" content)
	(progn
	  (setq start (match-end 0))
	  (setq content (trim (substring content start)))
	  (if (string-match "\\([^<\"]\\)\\([^>\"]\\)*" content)
	      (progn
		(setq filename (match-string 0 content))
		(if (boundp 'additional-include-path)
		    (if (atom additional-include-path)
			(if (string-equal additional-include-path ".")
			    (push (format "%s/" (dirname (buffer-file-name)))
				     search-list);; :test 'string-equal)
			  (push additional-include-path search-list));; :test 'string-equal))
		      (let ((include-path (reverse additional-include-path)))
			(while include-path
			  (if (string-equal (car include-path) ".")
			      (push (format "%s/" (dirname (buffer-file-name)))
				       search-list);; :test 'string-equal)
			    (push (car include-path) search-list));; :test 'string-equal))
			  (setq include-path (cdr include-path))))))

		(while (and search-list (not found))
		  (setq pathname (concat (car search-list) filename)
			search-list (cdr search-list))
		  (if (and (file-exists-p pathname)
			   (file-readable-p pathname))
		      (progn
			(setq found t)
			(find-file pathname))))
		(if (not found)
		    (message "%s doesn't exist or is not readable." filename)))
	    (message "Not valid include directive!.")))
      (message "Not valid include directive."))))

;; define c, cpp and java hooks
(defun make-c-c++-java-extent ()
  "Generate extent for C/C++/JAVA source code."
  (when (or (featurep 'gif)
	    (featurep 'GIF))
    (let* ((filename (buffer-file-name))
	   (ext nil)
	   (suffix nil)
	   (glyph nil))
      (when (extent-list)
	(setq ext (make-extent (point-min) (point-max)))
	(if (string-match ".*\\." buffer-file-name)
	    (progn
	      (setq suffix (substring filename (match-end 0)))
	      (setq suffix
		    (cond ((string= suffix "C")     "cpp")
			  ((string= suffix "cc")    "cpp")
			  ((string= suffix "cxx")   "cpp")
			  ((string= suffix "hh")    "h")
			  ((string= suffix "h")     "h")
			  ((string= suffix "i")     "c")
			  ((string= suffix "c")     "c")
			  ((string= suffix "java")  "java")
			  (t                        "cpp"))))
	  (setq suffix "cpp"))
	(set-extent-property ext 'start-closed t)
	(set-extent-property ext 'end-closed t)
	(set-extent-property ext 'detachable nil)
	(setq glyph (make-glyph
		     (vector 'gif ':file
			     (expand-file-name (format "%s.gif" suffix)
					       (expand-file-name "pic" emacs-base)))
		     (set-extent-begin-glyph ext glyph)))
	(set-extent-begin-glyph ext glyph)))))

(defun c-insert-comment ()
  "Embrase current line with C style comment."
  (goto-char (point-at-bol))
  (if (looking-at (format "[ \t]*\\(%s\\)" comment-start-skip))
      nil
    (funcall indent-line-function)
    (insert comment-start)
    (goto-char (point-at-eol))
    (insert comment-end))
  (funcall indent-line-function))

(defun c-remove-comment ()
  "Remove C style comment in current if any."
  (goto-char (point-at-bol))
  (if (looking-at (format "[ \t]*\\(%s\\)" comment-start-skip))
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(delete-char (- end start))
	(goto-char (point-at-eol))
	(if (setq start (search-backward "*/" (point-at-bol) t))
	    (progn
	      (setq start (match-beginning 0) end (match-end 0))
	      (goto-char start)
	      (delete-char (- end start))))))
  (funcall indent-line-function))

(defun c-comment-line ()
  "Embrass current li ne with C style comment if there is no comment or remove it if it already exists."
  (goto-char (point-at-bol))
  (if (looking-at (format "[ \t]*\\(%s\\)" comment-start-skip))
      (c-remove-comment)
    (c-insert-comment)))

(defun c-comment-region ()
  "Add C style comment to a region or remove C style comment from a region."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(let* ((line-beg (count-lines 1 (region-beginning)))
	       (line-end (count-lines 1 (region-end)))
	       (line line-beg)
	       (fun))
	  (goto-char (region-beginning))
	  (if (bolp)
	      (incf line))
	  (goto-char (point-at-bol))
	  (if (looking-at (format "[ \t]*\\(%s\\)" comment-start-skip))
	      (setq fun 'c-remove-comment)
	    (setq fun 'c-insert-comment))
	  (while (<= line  line-end)
	    (funcall fun)
	    (funcall indent-line-function)
	    (next-line 1)
	    (incf line)))
      (c-comment-line))
    (message "Done")))

(defun common-c-c++-java-mode-hook ()
  "Hook for C, C++ and JAVA code."
  (font-lock-mode 1)
  ;;(c-add-style "PERSONAL" my-c-style t)
  (when running-xemacs
    (add-menu-button nil ["%_Indent" c-indent t]))
  ;;show syntactic infor when indent
  (setq c-echo-syntactic-information-p nil)
  ;;(setq comment-column 32)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'label -1000)
  (c-toggle-auto-hungry-state 1)
  (setq c-strict-syntax-p nil);;TEST
  (setq c-progress-interval 1)
  ;; for function c-electric-semi&comma and c-semi&comma-newline
  ;;(load "csemi")
  ;;(setq c-hanging-semi&comma-criteria
	;;'(c-semi&comma-no-newlines-before-nonblanks c-semi&comma-newline))
  ;;(c-indent)
  (when running-xemacs
    (make-c-c++-java-extent))
  (define-key c-mode-base-map "\C-c\C-c" 'c-comment-region)
  (define-key c-mode-base-map [(control /)] 'c-comment-region)
  ;;(define-key c-mode-base-map "\015" 'newline-and-indent)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  ;;(setq c-hanging-braces-alist '((block-close . c-snug-do-while)))
  (setq c-hanging-braces-alist
	(append '((class-open after)
		  (class-close)
		  (defun-open after))
		c-hanging-braces-alist))
  (setq indent-tabs-mode nil)
  (imenu-add-menubar-index)
  )

(defvar local-compilation-search-path nil
  "Buffer local compilation search path")

(make-variable-buffer-local 'local-compilation-search-path)

(defun common-c-c++-mode-hook ()
  "Hook for C and C++ code."
  (when running-xemacs
    (add-menu-button nil ["%_Header" open-c-header t]))
  (define-key c-mode-map "\M-q" 'reindent-c-comment)
  (let ((file-name (buffer-file-name)))
    (when (string-match "^\\(.+\\)/work/\\([^/]+\\)/bora/.+$" file-name)
      (setq local-compilation-search-path
	    (list (format "%s/work/%s/"
			  (match-string 1 file-name)
			  (match-string 2 file-name)))))))

(defun remote-build-command (dir)
  "Compse remote build command for Makefile in dir"
  (format "ssh %s %s %s cd %s; %s "
	  (if remote-build-user
	      (format "-l %s" remote-build-user)
	    "")
	  (if remote-build-identity
	      (format "-i %s" remote-build-identity)
	    "")
	  remote-build-host dir default-make))

(defun my-c-mode-hook ()
  "Hook for C code."
  (common-c-c++-java-mode-hook)
  (common-c-c++-mode-hook)
  (setq font-lock-keywords c-font-lock-keywords-3)
  (when running-xemacs
    (delete-menu-item '("C")))
  (setq default-compiler default-c-compiler
	default-compiler-flag default-c-compiler-flag)
  (if (or (file-exists-p "makefile") (file-exists-p "Makefile")
	  (file-exists-p "GNUmakefile"))
      (if (not (and (boundp 'remote-build-host) remote-build-host))
	  (setq compile-command default-make)
	(let ((dir (file-name-directory buffer-file-name)))
	  (setq dir (if running-xemacs
			(replace-in-string dir "\\\\" "/")
		      (replace-regexp-in-string dir "\\\\" "/")))
	  (if (and (> (length dir) 1)
		   (eq (aref dir 1) ?:))
	      (setq dir (substring dir 3)))
	  (setq compile-command (remote-build-command dir))))
    (setq compile-command
	  (format "%s %s %s "
		  default-c-compiler
		  default-c-compiler-flag
		  ;;(basename buffer-file-name
		  ;;(format ".%s" (suffix-name buffer-file-name)))
		  buffer-file-name))))

(defun my-c++-mode-hook ()
  "Hook for C++ code."
  (common-c-c++-java-mode-hook)
  (common-c-c++-mode-hook)
  (c-set-offset 'access-label 0)
  (c-set-offset 'innamespace 0)
  (setq font-lock-keywords c++-font-lock-keywords-3)
  (when running-xemacs
    (delete-menu-item '("C++")))
  (setq default-compiler default-c++-compiler
	default-compiler-flag default-c++-compiler-flag)
  (if (or (file-exists-p "makefile") (file-exists-p "Makefile")
	  (file-exists-p "GNUmakefile"))
      (if (not (and (boundp 'remote-build-host) remote-build-host))
	  (setq compile-command default-make)
	(let ((dir (file-name-directory buffer-file-name)))
	  (setq dir (if running-xemacs
			(replace-in-string "\\\\" "/")
		      (replace-regexp-in-string dir "\\\\" "/")))
	  (if (and (> (length dir) 1)
		   (eq (aref dir 1) ?:))
	      (setq dir (substring dir 3)))
	  (setq compile-command (remote-build-command dir))))
    (setq compile-command
	  (format "%s %s %s "
		  default-c++-compiler
		  default-c++-compiler-flag
		  ;;(basename buffer-file-name
		  ;;(format ".%s" (suffix-name buffer-file-name)))
		  buffer-file-name))))

(defun my-java-mode-hook ()
  "Hook for JAVA code."
  (common-c-c++-java-mode-hook)
  ;;affect indent on public,private,protected
  (c-set-offset 'topmost-intro-cont 'access-label-offset)
  (setq font-lock-keywords java-font-lock-keywords-3)
  (when running-xemacs
    (delete-menu-item '("Java")))
  (setq default-compiler default-java-compiler
	default-compiler-flag default-java-compiler-flag)
  (if (file-exists-p "build.xml")
      (setq compile-command "ant")
    (if (or (file-exists-p "makefile") (file-exists-p "Makefile")
	    (file-exists-p "GNUmakefile"))
	(if (not (and (boundp 'remote-build-host) remote-build-host))
	    (setq compile-command default-make)
	  (let ((dir (file-name-directory buffer-file-name)))
	    (setq dir (replace-regexpin-string dir "\\\\" "/"))
	    (if (and (> (length dir) 1)
		     (eq (aref dir 1) ?:))
		(setq dir (substring dir 3)))
	    (setq compile-command (remote-build-command dir))))
      (setq compile-command
	    (format "%s %s %s"
		    default-java-compiler
		    default-java-compiler-flag
		    (basename buffer-file-name))))))

(defun lisp-comment-region ( &optional region )
  "Add lisp comment to a region or remove lisp comment from a region."
  (interactive)
  (save-excursion
    (let ((comment-start (concat comment-start comment-start))
	  (comment-start-skip "[ \t]*;+ *"))
      (c-comment-region))))

(defun  my-emacs-lisp-mode-hook ()
  "Hook for elisp code."
  (when running-xemacs
    (add-menu-button nil ["%_Indent" (lisp-indent-region 0 (1- (point-max))) t])
    (delete-menu-item '("Emacs-Lisp")))
  (font-lock-mode 1)
  (setq compile-command buffer-file-name)
  (setq compile-function (symbol-function 'byte-compile-file))
  (setq font-lock-keywords lisp-font-lock-keywords-2)
  (define-key emacs-lisp-mode-map "\C-c\C-c" 'lisp-comment-region)
  (define-key emacs-lisp-mode-map [(control /)] 'lisp-comment-region)
  (define-key emacs-lisp-mode-map "\015" 'newline-and-indent)
  (imenu-add-menubar-index))

(defun my-lisp-interaction-mode-hook ()
  "Hook for lisp code."
  (when running-xemacs
    (delete-menu-item '("Lisp-Interaction"))
    (delete-menu-item '("Lisp")))
  (define-key lisp-interaction-mode-map "\015" 'newline-and-indent)
  (my-emacs-lisp-mode-hook))

(defun my-perl-mode-hook ()
  "Hook for perl code."
  (when running-xemacs
    (delete-menu-item '("Perl")))
  (font-lock-mode 1)
  (setq font-lock-keywords perl-font-lock-keywords-2)
  (define-key cperl-mode-map "\C-c\C-c" 'c-comment-region)
  (define-key cperl-mode-map [(control /)] 'c-comment-region)
  (define-key cperl-mode-map "\015" 'newline-and-indent))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-interaction-mode-hook)
(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
;;(setq c-recognize-knr-p nil)

;;(defconst my-c-style
;;  '((c-tab-always-indent           . t)
;;    (c-comment-only-line-offset    . 4)
;;    (c-hanging-braces-alist        . ((substatement-open after)
;;				      (brace-list-open)))
;;    (c-hanging-colons-alist        . ((member-init-intro before)
;;				      (inher-intro)
;;				      (case-label after)
;;				      (label after)
;;				      (access-label 0)))
;;    (c-cleanup-list                . (scope-operator
;;				      empty-defun-braces
;;				      defun-close-semi))
;;    (c-offsets-alist               . ((arglist-close     . c-lineup-arglist)
;;				      (substatement-open . 0)
;;				      (case-label        . 4)
;;				      (access-label      . 0)
;;				      (block-open        . 0)
;;				      (knr-argdecl-intro . -)))
;;    (c-echo-syntactic-information-p . t)
;;    )
;;  "My C Programming Style"
;;  )

;;; Customizations for all of c-mode, c++-mode, and objc-mode
;;(defun my-c-mode-common-hook ()
;;  ;; add my personal style and set it for the current buffer
;;  (c-add-style "PERSONAL" my-c-style t)
;;  ;; offset customizations not in my-c-style
;;  (c-set-offset 'member-init-intro '++)
;;  ;; other customizations
;;  (setq tab-width 4
;;	;; this will make sure spaces are used instead of tabs
;;	indent-tabs-mode nil)
;;  ;; we like auto-newline and hungry-delete
;;  (c-toggle-auto-hungry-state 1)
;;  ;; keybindings for C, C++, and Objective-C.  We can put these in
;;  ;; c-mode-map because c++-mode-map and objc-mode-map inherit it
;;  (define-key c-mode-map "\C-m" 'newline-and-indent)


(defun my-insert-parentheses (arg)
  "Insert a pair of parentheses and place cursor after the first one."
  (interactive "P")
  (insert-parentheses 0)
  (insert-string "   ")
  (backward-char 2)
  )

(global-set-key "\M-(" 'my-insert-parentheses)

(defun insert-c-header ()
  "Insert defensive #ifndef/#define/#endif directives for C/C++ header files."
  (interactive)
  (let ((file (buffer-file-name))
	(suffix))
    (when file
      (setq file (basename file))
      (setq file (upcase file))
      (setq suffix (basename file ".H"))
      (if (string-equal file suffix)
	  (message "Not C/C++ header.")
	(setq file (if running-xemacs
		       (replace-in-string "\\." "_")
		     (replace-regexp-in-string file "\\." "_")))
	;;(setq file (format "_%s_" file))
	(insert (format "#ifndef _%s_\n" file))
	(insert (format "#define _%s_\n\n\n" file))
	(insert (format "#endif /* _%s_ */\n" file))
	))))

(defvar additional-include-path '(".") "Additional directories to look for a C/C++ header file")

(cond ((equal system-type 'windows-nt) ;; Windows
       )
      (t ;; Various UNIX systems
       (push "/usr/include/" additional-include-path);; :test 'string-equal)

       ;; ${HOME}/include
       (let ((include (expand-file-name "include/" "~")))
	 (if (and (file-exists-p include)
		  (file-directory-p include))
	     (push include additional-include-path)));; :test 'string-equal)))

       ;; ${HOME}/share/gcc/include/g++-v3
       (let ((include (expand-file-name "share/gcc/include/g++-v3/" "~")))
	 (if ( and (file-exists-p include)
		  (file-directory-p include))
	     (push include additional-include-path)))));; :test 'string-equal)))))

(defun fill-get-method (&optional var-name var-type)
  (interactive)
  (if (not var-name)
      (setq var-name (read-string "Variable name:")))
  (if (not var-type)
      (setq var-type (read-string "Variable type:")))
  (when (and (> (length var-name) 0)
	     (> (length var-type) 0))
    (let ((var-name-cap (substring var-name 0)))
      (aset var-name-cap 0 (upcase (aref var-name 0)))
      (insert-string (format "\npublic %s get%s()\n{\nreturn %s;\n}\n"
			     var-type var-name-cap var-name)))
    ))

(defun fill-set-method (&optional var-name var-type)
  (interactive)
  (if (not var-name)
      (setq var-name (read-string "Variable name:")))
  (if (not var-type)
      (setq var-type (read-string "Variable type:")))
  (when (and (> (length var-name) 0)
	     (> (length var-type) 0))
    (let ((var-name-cap (substring var-name 0))
	  (field-accessor "."))
      (when (or (eq major-mode 'c-mode)
		(eq major-mode 'c++-mode))
	(setq field-accessor "->"))
      (aset var-name-cap 0 (upcase (aref var-name 0)))
      (insert-string (format "\npublic void set%s(%s %s)\n{\nthis%s%s = %s;\n}\n"
			     var-name-cap var-type var-name field-accessor var-name var-name)))
    ))

(defun fill-get-set-method (&optional var-name var-type)
  (interactive)
  (if (not var-name)
      (setq var-name (read-string "Variable name:")))
  (if (not var-type)
      (setq var-type (read-string "Variable type:")))
  (when (and (> (length var-name) 0)
	     (> (length var-type) 0))
    (fill-get-method var-name var-type)
    (fill-set-method var-name var-type)))

(defun get-word-at-cursor (&optional format)
  (interactive)
  (when (not format)
    (setq format "A-Za-z0-9_"))
  (let ((word nil))
    (setq word (save-excursion
		 (buffer-substring
		  (progn
		    (re-search-backward "\\sw" nil t)
		    (skip-chars-backward format) (point))
		  (progn (skip-chars-forward format) (point)))))))

(defun clone-field (&optional field)
  (interactive)
  (when (not field)
    (setq field (get-word-at-cursor))
    (when (and field (> (length field) 0))
      (let ((point (point))
	    (field-accessor "."))
	(when (or (eq major-mode 'c-mode)
		  (eq major-mode 'c++-mode))
	  (setq field-accessor "->"))
	(goto-char (point-max))
	(insert-string (format "this%s%s = clone%s%s;\n"
			       field-accessor field field-accessor field))
	(goto-char point)))))

(defun my-tcl-mode-hook ()
  (define-key tcl-mode-map (kbd "DEL") 'backward-or-forward-delete-char))

(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)

;;(define-key c-mode-base-map [(tab)] 'hippie-expand)
;;(define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
;;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
;;(setq c-macro-shrink-window-flag t)
;;(setq c-macro-preprocessor "cpp")
;;(setq c-macro-cppflags " ")
;;(setq c-macro-prompt-flag t)
;;(setq hs-minor-mode t)
;;(setq abbrev-mode t)

(setq semanticdb-project-roots (list (expand-file-name "/")))

(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command))
  )

(autoload 'senator-try-expand-semantic "senator")

(setq hippie-expand-try-functions-list
 	  '(
		senator-try-expand-semantic
		try-expand-dabbrev
		try-expand-dabbrev-visible
		try-expand-dabbrev-all-buffers
		try-expand-dabbrev-from-kill
		try-expand-list
		try-expand-list-all-buffers
		try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        ))

(global-set-key [(control tab)] 'my-indent-or-complete)

;(require 'csharp-mode)
;(defun my-csharp-mode-hook ()
;  (setq default-compiler default-csharp-compiler)
;  (setq compile-command (format "%s %s"
;				default-compiler buffer-file-name)))
;(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(require 'xcscope)
(global-set-key "\M-." 'cscope-find-global-definition-no-prompting)
(global-set-key (kbd "C-M-.") 'cscope-find-this-symbol)
(setq cscope-do-not-update-database t)

(setq python-indent-offset 3)
