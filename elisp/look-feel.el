;;
;;	File:		look-feel.el
;;
;;	Description:	Look & feel customization
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: look-feel.el,v $
;;
;;	$Revision: 1.5 $
;;
;;	$Log: look-feel.el,v $
;;	Revision 1.5  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.4  2011/04/01 09:36:33  lixin
;;	Port to emacs 23 on windows.
;;
;;	Revision 1.3  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.2  2005/01/06 06:26:23  lixin
;;	Add CVS header
;;

(when running-xemacs
  (set-face-background 'default (make-color-specifier "#CCE8CF"));; RGB(204, 232, 207)
  (set-face-foreground 'default "black");;normal text
  (if (>= emacs-major-version 21)
      (progn
	(custom-set-variables
	 '(paren-mode (quote blink-paren) nil (paren))
	 '(blink-cursor-mode t nil (blink-cursor)))
	(custom-set-faces))
    (progn;;else
      (require 'paren)
      (paren-set-mode 'blink-paren))))

(when running-emacs
  (set-background-color "#CCE8CF")
  (set-cursor-color "black"))

(custom-set-faces
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "red" :bold t))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "black" :bold t))))
 '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "blue3"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "magenta4"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "steelblue" :bold t))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "steelblue4" :bold t))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "blue4" :family "clucida" :bold t :italic nil))))
 '(font-lock-doc-string-face ((((class color) (background light)) (:foreground "blue3" :bold t))))
 '(info-node ((t (:bold t))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "Red" :bold t))))
 '(highlight ((t (:background "darkseagreen2" :bold t))) t)
 '(man-italic ((t (:bold nil :underline t))))

 ;; for Emacs
 '(font-lock-string-face ((((class color) (background light)) (:foreground "blue4" :bold t))))
 '(font-lock-keywords ((((class color) (background light)) (:foreground "black" :bold t))))
 )
