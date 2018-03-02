
(defun code-prologue (file-name)
  (code-prologue-MemVerge file-name))

(defun code-prologue-MemVerge (file-name)
  (let ((name-in-header (concat "_"
				(if running-xemacs
				    (replace-in-string (upcase (file-name-nondirectory file-name)) "\\." "_")
				  (replace-regexp-in-string (upcase (file-name-nondirectory file-name)) "\\." "_"))
				"_")))
    (concat "/*\n"
	    " * Copyright (C) " (format-time-string "%Y") " MemVerge inc.\n"
	    " */\n"
	    )))

(defun code-prologue-EMC-VMWare (file-name)
  (let ((name-in-header (concat "_"
				(if running-xemacs
				    (replace-in-string (upcase (file-name-nondirectory file-name)) "\\." "_")
				  (replace-regexp-in-string (upcase (file-name-nondirectory file-name)) "\\." "_"))
				"_")))
    (concat "/* **********************************************************"
	    "\n"
	    " * Copyright (C) " (format-time-string "%Y") " EMC Corporation."
	    "\n"
	    " * Copyright (C) " (format-time-string "%Y") " VMware, Inc."
	    "\n"
	    " * All Rights Reserved."
	    "\n"
	    " * **********************************************************/"
	    "\n\n"
	    "/*\n"
	    " * " (file-name-nondirectory file-name) " --" "\n"
	    " *\n"
	    " *      This header file illustrates all the elements\n"
	    " *      of a module interface header file.\n"
	    " */\n\n"
	    "#ifndef " name-in-header "\n"
	    "#define " name-in-header "\n"
	    "\n"
	    "#include <stdio.h>\n"
	    "#include \"\"\n"
	    "\n\n"
	    "#endif // ifndef " name-in-header "\n"
	    )))

(defun code-prologue-EMC (file-name)
  (concat "/*\n"
	  " * Copyright (C) "
	  (format-time-string "%Y")
	  ", All Rights Reserved, by\n"
	  " * EMC Corporation, Hopkinton, MA.\n"
	  " *\n"
	  " * This software is furnished under a license and may be used and copied\n"
	  " * only  in  accordance  with  the  terms  of such  license and with the\n"
	  " * inclusion of the above copyright notice. This software or  any  other\n"
	  " * copies thereof may not be provided or otherwise made available to any\n"
	  " * other person. No title to and ownership of  the  software  is  hereby\n"
	  " * transferred.\n"
	  " *\n"
	  " * The information in this software is subject to change without  notice\n"
	  " * and  should  not be  construed  as  a commitment by EMC Corporation.\n"
	  " *\n"
	  " * EMC assumes no responsibility for the use or  reliability  of its\n"
	  " * software on equipment which is not supplied by EMC.\n"
	  " */\n"))

;; Example

;;/*
;;* Copyright (C) 2008, All Rights Reserved, by
;;* EMC Corporation, Hopkinton, MA.
;;*
;;* This software is furnished under a license and may be used and copied
;;* only  in  accordance  with  the  terms  of such  license and with the
;;* inclusion of the above copyright notice. This software or  any  other
;;* copies thereof may not be provided or otherwise made available to any
;;* other person. No title to and ownership of  the  software  is  hereby
;;* transferred.
;;*
;;* The information in this software is subject to change without  notice
;;* and  should  not be  construed  as  a commitment by EMC Corporation.
;;*
;;* EMC assumes no responsibility for the use or  reliability  of its
;;* software on equipment which is not supplied by EMC.
;; */

(defun code-prologue-MorganStanley (file-name)
  (concat "///////////////////////////////////////////////////////////////////////////////\n"
	  "//\n"
	  "// Copyright (c) "
	  (format-time-string  "%Y")
	  " Morgan Stanley & Co. Incorporated, All Rights Reserved\n"
	  "//\n"
	  "// Unpublished copyright.  All rights reserved.  This material contains\n"
	  "// proprietary information that shall be used or copied only within Morgan\n"
	  "// Stanley, except with written permission of Morgan Stanley.\n"
	  "//\n"
	  "///////////////////////////////////////////////////////////////////////////////\n"))

;; Example
;;///////////////////////////////////////////////////////////////////////////////
;;//
;;// Copyright (c) 2008 Morgan Stanley & Co. Incorporated, All Rights Reserved
;;//
;;// Unpublished copyright.  All rights reserved.  This material contains
;;// proprietary information that shall be used or copied only within Morgan
;;// Stanley, except with written permission of Morgan Stanley.
;;//
;;///////////////////////////////////////////////////////////////////////////////

(defun code-prologue-eBay (file-name)
  (concat
   "/*\n"
   " *\tFile:\t\t" (file-name-nondirectory file-name) "\n *\n"
   " *\tDescription:\t\n *\n"
   " *\tCompany:\teBay CDC\n *\n"
   " *\tCopyright:\tCopyright (c) "
   (format-time-string  "%Y")
   "\n *\n"
   " *\tAuthor:\t\t" (user-full-name) "\n *\n"
   " * \tCreated on:\t"
   (format "%s %s %s"
	   (format-time-string  "%Y/%m/%d %H:%M:%S")
	   (nth 1 (current-time-zone))
	   (format-time-string "%A %B %e"))
   "\n *\n"
   " *\tLast modifed on: $Date" "$\n *\n"
   " *\t$RCSfile" "$\n *\n"
   " *\t$Revision" "$\n *\n"
   " *\t$Date" "$\n *\n"
   " *\t$Log" "$\n */\n"))

;; Example
;;/*
;;*	File:		Example.c
;;*
;;*	Description:
;;*
;;*	Company:	eBay CDC
;;*
;;*	Copyright:	Copyright (c) 2008
;;*
;;*	Author:		Andrew Li
;;*
;;* 	Created on:	2008/03/18 17:36:16 HKT Tuesday March 18
;;*
;;*	Last modifed on: $Date: 2018/02/28 19:39:27 $
;;*
;;*	$RCSfile: prologue.el,v $
;;*
;;*	$Revision: 1.6 $
;;*
;;*	$Date: 2018/02/28 19:39:27 $
;;*
;;*	$Log: prologue.el,v $
;;*	Revision 1.6  2018/02/28 19:39:27  xinli
;;*	Add support for MemVerge.
;;*	
;;*	Revision 1.5  2013/07/03 18:10:11  xinli
;;*	Call replace-in-string in XEmacs, call replace-regexp-in-string in Emacs.
;;*
;;*	Revision 1.4  2013/06/12 20:54:08  xinli
;;*	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;*
;;*	Revision 1.3  2012/10/29 23:56:40  xinli
;;*	Replace replace-in-string with replace-regexp-in-string.
;;*
;;*	Revision 1.2  2010/01/21 04:16:11  lixin
;;*	Change for EMC-VMWare joint project Aurora.
;;*
;;*	Revision 1.1  2008/11/12 11:11:53  lixin
;;*	Join Morgan Stanley and EMC.
;;*
;; */

(defun shell-prologue (file-name)
  (shell-prologue-EMC file-name))

(defun shell-prologue-EMC (file-name)
  (concat "#! /bin/sh\n\n"
	  "# Copyright (C) "
	  (format-time-string "%Y")
	  ", All Rights Reserved, by\n"
	  "# EMC Corporation, Hopkinton, MA.\n"
	  "#\n"
	  "# This software is furnished under a license and may be used and copied\n"
	  "# only  in  accordance  with  the  terms  of such  license and with the\n"
	  "# inclusion of the above copyright notice. This software or  any  other\n"
	  "# copies thereof may not be provided or otherwise made available to any\n"
	  "# other person. No title to and ownership of  the  software  is  hereby\n"
	  "# transferred.\n"
	  "#\n"
	  "# The information in this software is subject to change without  notice\n"
	  "# and  should  not be  construed  as  a commitment by EMC Corporation.\n"
	  "#\n"
	  "# EMC assumes no responsibility for the use or  reliability  of its\n"
	  "# software on equipment which is not supplied by EMC.\n"
	  "#\n"))

;; Example

;;#! /bin/sh
;;
;;# Copyright (C) 2008, All Rights Reserved, by
;;# EMC Corporation, Hopkinton, MA.
;;#
;;# This software is furnished under a license and may be used and copied
;;# only  in  accordance  with  the  terms  of such  license and with the
;;# inclusion of the above copyright notice. This software or  any  other
;;# copies thereof may not be provided or otherwise made available to any
;;# other person. No title to and ownership of  the  software  is  hereby
;;# transferred.
;;#
;;# The information in this software is subject to change without  notice
;;# and  should  not be  construed  as  a commitment by EMC Corporation.
;;#
;;# EMC assumes no responsibility for the use or  reliability  of its
;;# software on equipment which is not supplied by EMC.
;;#

(defun shell-prologue-MorganStanley (file-name)
  (concat "#! /bin/sh\n\n"
	   "###############################################################################\n"
	  "##\n"
	  "## Copyright (c) "
	  (format-time-string  "%Y")
	  " Morgan Stanley & Co. Incorporated, All Rights Reserved\n"
	  "##\n"
	  "## Unpublished copyright.  All rights reserved.  This material contains\n"
	  "## proprietary information that shall be used or copied only within Morgan\n"
	  "## Stanley, except with written permission of Morgan Stanley.\n"
	  "##\n"
	  "###############################################################################\n"))

;; Example
;;#! /bin/sh
;;
;;###############################################################################
;;##
;;## Copyright (c) 2008 Morgan Stanley & Co. Incorporated, All Rights Reserved
;;##
;;## Unpublished copyright.  All rights reserved.  This material contains
;;## proprietary information that shall be used or copied only within Morgan
;;## Stanley, except with written permission of Morgan Stanley.
;;##
;;###############################################################################

(defun shell-prologue-eBay (file-name)
  (concat "#! /bin/sh\n#\n"
	  "#\tFile: " (file-name-nondirectory file-name) "\n"
	  "#\n"
	  "#\tAuthor: " (user-full-name) "\n"
	  "#\n"
	  "#\tDescription:\n#\n"
	  "#\tCreated on: "
	  (format "%s %s %s"
		  (format-time-string  "%Y/%m/%d %H:%M:%S")
		  (nth 1 (current-time-zone))
		  (format-time-string "%A %B %e"))
	  "\n"
	  "#\n"
	  "#\tLast modifed on: $" "Date$ \n"
	  "#\n"
	  "#\t$Revision" "$\n"
	  "#\n#\t$Log" "$"
	  "\n\n"))

;; Example
;;#! /bin/sh
;;#
;;#	File: Example.sh
;;#
;;#	Author: Andrew Li
;;#
;;#	Description:
;;#
;;#	Created on: 2008/03/19 09:54:51 HKT Wednesday March 19
;;#
;;#	Last modifed on: $Date: 2018/02/28 19:39:27 $
;;#
;;#	$Revision: 1.6 $
;;#
;;#	$Log: prologue.el,v $
;;#	Revision 1.6  2018/02/28 19:39:27  xinli
;;#	Add support for MemVerge.
;;#	
;;#	Revision 1.5  2013/07/03 18:10:11  xinli
;;#	Call replace-in-string in XEmacs, call replace-regexp-in-string in Emacs.
;;#
;;#	Revision 1.4  2013/06/12 20:54:08  xinli
;;#	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;#
;;#	Revision 1.3  2012/10/29 23:56:40  xinli
;;#	Replace replace-in-string with replace-regexp-in-string.
;;#
;;#	Revision 1.2  2010/01/21 04:16:11  lixin
;;#	Change for EMC-VMWare joint project Aurora.
;;#
;;#	Revision 1.1  2008/11/12 11:11:53  lixin
;;#	Join Morgan Stanley and EMC.
;;#
