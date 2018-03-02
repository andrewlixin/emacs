;;
;;	File:		cron.el
;;
;;	Description:	clock daemon function
;;
;;	Copyright:	Copyright (c) 2005
;;
;;	Author:		Li Xin
;;
;; 	Created on:	2005/01/06 14:03:39 EAT Thursday January  6
;;
;;	Last modifed on: $Date: 2013/06/12 20:54:08 $
;;
;;	$RCSfile: cron.el,v $
;;
;;	$Revision: 1.5 $
;;
;;	$Log: cron.el,v $
;;	Revision 1.5  2013/06/12 20:54:08  xinli
;;	Cleanup: Remove unsed files, remove blank lines and trailing whitespaces.
;;	
;;	Revision 1.4  2008/11/12 11:09:26  lixin
;;	Join Morgan Stanley and EMC in Mar. 2008 and Oct. 2008.
;;
;;	Revision 1.3  2005/01/06 06:26:22  lixin
;;	Add CVS header
;;

(defvar cronjob nil)
(defvar cron-timer nil)
(defvar last-minute -1)

(defun have-a-rest ()
  (let* ((time (decode-time))
	 (minute (nth 1 time))
	 (hour (nth 2 time)))
    (pop-up-message (format "Current time is %02s:%02s.%sHave a rest." hour minute (generate-tab-space)))))

(defun pop-up-message (message &optional user)
  (let ((cmd)
	(args))
    (unless user
      (setq user (user-login-name)))
    (if (equal system-type 'windows-nt)
	(setq cmd "cmd"
	      args (format "/c net send %s" user))
      (setq cmd "sendmsg"
	    args ""))
    (start-process "sendmsg" nil cmd args message)))

(defun delete-cron-job ( cronjob-to-delete )
  (let* ((old cronjob)
	 (element (car old))
	 (new))
    (while element
      (unless (equal cronjob-to-delete element)
	(pushnew element new))
      (setq old (cdr old))
      (setq element (car old)))
    (setq cronjob new)))

(defun cron-job-match (job minute hour day month week)
  (let ((min (nth 0 job))
	(h   (nth 1 job))
	(d   (nth 2 job))
	(mon (nth 3 job))
	(w   (nth 4 job))
	(wild '*))
    (if (and (or (eq min wild) (= min minute))
	     (or (eq h wild)   (= h hour))
	     (or (eq d wild)   (= d day))
	     (or (eq mon wild) (= mon month))
	     (or (eq w wild)   (= w week)))
	t
      nil)))

(defun do-cron-job ()
  (interactive)
  (let* ((current-time    (decode-time))
	 (minute          (nth 1 current-time))
	 (hour            (nth 2 current-time))
	 (day             (nth 3 current-time))
	 (month           (nth 4 current-time))
	 (week            (nth 6 current-time))
	 (cron-jobs       cronjob)
	 (current-job      nil))
    (when (/= last-minute minute)
      (setq last-minute minute)
      (while cron-jobs
	(setq current-job (car cron-jobs))
	(setq cron-jobs (cdr cron-jobs))
	(if (and (>= (length current-job) 6)
		 (cron-job-match current-job minute hour day month week))
	    (if (nth 6 current-job)
		(funcall (nth 5 current-job) (nth 6 current-job))
	      (funcall (nth 5 current-job))))))))

(defun start-cronjob ()
  (interactive)
  (when (and (null cron-timer)
	     (not (file-exists-p "~/.cronlock")))
    (setq cron-timer (start-itimer "cronjob" 'do-cron-job 55 55 nil nil))
    (let ((buffer (find-file "~/.cronlock")))
      (set-buffer-modified-p t buffer)
      (save-buffer buffer)
      (kill-buffer buffer))
    (add-hook 'kill-emacs-hook 'stop-cronjob)))

(defun stop-cronjob ()
  (interactive)
  (when cron-timer
    (delete-itimer cron-timer)
    (setq cron-timer  nil)
    (delete-file "~/.cronlock")))

(defun mail-send-in-cron ()
  (let ((mail-buffer (find-buffer-by-name "*mail*"))
	(current-buffer (current-buffer)))
    (when mail-buffer
      (switch-to-buffer mail-buffer)
      (set-buffer-modified-p t mail-buffer)
      (mail-send)
      (switch-to-buffer current-buffer))))

(pushnew '(45 11 * * * pop-up-message "It's time to have lunch") cronjob)
(pushnew '(55 17 * * * pop-up-message "Get ready to go home") cronjob)
(pushnew '(00 18 * * * pop-up-message "It's time to go home") cronjob)

(pushnew '(45 9  * * * have-a-rest) cronjob)
(pushnew '(00 11 * * * have-a-rest) cronjob)
(pushnew '(50 13 * * * have-a-rest) cronjob)
(pushnew '(00 15 * * * have-a-rest) cronjob)
(pushnew '(00 16 * * * have-a-rest) cronjob)
(pushnew '(00 17 * * * have-a-rest) cronjob)
(pushnew '(00 19 * * * have-a-rest) cronjob)
(pushnew '(00 20 * * * have-a-rest) cronjob)
(pushnew '(00 21 * * * have-a-rest) cronjob)
(pushnew '(00 22 * * * have-a-rest) cronjob)

;(pushnew '(XX XX * * * mail-send-in-cron) cronjob)

(start-cronjob)
