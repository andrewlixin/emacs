init:
	@- test -f ${HOME}/.emacs && ${RM} ${HOME}/.emacs
	ln -s ${HOME}/emacs/emacs ${HOME}/.emacs
	@- test -d ${HOME}/.xemacs && ${RM} -r ${HOME}/.xemacs
	ln -s ${HOME}/emacs/xemacs ${HOME}/.xemacs

