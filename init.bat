copy emacs c:\.emacs

xcopy /s/e/i/exclude:initexclude.txt xemacs c:\.xemacs

setenv -e HOME C:\

attrib +h c:\.emacs

attrib +h c:\.xemacs

