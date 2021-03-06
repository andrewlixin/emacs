(defvar system-path-separator "/"
  "Default file separator for Mac OS X")

(setq default-c-compiler
      (locate-file "cc" exec-path)
      default-c-compiler-flag "-g -Wall"
      default-c++-compiler-flag
      (locate-file "CC" exec-path)
      default-c++-compiler-flag "-g -Wall"
      default-java-compiler "javac"
      default-java-compiler-flag "-g"
      default-make "make")

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
