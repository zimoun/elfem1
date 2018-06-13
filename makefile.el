
;;  emacs -Q -batch -L . -l makefile.elc -e do/all

(defun do/all ()
  (interactive)
  (do/compile)
  (do/test))

(defun do/compile ()
  (interactive)
  (byte-recompile-directory "." 0))

(defun do/test ()
  (interactive)
  (require 'ert)
  (require 'tests)
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert-run-tests-interactively t)))


(defun do/clean ()
  (interactive)
  (mapc 'delete-file
        (directory-files "./" nil ".+\\.elc$")))
