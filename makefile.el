
;;  emacs -Q -batch -L . -l makefile.elc -e do/all

(defun do/all ()
  (interactive)
  (let ((that (make-progress-reporter "All...")))
    (do/compile)
    (message "")
    (do/test)
    (progress-reporter-done that)))

(defun redo/all ()
  (interactive)
  (do/clean)
  (do/all))

(defun do/compile ()
  (interactive)
  (let ((that (make-progress-reporter "Compiling...")))
    (byte-recompile-directory "." 0)
    (message "")
    (progress-reporter-done that)))

(defun redo/compile ()
  (interactive)
  (do/clean)
  (do/compile))


(defun do/test ()
  (interactive)
  (let ((that (make-progress-reporter "Testing...")))
    (require 'ert)
    (require 'tests)
    (if noninteractive
        (ert-run-tests-batch)
      (ert-run-tests-interactively t))
    (progress-reporter-done that)))


(defun do/clean ()
  (interactive)
  (let ((that (make-progress-reporter "Cleaning...")))
    (mapc 'delete-file
          (directory-files "./" nil ".+\\.elc$"))
    (progress-reporter-done that)))
