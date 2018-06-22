
;;  emacs -Q -batch -L . -l makefile.elc -e do/all


(defmacro def:do (name msg &rest code)
  (let ((fun (intern (concat "do/" (symbol-name name)))))
    `(defun ,fun ()
       (interactive)
       (let ((that (make-progress-reporter (concat ,msg "..."))))
         ,@code
         (progress-reporter-done that))
    )))


(def:do this
     "This"
     (message "line 1")
     (message "line 2")
     (message "line 3")
     )

(def:do all
  "All"
  (do/compile)
  (message " ")
  (do/test))

(def:do compile
  "Compiling"
  (byte-recompile-directory "." 0)
  (message ""))


(def:do test
  "Testing"
  (require 'ert)
  (require 'tests)
  (if noninteractive
      (ert-run-tests-batch)
    (ert-run-tests-interactively t)))


(def:do clean
  "Cleanning"
  (mapc 'delete-file
        (directory-files "./" nil ".+\\.elc$")))



;; (defun do/all ()
;;   (interactive)
;;   (let ((that (make-progress-reporter "All...")))
;;     (do/compile)
;;     (message "")
;;     (do/test)
;;     (progress-reporter-done that)))

(defun redo/all ()
  (interactive)
  (do/clean)
  (do/all))

;; (defun do/compile ()
;;   (interactive)
;;   (let ((that (make-progress-reporter "Compiling...")))
;;     (byte-recompile-directory "." 0)
;;     (message "")
;;     (progress-reporter-done that)))

(defun redo/compile ()
  (interactive)
  (do/clean)
  (do/compile))


;; (defun do/test ()
;;   (interactive)
;;   (let ((that (make-progress-reporter "Testing...")))
;;     (require 'ert)
;;     (require 'tests)
;;     (if noninteractive
;;         (ert-run-tests-batch)
;;       (ert-run-tests-interactively t))
;;     (progress-reporter-done that)))


;; (defun do/clean ()
;;   (interactive)
;;   (let ((that (make-progress-reporter "Cleaning...")))
;;     (mapc 'delete-file
;;           (directory-files "./" nil ".+\\.elc$"))
;;     (progress-reporter-done that)))
