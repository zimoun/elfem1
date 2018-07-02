
;;  emacs -Q -batch -L . -l makefile.elc -e do/all


(defmacro def:do (name msg &rest code)
  (let ((fun (intern (concat "do/" (symbol-name name)))))
    `(defun ,fun ()
       (interactive)
       (let ((that (make-progress-reporter (concat ,msg "..."))))
         ,@code
         (progress-reporter-done that))
    )))

(defmacro def:re (name &rest code)
  (let ((fun (intern (concat "re" (symbol-name name)))))
    `(defun ,fun ()
       (interactive)
       (do/clean)
       (,name)
       ,@code
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


(def:re do/all)
(def:re do/compile)
