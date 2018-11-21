
(defun matrix/make-ones (&optional size accumulate rows)
  (let ((ncol size)
        (nrow rows)
        (acc accumulate))
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
        (setq nrow ncol))

    (setq acc (push-> (vector/make-ones ncol) acc))

    (if (= nrow 1)
        acc
      (matrix/make-ones ncol acc (- nrow 1)))
    ))

(defun matrix/make-eye (&optional size accumulate rows)
  (let ((ncol size)
        (nrow rows)
        (acc accumulate))
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
        (setq nrow ncol))

    (setq acc (push-> (vector/make-ith nrow ncol) acc))

    (if (= nrow 1)
        acc
      (matrix/make-eye ncol acc (- nrow 1)))
    ))

(defun matrix/vector-dot (mat vec &optional accumulate)
  (let ((acc accumulate)
        (wec (car mat))
        (nat (cdr mat)))
    (setq acc (push-> (vector/dot wec vec) acc))
    (if (eq nat nil)
        (reverse-recursive-lisp acc)
      (matrix/vector-dot nat vec acc))
  ))
