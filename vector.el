(defvar vector/default-number-of-elements 5)

(defun vector/make-linspace-real (start stop &optional n accu)
  "Return linspace range from START to STOP, both included.

The optional parameter N specifies the number of elements in the range.
By default N is set to 5.

(see `push->')"
  (let ((acc accu)
        num
        step newstop)
    (when (eq nil accu)
      (setq acc (list stop)))
    (if (eq nil n)
        (setq num (float vector/default-number-of-elements))
      ;; be careful by the true division:
      ;; (/ 1 2) returns 0 but (/ 1 2.0) return 0.5
      (setq num (float n)))

    (setq step (/ (- stop start) (- num 1)))
    (setq newstop (- stop step))

    (if (>= start stop)
        acc
      (vector/make-linspace-real start newstop (- num 1) (push-> newstop acc)))
    ))

(defun vector/make-linspace (start stop &optional n)
  "Return complexified linspace range.

(see `vector/make-linspace-real')
(see `complex/ify')
(see `map')"
  (map-reverse 'complex/ify (vector/make-linspace-real start stop n)))

(defun vector/dot (x y &optional accu)
  "Compute the inner product and return it as `complex'.

Work for all datatypes supported by `complex'.

This function walks recursively through the nil-terminated lists X and Y.
If the lists X and Y does not have the same length (see `get-length'),
then no one warning is raised,
and the returned value corresponds to the inner prodcut of the common length."
  (let ((acc accu)
        (xtail (cdr x))
        (ytail (cdr y))
        (xhead (car x))
        (yhead (car y))
        val)
    (when (eq nil accu)
      (setq acc 0))

    (setq val (complex/add acc (complex/mul xhead yhead)))

    (if (or
         (eq nil xtail)
         (eq nil ytail))
        val
      (vector/dot xtail ytail val))
  ))

(defun vector/dott (x y)
  (reduce 'complex/add
          (map 'complex/mul (combine-reversed nil x y))))

(defun vector/abs (vec)
  (map 'complex/abs vec))


(defun vector/norm2 (x)
  (vector/dot x x))

(defun vector/norm (x)
  (math/sqrt (vector/norm2 x)))

(provide 'vector)
