
(defun complex (real imag)
  "Return the list of 2 elements.

The first element is REAL.
The second element is IMAG.

i.e., REAL _is_ (car (complex (REAL IMAG)))
and IMAG _is_ (car (cdr (complex REAL IMAG))).

(see `real' and `imag')
"
  (list (float real) (float imag)))

(defun complex/which-part (cplx i)
  "See `real' or `imag'.

Refactorization."
  ;; instead to compare using `type-of'
  ;; should be used: `consp' or `intergerp' or `floatp'
  (let ((typeof (type-of cplx))
        val)
    (cond
     ((eq typeof 'cons)
      (nth i cplx))
     ((or
       (eq typeof 'integer)
       (eq typeof 'float))
      (if (= i 0)
          (float cplx)
        0.0))
     )))

(defun real (cplx)
  "Return real part. (see `complex')"
  (complex/which-part cplx 0))

(defun imag (cplx)
  "Return imag part. (see `complex')"
  (complex/which-part cplx 1))


(defun complex/add (a b)
  "Return the complex addition: A+B.

Note that this obviously commutes.
(complex/add a b) _equivalent_ (complex/add b a)

(see `complex')"
  (let ((ar (real a))
        (ai (imag a))
        (br (real b))
        (bi (imag b))
        )
    (complex (+ ar br) (+ ai bi))
    ))

(defun complex/mul (a b)
  "Return the complex multiplication: A*B.

Note that this obviously commutes.
(complex/mul a b) _equivalent_ (complex/mul b a)

(see `complex')
"
  (let ((ar (real a))
        (ai (imag a))
        (br (real b))
        (bi (imag b))
        )
    (complex (- (* ar br) (* ai bi))
          (+ (* ar bi) (* ai br)))
    ))


(provide 'complex)
