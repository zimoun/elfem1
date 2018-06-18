

(defun complex (real imag)
  "Return the list of 2 elements.

The first element is REAL.
The second element is IMAG.
Both are converted to float datatype.

i.e., REAL _is_ (car (complex (REAL IMAG)))
and IMAG _is_ (car (cdr (complex REAL IMAG))).

(see `real' and `imag')
"
  (list (float real) (float imag)))

(defun complex/which-part (cplx i)
  "Generic function used by `real' or `imag'.

Robust enough to even work with interger and float datatype."
  ;; instead to check using `type-of'
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
  "Return imaginary part. (see `complex')"
  (complex/which-part cplx 1))

(defun conj (cplx)
  "Return the conjugate of CPLX. (see `complex')"
  (let ((a (real cplx))
        (b (imag cplx)))
    (complex a (- b))
    ))

(defun complex/add (a b)
  "Return the complex addition: A+B.

Note that this obviously commutes.
(complex/add a b) _equivalent_ (complex/add b a)

(see `complex')"
  (let ((ar (real a))
        (ai (imag a))
        (br (real b))
        (bi (imag b)))
    (complex (+ ar br) (+ ai bi))
    ))

(defun complex/sub (a b)
  "Return the complex substraction: A-B.

(see `complex')"
  (let ((br (real b))
        (bi (imag b)))
    (complex/add a (complex (- br) (- bi)))
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

(defun complex/cabs2 (cplx)
  "Return the complex squared modulus.

(see `complex' and `complex/mul' and `conj')"
  (complex/mul cplx (conj cplx)))

(defun complex/abs2 (cplx)
  "Return the squared modulus.

(+ `real'^2 `imag'^2)

Note that real^2 and imag^2 do not make any sense in Lisp.

(see `complex' and `complex/cabs2')"
  (real (complex/cabs2 cplx)))

(defun complex/abs (cplx)
  "Return the modulus.

(`sqrt' (+ `real'^2 `imag'^2))

Note that real^2 and imag^2 do not make any sense in Lisp.

(see `complex' and `complex/abs2')"
  (sqrt (complex/abs2 cplx)))

(defun complex/div (a b)
    "Return the complex multiplication: A/B.

(see `complex')."
  (let ((num (complex/mul a (conj b)))
        (iden (/ 1 (complex/abs2 b))))
    (complex/mul num iden)
    ))

(defun complex/pow (cplx n &optional accu)
  "Compute CPLX power N.

ACCU is set to 1 by default.
It corresponds to the value of the tail-recursion.
Even if Emacs Lisp does not optimize the tail-recursion.

(see `complex' and `complex/mul')"
  (let (acc)
    (if (eq nil accu)
        (setq acc (complex 1 0))
      (setq acc accu))
    (if (equal n 0)
        acc
      (complex/pow cplx (- n 1) (complex/mul cplx acc)))
    ))


(provide 'complex)
