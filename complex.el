

(defun complex (real imag)
  "Return the dotted paired list of 2 elements.

The first element is REAL.
The second element is IMAG.
Both are converted to float datatype.

i.e., REAL _is_ (car (complex (REAL IMAG)))
and IMAG _is_ (cdr (complex REAL IMAG))).

(see `real' and `imag')"
  (cons (float real) (float imag)))

;; (defun complex/ify (real)
;;   "See `complex'"
;;   (complex real 0))

(defun complex? (number)
    (let ((typeof (type-of number)))
    (cond
     ((eq typeof 'cons)
      t)
     ((or
      (eq typeof 'integer)
      (eq typeof 'float))
      nil)
    )))

(defalias 'complex-p 'complex?)

(defun complex/ify (number)
  (if (complex-p number)
      number
    (complex number 0)))

(defun complex/ify--trigo (number)
  "Return NUMBER as `complex' with imag-part only.

If NUMBER is already `complex',
then if real-part of NUMBER is smaller than 0 (`complex/numerical-zero'),
     then NUMBER becomes `complex' 0 real-part,
     else NUMBER becomes `complex' 0 imag-part;
else NUMBER is complexified with imag-part.

Therefore, be careful when the real-part and the imag-part of x are both greater than 0.
"
  (let (real imag x y)
    (if (complex-p number)
        (progn
          (setq real (complex/real number))
          (setq imag (complex/imag number))
          (if (< (math/abs real) complex/numerical-zero)
              (setq x imag)
            (setq x real)))
      (setq x number))
    ;; Add modulo
    (if (> x math/pi)
        (setq y (- x (* 2 math/pi)))
      (setq y x))
    y
    ))


(defun complex/which-part (cplx i)
  "Generic function used by `real' or `imag'.

Robust enough to even work with interger and float datatype."
  ;; instead to check using `type-of'
  ;; should be used: `consp' or `intergerp' or `floatp'
  (let ((typeof (type-of cplx)))
    (cond
     ((eq typeof 'cons)
      (if (= i 0)
          (car cplx)
        (cdr cplx)))
     ((or
       (eq typeof 'integer)
       (eq typeof 'float))
      (if (= i 0)
          (float cplx)
        0.0))
     )))

(defun complex/real (cplx)
  "Return real part. (see `complex')"
  (complex/which-part cplx 0))

(defun complex/imag (cplx)
  "Return imaginary part. (see `complex')"
  (complex/which-part cplx 1))

(defun complex/conj (cplx)
  "Return the conjugate of CPLX. (see `complex')"
  (let ((a (complex/real cplx))
        (b (complex/imag cplx)))
    (complex a (- b))
    ))

(defun complex/add (a b)
  "Return the complex addition: A+B.

Note that this obviously commutes.
(complex/add a b) _equivalent_ (complex/add b a)

(see `complex')"
  (let ((ar (complex/real a))
        (ai (complex/imag a))
        (br (complex/real b))
        (bi (complex/imag b)))
    (complex (+ ar br) (+ ai bi))
    ))

(defun complex/sub (a b)
  "Return the complex substraction: A-B.

(see `complex')"
  (let ((br (complex/real b))
        (bi (complex/imag b)))
    (complex/add a (complex (- br) (- bi)))
    ))

(defun complex/mul (a b)
  "Return the complex multiplication: A*B.

Note that this obviously commutes.
(complex/mul a b) _equivalent_ (complex/mul b a)

(see `complex')
"
  (let ((ar (complex/real a))
        (ai (complex/imag a))
        (br (complex/real b))
        (bi (complex/imag b))
        )
    (complex (- (* ar br) (* ai bi))
          (+ (* ar bi) (* ai br)))
    ))

(defun complex/cabs2 (cplx)
  "Return the complex squared modulus.

(+ `real'^2 `imag'^2) = (* CPLX (`conj' CPLX))

Note that real^2 and imag^2 do not make any sense in Lisp.

(see `complex' and `complex/mul' and `conj')"
  (complex/mul cplx (complex/conj cplx)))

(defun complex/abs2 (cplx)
  "Return the real squared modulus.

(see `complex/cabs2' and `real')"
  (complex/real (complex/cabs2 cplx)))

(defun complex/abs-naive (cplx)
  "Return the modulus.

Evaluate (`math/sqrt' `complex/abs2')

Note that the recursive computation of the square root is always done,
even if CPLX is purely `real' or purely `imag'-inary.
(see `complex/abs')"
  (math/sqrt (complex/abs2 cplx)))

(defconst complex/numerical-zero 1e-5
  "Fix the value of number that are skipped,
because they are considered as numerical noise.")

(defun complex/abs (cplx)
  "Return the modulus.

If `real' or `imag' are less than `complex/numerical-zero'
Then return the well-adapted absolute value computed by `math/abs',
Else apply `complex/abs-naive'."
  (let ((re (math/abs (complex/real cplx)))
        (im (math/abs (complex/imag cplx))))
    (cond
     ((< im complex/numerical-zero)
      re)
     ((< re complex/numerical-zero)
      im)
     ((and
       (>= im complex/numerical-zero)
       (>= re complex/numerical-zero))
      (complex/abs-naive cplx)))
    ))

(defun complex/div (a b)
    "Return the complex multiplication: A/B.

(see `complex')."
  (let ((num (complex/mul a (complex/conj b)))
        (inv-den (/ 1 (complex/abs2 b))))
    (complex/mul inv-den num)
    ))

(defun complex/pow (cplx n &optional accumulate)
  "Compute CPLX power N.

ACCUMULATE is set to 1 by default.
It corresponds to the value of the tail-recursion.
Even if Emacs Lisp does not optimize the tail-recursion.

WARNING:
Even if Emacs is not optimized --at all-- for computations of loop-recursion.
Try e.g., (complex/pow (complex 1 0) (\ max-lisp-eval-depth 3))
or decrease 3 a bit, and then depth will exceed `max-lisp-eval-depth'.

(see `complex' and `complex/mul')"
  (let ((acc accumulate))
    (when (eq nil accumulate)
        (setq acc (complex 1 0)))
    (if (= n 0)
        acc
      (complex/pow cplx (- n 1) (complex/mul cplx acc)))
    ))

(defvar complex/exp-tolerance 1e-6
  "Fix the maximal aboslute smallest residual of `complex/exp' (serie convergence)")

(defun complex/exp (cplx &optional tolerance nth current)
  (let ((cur current)
        (n nth)
        (tol tolerance)
        nth-term)

    (when (eq current nil)
      (setq cur (complex 0 0)))
    (when (eq nth nil)
      (setq n 0))
    (when (eq tolerance nil)
      (setq tol complex/exp-tolerance))

    (setq nth-term (complex/div
                    (complex/pow cplx n)
                    (math/fac n)))

    (setq cur (complex/add cur nth-term))

    (if (< (complex/abs nth-term) tol)
        cur
       (complex/exp cplx tol (+ n 1) cur))
    ))

(defun complex/sin (x &optional tolerance nth current)
  "Return the complexified sine of X.
"
  (complex/ify
   (complex/imag
    (complex/exp
     (complex/ify--trigo x)
     tolerance nth current))))

(defun complex/cos (x &optional tolerance nth current)
  "Return the complexified cosine of X.
"
  ;; FIXME: do not work for x > 5 ??
  (complex/ify
   (complex/real
    (complex/exp
     (complex/ify--trigo x)
     tolerance nth current))))

(defun one? (x)
  (let* ((cosine (complex/cos x))
         (sine (complex/sin x))
         (cos2 (complex/mul cosine cosine))
         (sin2 (complex/mul sine sine))
         (one (complex/add cos2 sin2))
         (zero (complex/sub one 1)))
    (if (< (complex/abs zero) complex/numerical-zero)
        t
      nil)
    ))


;; Require the library s.el
;; Try install it: M-x package-install s
;; (require 's)
(defun pprinter (x)
  (let ((real (complex/real x))
        (imag (complex/imag x)))
    (s-lex-format "${real} + ${imag}i")))


(provide 'complex)
