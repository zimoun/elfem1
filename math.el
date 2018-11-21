(defvar math/pi 3.141592653589793)
(defvar math/exp 2.718281828459045)

(defun math/abs (number)
  "Return absolute value of the NUMBER.

If NUMBER is greater than zero, then the function returns NUMBER,
Else, the function return the converted -NUMBER to `float'"
  (if (>= number 0)
      number
    (- 0.0 number)))

(defvar math/sqrt-tolerance 1e-4
  "Fix the maximal aboslute gap between two computations of `math/sqrt-recursive-lisp'")

(defun math/sqrt-recursive-lisp (number &optional tolerance current)
  "Return the square root of the NUMBER.

Pure Lisp implementation, using recursively a fixed-point algorithm to solve: x^2=number,
i.e., recursively updates by (/ (+ current number) (+ current 1.0))
until the reached TOLERANCE.

The optionnal parameter TOLERANCE is set by default to `math/sqrt-tolerance'.
CURRENT is by default set to NUMBER.

WARNING:
(see `max-lisp-eval-depth' and `max-specpdl-size')

The number of loop-recursion is limited (see elisp specifications).
Therefore, this function cannot evaluate the square root of number greater
than approximatively (/ max-specpdl-size 3).
It is then recommended to use `math/sqrt' instead."
  (let ((cur current)
        (tol tolerance)
        update)

    (when (eq current nil)
      (setq cur number))
    (when (eq tolerance nil)
      (setq tol math/sqrt-tolerance))

    (setq update (/ (+ cur number) (+ cur 1.0)))

    (if (< (math/abs (- update cur)) tol)
        update
      (math/sqrt-recursive-lisp number tol update))
    ))

(defun math/sqrt (number &optional built-in)
  "Return the square root of the NUMBER

If NUMBER is greater than (/ max-specpdl-size 3),
then it applies the built-in C function `sqrt',
else it uses the pure lisp implementation `math/sqrt-recursive-lisp'.

If BUILT-IN is not `nil', then `sqrt' is applied for any NUMBER."
  (let ((val 0))
    (when (eq built-in nil)
        (setq val (/ max-specpdl-size 3)))

    (if (> number val)
        (sqrt number)
      (math/sqrt-recursive-lisp number))
    ))


(defun math/pow-int (number power &optional accumulate)
  (let ((acc accumulate))
    (when (eq accumulate nil)
      (setq acc 1.0))
    (if (<= power 0)
        acc
      (math/pow-int number (- power 1) (* number acc)))
    ))

(defun math/exp (number))
