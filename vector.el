
(load-file "list.el")
(load-file "complex.el")

(defconst vector/default-number-of-elements 5)

(defun vector/make-linspace-real (start stop &optional n accu)
  "Return linspace range from START to STOP, both included.

The optional parameter N specifies the number of elements in the range.
By default N is set to `vector/default-number-of-elements'.

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
(see `map-reverse')"
  (map-reverse 'complex/ify (vector/make-linspace-real start stop n)))

(defun vector/make-value-real (&optional n value accumulate)
  "Return vector filled by N VALUEs elements

By default N is set to `vector/default-number-of-elements',
and VAL is set to 1.


(see `push->')"
  (let ((acc accumulate)
        (num n)
        (val value)
        w)
    (when (eq val nil)
      (setq val 1.0))
    (when (eq num nil)
      (setq num vector/default-number-of-elements))
    (when (eq acc nil)
      (setq acc (list val)))
    (setq w val)
    (if (eq num 1)
        acc
      (vector/make-value-real (- num 1) w (push-> w acc)))
    ))

(defun vector/make-value (&optional n value)
  "Return complexified VALUEs.

(see `vector/make-value-real')
(see `complex/ify')
(see `map')"
  ;; `map-reverse' should be used, as in `vector/make-linspace'
  ;; but, since it is only VAL, it is an useless extra cost.
  (map 'complex/ify (vector/make-value-real n value)))

;; (defun vector/make-ones (&optional n value)
;;   (let ((val value))
;;     (when (eq value nil)
;;       (setq val 1))
;;     (map '(lambda (x) (complex/mul value x)) (vector/make-value-real n))
;;     ))

(defun vector/make-ones (&optional n)
  "Return complexified vector filled by 1..

(see `vector/make-value-real')
(see `complex/ify')
(see `map')"
  ;; `map-reverse' should be used, as in `vector/make-linspace'
  ;; but, since it is only VAL, it is an useless extra cost.
  (map 'complex/ify (vector/make-value-real n)))

(defun vector/make-ith-real (i &optional n accumulate)
  "Return a vector of length N filled by zeros,
except at the Ith which is filled by 1.

Index I starts at one.

N is set by default to `vector/default-number-of-elements'
ACCUMULATE is optionnal and set by default to `nil'.


Example:

(vector/make-ith-real 2 7)
--> (0 1 0 0 0 0 0)

(see `push->')"
  (let ((acc accumulate)
        (num n)
        (val 0))
    (when (eq num nil)
      (setq num vector/default-number-of-elements))
    (when (eq i num)
      (setq val 1))
    (when (eq acc nil)
      (setq acc '()))
    (setq acc (push-> val acc))
    (if (eq num 1)
        acc
      (vector/make-ith-real i (- num 1) acc))
    ))

(defun vector/make-ith (i &optional n)
  (map-reverse 'complex/ify (vector/make-ith-real i n)))


(defun vector/make-ith-val-real (i val &optional n accumulate)
  "Return a vector of length N filled by zeros,
except at the Ith which is filled by 1.

Index I starts at one.

N is set by default to `vector/default-number-of-elements'
ACCUMULATE is optionnal and set by default to `nil'.


Example:

(vector/make-ith-real 2 7)
--> (0 1 0 0 0 0 0)

(see `push->')"
  (let ((acc accumulate)
        (num n)
        (vall 0))
    (when (eq num nil)
      (setq num vector/default-number-of-elements))
    (when (eq i num)
      (setq vall val))
    (when (eq acc nil)
      (setq acc '()))
    (setq acc (push-> vall acc))
    (if (eq num 1)
        acc
      (vector/make-ith-val-real i val (- num 1) acc))
    ))

(defun vector/make-ith-val (i val &optional n)
  (map-reverse 'complex/ify (vector/make-ith-val-real i val n) t))


(defun vector/dot-ouch (x y &optional accu)
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
      (vector/dot-ouch xtail ytail val))
  ))

(defun vector/dot (x y)
  "Compute by Map/Reduce the inner product and return it as `complex'.

Work for all datatypes supported by `complex'.

If the lists X and Y does not have the same length (see `get-length'),
then no one warning is raised,
and the returned value corresponds to the inner prodcut of the common length.

(see `map' and `reduce')


Example:

(setq x (vector/make-linspace 1 5))
(setq y (vector/make-linspace -5 -1))
(vector/dot x y)"
  (reduce 'complex/add
          (map 'complex/mul (combine-reversed x y))))

(defun vector/abs (vec)
  (map-reverse 'complex/abs vec t))


(defun vector/norm2 (x)
  (vector/dot x x))

(defun vector/norm (x)
  (math/sqrt (complex/real (vector/norm2 x))))

(defun vector/set-i (vec i value &optional accumulate current)
  (let ((acc accumulate)
        (cur current)
        (val (car vec)))

    (when (eq cur nil)
      (setq cur 1))

    (when (eq i cur)
      (setq val value))

    (setq acc (push-> val acc))

    (if (eq (cdr vec) nil)
        (map 'complex/ify acc t)
      (vector/set-i (cdr vec) i value acc (+ cur 1)))
    ))

(defun vector/get-i (vec index)
  (get-ith vec index))

(defun vector/map (func x)
  ;; incosistent between list.el and here.
  ;; Still because map is a better name ?
  (reverse-map func x t))


(defun vector/map-binary (func x y)
  (reverse-map-binary func x y t))

(defun vector/add (x y)
  (vector/map-binary 'complex/add x y))

(defun vector/sub (x y)
  (vector/map-binary 'complex/sub x y))

(provide 'vector)
