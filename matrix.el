
(require 'list)
(require 'complex)
(require 'vector)

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

(defun matrix/make-laplacian (&optional size accumulate rows)
  (let ((ncol size)
        (nrow rows)
        (acc accumulate)
        vec)
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
        (setq nrow ncol))

    (setq vec (vector/make-ith-val nrow 2 ncol))
    (when (< nrow ncol)
      (setq vec (vector/set-i vec (+ nrow 1) -1)))
    (when (> nrow 1)
      (setq vec (vector/set-i vec (- nrow 1) -1)))
    (setq acc (push-> vec acc))

    (if (= nrow 1)
        acc
      (matrix/make-laplacian ncol acc (- nrow 1)))
    ))

(defun matrix/make-laplacian-complex (&optional size accumulate rows)
  "Need refactorization"
  (let ((ncol size)
        (nrow rows)
        (acc accumulate)
        vec)
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
        (setq nrow ncol))

    (setq vec (vector/make-ith-val nrow (complex 2 2) ncol))
    (when (< nrow ncol)
      (setq vec (vector/set-i vec (+ nrow 1) (complex -1 -1))))
    (when (> nrow 1)
      (setq vec (vector/set-i vec (- nrow 1) (complex -1 -1))))
    (setq acc (push-> vec acc))

    (if (= nrow 1)
        acc
      (matrix/make-laplacian-complex ncol acc (- nrow 1)))
    ))


(defun matrix/make-linspace-diag (&optional size accumulate rows)
  (let ((ncol size)
        (nrow rows)
        (acc accumulate))
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
        (setq nrow ncol))

    (setq acc (push-> (vector/make-ith-val nrow nrow ncol) acc))

    (if (= nrow 1)
        acc
      (matrix/make-linspace-diag ncol acc (- nrow 1)))
    ))

(defun matrix/make-linspace (&optional size accumulate rows offset)
  (let ((ncol size)
        (nrow rows)
        (acc accumulate)
        (off offset))
    (when (eq nil size)
      (setq ncol vector/default-number-of-elements))
    (when (eq nil rows)
      (setq nrow ncol))
    (when (eq off nil)
      (setq off (+ (* ncol ncol) 1)))

    (setq acc (push-> (vector/make-linspace (- off ncol) (- off 1) (+ ncol 0)) acc))

    (if (= nrow 1)
        acc
      (matrix/make-linspace ncol acc (- nrow 1) (- off ncol)))
    ))


(defun matrix/vector-cat (mat vec &optional accumulate)
  (let ((acc accumulate)
        (nat (cdr mat))
        (val (car vec)))
    (setq acc (push-> (push<- (car mat) val) acc))

    (if (eq nat nil)
        (reverse acc)
      (matrix/vector-cat nat (cdr vec) acc))
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

(defun matrix/get-ij (matrix i j)
  (let ((irow (get-ith matrix i)))
    (get-ith irow j)
    ))

(defun matrix/set-ij (matrix i j value &optional accumulate current)
  (let ((acc accumulate)
        (cur current)
        (vec (car matrix)))

    (when (eq cur nil)
      (setq cur 1))

    (when (eq cur i)
      (setq vec (vector/set-i vec j value)))

    (setq acc (push-> vec acc))

    (if (eq (cdr matrix) nil)
        (reverse acc)
      (matrix/set-ij (cdr matrix) i j value acc (+ cur 1)))
  ))

(defun matrix/square? (A &optional number-row number-col)
  (let ((nrow number-row)
        (ncol number-col)
        (ntmp (get-length (car A))))
    (when (eq nrow nil)
      (setq nrow 1))
    (when (eq ncol nil)
      (setq ncol ntmp))

    (if (not (eq ncol ntmp))
        nil
      (if (eq (cdr A) nil)
          (if (eq nrow ncol)
              t
            nil)
        (matrix/square? (cdr A) (+ nrow 1) ncol)))
    ))

(defalias 'matrix/square-p 'matrix/square?)

(defun matrix/vector-solve (A b)
  (let ((Mat (matrix/vector-cat A b))
        den num scale
        (nrow (get-length b))
        x y z
        i j k)
    (if (not (matrix/square? A))
        (vector/make-value nrow 0)
      (if (not (eq nrow (get-length (car A))))
          (vector/make-value nrow 0)
        (progn
          (dotimes (kcount (- nrow 1))
            (setq k (+ kcount 1))
            (progn
              (setq den (matrix/get-ij Mat k k))
              (setq i (+ k 1))
              (dotimes (icount (- (+ nrow 1) (+ k 1)))
                (progn
                  (setq num (matrix/get-ij Mat i k))
                  (setq scale (complex/div num den))
                  (setq j k)
                  (dotimes (jcount (- (+ nrow 2) k))
                    (progn
                      (setq Mat (matrix/set-ij Mat i j
                                               (complex/sub
                                                (matrix/get-ij Mat i j)
                                                (complex/mul
                                                 scale
                                                 (matrix/get-ij Mat k j))
                                                )))
                      (setq j (+ j 1))
                      ))
                  (setq i (+ i 1)))
                ))))))

    (setq x (vector/make-value nrow 0))

    (setq scale (complex/div
             (matrix/get-ij Mat nrow (+ nrow 1))
             (matrix/get-ij Mat nrow nrow)))

    (setq x (vector/set-i x nrow scale))

    (setq i (- nrow 1))
    (dotimes (icount (- nrow 1))
      (progn
        (setq y (matrix/get-ij Mat i (+ nrow 1)))
        (setq j (+ i 1))
        (dotimes (jcount (- nrow i))
          (progn
            (setq y
                  (complex/sub y
                               (complex/mul
                                (matrix/get-ij Mat i j)
                                (vector/get-i x j))
                               ))
            (setq j (+ j 1))))
        (setq y (complex/div y (matrix/get-ij Mat i i)))
        (setq x (vector/set-i x i y))
        (setq i (- i 1))
        ))
      x
    ))


(defun matrix/solver-residu (A x b)
  (/ (vector/norm
      (vector/sub b
                  (matrix/vector-dot A x)))
     (vector/norm b)
     ))


(provide 'matrix)
