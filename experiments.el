
(load-file "./complex.el")
(load-file "./list.el")


(defun f (x)
  (let (gg)
    (defun gg (x)
      (+ x 2))
    (gg (+ x 2))))

(setq x (complex 1 2))
(setq y (complex 2 3))


(setq ll (number-sequence 0 4))

(setq l1 (number-sequence 7 12))
(setq l2 (number-sequence 2 6))

(setq L1 l1)
(setq L (join l1 l2))
(setq L (join L ll))

(defun f (x) (* 2 x))
(defun ff (x) (* x x))
(defun g (fn x) (funcall fn (+ 1 x)))

;; (setq l1 (number-sequence 162 0 -1))
;; (setq l2 (number-sequence 800 101 -1))
;; (setq LL (join l1 l2))

;; (setq l1 (number-sequence 41 0 -1))
;; (setq l2 (number-sequence 93 11 -1))
;; (setq LL (join l1 l2))
;; (setq sorted (sort-by-insertion LL))

;; (setq ll (number-sequence 111 0 -1))
;; (setq ls (sort-by-insertion ll))

;; (setq l1 (number-sequence 600 401 -1))
;; (setq LL (join LL l1))
;; (setq l1 (number-sequence 800 402 -1))
;; (setq LL (join LL l1))


(defun tmp (x) (+ 1 x))
(defun f (list &optional g accu)
  (let ((head (car list))
        (tail (cdr list))
        (acc accu))
    (when (eq nil accu)
      (setq acc '()))

    (makunbound 'tmp)
    (if (eq nil g)
        (setq tmp (lambda (x) x))
      (setq tmp (lambda (x) (funcall g x))))

    (if (eq nil tail)
        (push-> head acc)
      ;(push-> head (f tail)))
      (push-> (funcall tmp head) (f tail 'tmp acc)))
    ))

(defun f (list &optional g acc)
  list)

(defun push-> (elt list)
  (cons elt list))
