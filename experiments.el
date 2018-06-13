

(require 'complex)
(require 'list)


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
