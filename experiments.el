
(load-file "./list.el")
(load-file "./math.el")
(load-file "./complex.el")
(load-file "./vector.el")
(load-file "./matrix.el")


(setq N 10)

(setq A (matrix/make-eye N))

(setq A (matrix/make-laplacian N))

(setq b1 (vector/make-ones N))
(setq x1 (matrix/vector-solve A b1))
(setq r1 (vector/sub b1 (matrix/vector-dot A x1)))
(setq e1 (matrix/solver-residu A x1 b1))


(setq b2 (vector/make-linspace 1 2 N))
(setq x2 (matrix/vector-solve A b2))
(setq r2 (vector/sub b2 (matrix/vector-dot A x2)))
(setq e2 (matrix/solver-residu A x2 b2))

(setq b3 (vector/map 'complex/cos
                     (vector/make-linspace math/-pi math/pi N)))
(setq x3 (matrix/vector-solve A b3))
(setq r3 (vector/sub b3 (matrix/vector-dot A x3)))
(setq e3 (matrix/solver-residu A x3 b3))
