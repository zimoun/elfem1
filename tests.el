
(require 'complex)
(require 'list)


(ert-deftest complex ()
  "Test `complex'."
  (let* ((re 2)
         (im 42)
         (cplx1 (complex re im))
         (cplx2 (complex 1 2))
         (res-add (complex (+ re 1) (+ im 2)))
         (res-mul (complex (- re (* im 2)) (+ (* re 2) im))
         ))
    ;; (should (equal (complex-p cplx) t))
    ;; (should (equal (complex-p re) nil))
    ;; (should (equal (complex-p im) nil))
    (should (equal (real cplx1) (float re)))
    (should (equal (imag cplx1) (float im)))
    (should (equal (complex/add cplx1 cplx2) res-add))
    (should (equal (complex/mul cplx1 cplx2) res-mul))
    ))


(ert-deftest list ()
  "Test my `list' functions."
  (let ((l (list 1 2 3 4 5 6 7 8 9 10))
        (L (list -1 -2 -3)))
    (should (equal (get-first l) 1))
    (should (equal (get-last l) 10))
    (should (equal (reverse-manually l) (list 10 9 8 7 6 5 4 3 2 1)))
    (should (equal (append-element l -1) (list 1 2 3 4 5 6 7 8 9 10 -1)))
    (should (equal (join l L) (list 1 2 3 4 5 6 7 8 9 10 -1 -2 -3)))
    :expected-result :failed
    (equal (append-element l -3) (list 1 2 3 4 5 6 7 8 9 10 -2))
    ))


(provide 'tests)
