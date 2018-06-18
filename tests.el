
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
    (should (equal (conj cplx1) (complex re (- 0 im))))
    (should (equal (complex/abs2 cplx1) (+ 4 1764.0)))
    (should (equal (complex/abs2 cplx2) 5.0))
    (should (equal (complex/div cplx1 cplx2) (complex 17.2 7.6000000000000005)))
    (should (equal (complex/pow cplx1 5) (complex 30975872.0 127731072.0)))
    (should (equal (complex/pow cplx2 7) (complex 29 278)))
    ))


(ert-deftest list ()
  "Test my `list' functions."
  (let ((l1 (list 1 2 3 4 5 6 7 8 9 10))
        (l2 (list -1 -2 -3)))
    (should (equal (get-first l1) 1))
    (should (equal (get-last l1) 10))
    (should (equal (reverse-manually l1) (list 10 9 8 7 6 5 4 3 2 1)))
    (should (equal (append-element l1 -1) (list 1 2 3 4 5 6 7 8 9 10 -1)))
    (should (equal (join l1 l2) (list 1 2 3 4 5 6 7 8 9 10 -1 -2 -3)))
    :expected-result :failed
    (equal (append-element l1 -3) (list 1 2 3 4 5 6 7 8 9 10 -2))
    ))


(provide 'tests)
