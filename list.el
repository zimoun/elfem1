

(defun get-first (list)
  "Return first element of LIST.

(see `car')"
  (car list))

(defun list-list-reversed (list &optional accumulate)
  (let ((acc accumulate)
        (head (list (car list)))
        (tail (cdr list)))
    (if (eq tail nil)
        acc
      (list-list-reversed tail (push-> head acc)))
    ))

(defun list-list (list)
  (reverse-recursive-lisp (list-list-reversed list)))

(defun combine-strict-reversed (list1 list2 &optional accumulate)
  (let ((acc accumulate)
        (head1 (car list1))
        (tail1 (cdr list1))
        (head2 (car list2))
        (tail2 (cdr list2)))

    (if (listp head1)
        (progn
          (setq acc (push-> head2 (push-> head1 acc)))
          (message "list? acc=%S" acc)
          )
      (setq acc (push-> (list head2 head1) acc)))
    (message "%S %S" (listp head1) head1)

    (if (or (eq tail1 nil)
            (eq tail2 nil)
            )
        acc
      (combine-strict-reversed tail1 tail2 acc))
    ))

(defun combine-strict (list1 list2)
  (reverse-recursive-lisp (combine-strict-reversed list1 list2)))

(defun combine-reversed (list1 list2 &rest lists)
  (let ((combined (combine-strict-reversed list1 list2))
        (list (car lists))
        (rest (cdr lists)))
    (if (eq list nil)
        combined
      (combine-reversed combined list rest))
    ))

(defun get-last (list)
  "Return the last element of LIST.

Pure Lisp implementation, using only `car' and `cdr'.
Expects a properly nil-terminated list.
The function recursively walks through LIST until the nil `cons' cell.

(see `last' for efficient built-in implementation)
(e.g., (get-last list) _equivalent_ (car (last list)))
"
  (if (eq nil (cdr list))
      (car list)
    (get-last (cdr list))
    ))

(defun get-ith (list i &optional tmp)
  "Return the I-th element of LIST.

Pure Lisp implementation, using only `cdr' and `car'.
Expects a properly nil-terminated list.
The function recursively walks through LIST until the nil `cons' cell.

TMP is set by default to 1.

WARNING:
The number of loop-recursion is limited. Try e.g.,
  (get-length (number-sequence 0 (/ max-lisp-eval-depth 3)))
See `max-lisp-eval-depth'."
  (let ((j tmp))
    (when (eq j nil)
      (setq j 1))
    (if (eq i j)
        (car list)
      (get-ith (cdr list) i (+ j 1)))
    ))

(defun get-length (list &optional accumulate)
  "Return the length of LIST.

Pure Lisp implementation, using only `cdr'.
Expects a properly nil-terminated list.
The function recursively walks through LIST until the nil `cons' cell.

WARNING:
The number of loop-recursion is limited. Try e.g.,
  (get-length (number-sequence 0 (/ max-lisp-eval-depth 3)))
See `max-lisp-eval-depth'.

(see `length' for efficient built-in implementation without recursion issue)"
  (let ((acc accumulate)
        (tail (cdr list)))
    (when (eq nil accumulate)
      (setq acc 0))
    (if (eq nil tail)
        (+ 1 acc)
      (get-length tail (+ 1 acc)))
  ))

(defun reverse-recursive-lisp (list &optional accumulate)
  "Reverse LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCUMULATE is by default set to `nil'.

The function recursively walks through LIST pushing each element to ACCUMULATE.
Since the number of loop-recursion is limited, the acceptable length is roughly limited.
(see `max-lisp-eval-depth' and `(elisp)')

(see `reverse' for efficient built-in implementation)
(see `nreverse' for built-in implementation modifying LIST)"
  (let ((head (car list))
        (tail (cdr list))
        (acc accumulate))
    ;; (when (eq nil accumulate)
    ;;     (setq acc ()))

    ;; should be refactored ?
    ;; since `(push-> head acc)' is done in any case
    ;; however the return needs to be more careful
    (if (eq nil tail)
        (push-> head acc)
      (reverse-recursive-lisp tail (push-> head acc))
      )))

(defalias 'rreverse 'reverse-recursive-lisp)

(defun push-> (elt list)
  "Push ELT to LIST and returns the new list.

(see `cons')
(see `push' for more a general implementation by `defmacro')"
  (cons elt list))

(defun push<- (list elt &optional accumulate)
  "Append ELT to LIST, i.e., the 'opposite' function of `push->'.

Pure Lisp implementation, using only `car' and `cdr' and `push->'.
The cost is O(2n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCUMULATE is by default set to `nil'.

The function recursively walks through the LIST pushing each element to ACCUMULATE.
Therefore, the resulting list is reversed.
Then, the last step pushes ELT to ACCU and applies `reverse-recursive-lisp'.

(see `append' for efficient built-in implementation)
(however ELT needs to be a properly nil-terminated list)
(e.g., (push<- LIST ELT) _equivalent_ (append LIST (list ELT)))
"
  (let ((head (car list))
        (tail (cdr list))
        (acc accumulate))

    (setq acc (push-> head acc))
    (if (eq nil tail)
        (reverse-recursive-lisp (push-> elt acc))
      (push<- tail elt acc)
    )))

(defalias 'append-element 'push<-)

(defun push-all (rlist list)
  "Push all elements from RLIST to LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push->'.
The cost is O(number of elements of RLIST). Do not know about memory cost.

Expects a properly nil-terminated lists.

The function recursively walks through RLIST pushing each element to LIST.
Therefore, the working RLIST length is limited (see `max-lisp-eval-depth').

(built-in implementation ?)"
    (let ((last (car rlist))
          (rest (cdr rlist)))

    (if (eq nil rest)
        (push-> last list)
      (push-all rest (push-> last list)))
    ))

(defun join-strict (list1 list2)
  "Join the nil-terminated lists LIST1 and LIST2
and so return the joined nil-terminated list.

The function first applies `reverse-recursive-lisp' then `push-all'.
Therefore, the cost is O(2 times number of elements of LIST1).
Do not know about memory cost."
  (let ((rlist (reverse-recursive-lisp list1))
        (list list2))
    (push-all rlist list)
    ))

;; (defun join-pop (list1 list2 &rest lists)
;;   "Join several nil-terminated lists.

;; LISTS allows to join more than only 2 lists, e.g.,
;;   (join l1 l2 l3 l4)

;; The function applies `join-strict' to LIST1 and LIST2,
;; and then recursively applies itself to this new joined list with `pop'-ing the rest LISTS.
;; (do not modify any list in stored in LISTS)"
;;     (let ((joined (join-strict list1 list2))
;;           (list (pop lists)))
;;       (if (eq nil list)
;;           joined
;;         (apply 'join-pop joined list lists))
;;     ))

(defun join (list1 list2 &rest lists)
  "Join several nil-terminated lists.

LISTS allows to join more than only 2 lists, e.g.,
  (join l1 l2 l3 l4)

The function applies `join-strict' to LIST1 and LIST2,
and then recursively applies itself to this new joined list with popping the rest LISTS."
    (let ((joined (join-strict list1 list2))
          (list (car lists))
          (rest (cdr lists)))
      (if (eq nil list)
          joined
        (join joined list rest))
      ;; should be better to use:
      ;; (apply 'join joined list rest))
      ;; because rest is not considered as list (or list of list)
      ;; and not as sequence of argument, i.e., l1 l2 l3 l4.
      ;; WARNING: `push-all' used in `join-strict' seems fixing.
    ))


(defun map (func list &optional one-elt accumulate)
  "Map FUNC to LIST.

The argument LIST represents a nil-terminated list.
The argument FUNC needs to be a symbol of function that takes
arguments and returns one element.

The function recursively walks through the LIST,
and applies FUNC to each element.
The results are collected in ACCUMULATE (by defaut set to `nil'),
therefore, the resulting list is reversed.
(see `map-reverse' or `reverse-map')

If ONE-ELT is set to `t',
then it allows to consider one element of LIST
as only one object passed to FUNC,
else, as expanded elements passed to FUNC.
The `complex' data structure is a 2-list,
therefore ONE-ELT is set to `t' for this use case.
(by defaut set to `nil')

Example:

(setq L (list 0 1 2 3 4))
(defun +one (x) (+ x 1))
(map '+one L)
--> (5 4 3 2 1)

(setq ll (list (list 1 2) (list 2 3) (list 3 4) (list 4 5)))
(map '(lambda (x) (push-> 42 x)) ll t)
--> ((42 4 5) (42 3 4) (42 2 3) (42 1 2))

(map '(lambda (x y) (+ x y)) ll)
--> (9 7 5 3)"
  (let ((acc accumulate)
        (head (car list))
        (tail (cdr list))
        ret val)

     (if (listp head)
       (if (eq one-elt nil)
          (setq val (apply func head))
         (setq val (funcall func head)))
       (setq val (funcall func head)))

    (setq ret (push-> val acc))
    (setq acc ret)

    (if (eq tail nil)
        acc
      (map func tail one-elt acc))
    ))

(defun map-reverse (func list &optional one-elt)
  "Map FUNC to LIST.

First, `reverse-recursive-lisp' is applied.
Second, `map' is applied.

The application of these two functions is commutative.
(see `reverse-map')


Example:

(setq L (list 0 1 2 3 4))
(defun +one (x) (+ x 1))
(map-reverse '+one L)
--> (1 2 3 4 5)"
  (map func (reverse-recursive-lisp list) one-elt))

(defun reverse-map (func list &optional one-elt)
  "Map FUNC to LIST.

First, `map' is applied.
Second, `reverse-recursive-lisp' is applied.


The application of these two functions is commutative.
(see `map-reverse')


Example:

(setq L (list 0 1 2 3 4))
(defun +one (x) (+ x 1))
(reverse-map '+one L)
--> (1 2 3 4 5)"
  (reverse-recursive-lisp (map func list one-elt)))



(defun reduce (func list &optional accumulate)
  "Reduce LIST by FUNC.

The argument LIST represents a nil-terminated list.
The argument FUNC needs to be a symbol of function that takes two
arguments and returns one element.

The function recursively walks through the LIST,
and applies to each element of LIST:
 (FUNC element ACCCUMALTE)
where the result is then collected in ACCUMULATE.
(by default ACCUMULATE is set to 0.0)


Example:

(setq L (list 1 2 3 4 5))
(defun will-compute-factorial (x y) (* x y))
(reduce 'will-compute-factorial L)
--> 120"
  (let ((acc accumulate)
        (head (car list))
        (tail (cdr list)))

    (if (eq accumulate nil)
      (setq acc head)
     (setq acc (funcall func head acc)))

    (if (eq tail nil)
        acc
      (reduce func tail acc))
    ))

(defun list-list-reversed (list &optional accumulate)
  "Return the listify LIST.

LIST is a nil-terminated list.
ACCUMULATE is the optionnal initial value of the loop recursion, and
then, where the result is collected. By default set to `nil.
Therefore, the resulting list is reversed.

(see `list-list' for non-reversed).


Example:

(list-list-reversed (list 1 2 3 4))
--> ((3) (2) (1) (4))"

  (let ((acc accumulate)
        (head (list (car list)))
        (tail (cdr list)))

    (setq acc (push-> head acc))
    (if (eq tail nil)
        acc
      (list-list-reversed tail acc))
    ))

(defun list-list (list)
    "Return the listify LIST.

LIST is a nil-terminated list.
(see `list-list-reversed')."
  (reverse-recursive-lisp (list-list-reversed list)))


(defun combine-strict-reversed (list1 list2 &optional accumulate ugly)
  "Combine LIST1 and LIST2, both nil-terminated lists.

The optionnal parameter ACCUMULATE recursively collects the result.
(by default set to `nil')

(see `combine-strict' for non reverse result)

And UGLY is internal parameter used by `combine-reversed-ugly'.
(by default set to `nil')


Example:

(setq l1 (list 1 2 3))
(setq l2 (list -1 -2 -3))
(combine-strict-reversed l1 l2)
--> ((-3 3) (-2 2) (-1 1))
"
  (let ((acc accumulate)
        (head1 (car list1))
        (tail1 (cdr list1))
        (head2 (car list2))
        (tail2 (cdr list2)))

    (if (eq ugly nil)
          (setq acc (push-> (list head2 head1) acc))
        (if (eq accumulate nil)
            (setq acc (list (push-> head2 head1)))
            (setq acc (push-> (push-> head2 head1) acc))))

    (if (or (eq tail1 nil)
            (eq tail2 nil))
        acc
      (combine-strict-reversed tail1 tail2 acc ugly))
    ))

(defun combine-strict (list1 list2)
  "Combine LIST1 and LIST2, bot nil-terminated lists.

First, apply `combine-strict-reversed'.
Then, `reverse-recursive-lisp'."
  (reverse-recursive-lisp (combine-strict-reversed list1 list2)))

(defun combine-reversed-ugly (ugly list1 list2 &rest lists)
  "Extension of `combine-strict-reversed' to arbitrary number of lists.

Do not use this function, prefer `combine-reversed'.

UGLY has to be set to `nil' at the first call (intial recursion term),
then it is internally turned to `t'. It is required by
`combine-strict-reversed' to properly handle the edge cases.

Example:

(setq l1 (list 1 2 3))
(setq l2 (list -1 -2 -3))
(combine-reversed-ugly nil l1 l2 l1 l2)"
  (let ((combined (combine-strict-reversed list1 list2 nil ugly))
        (list (car lists))
        (rest (cdr lists)))
    (if (eq list nil)
        combined
      (apply 'combine-reversed-ugly t (reverse-recursive-lisp combined) list rest))
    ))

(defun combine-reversed (list1 list2 &rest lists)
  "Wrapper around `combine-reversed-ugly'


Example:

(setq l1 (list 1 2 3))
(setq l2 (list -1 -2 -3))
(combine-reversed-ugly nil l1 l2 l1 l2)"
  (combine-reversed-ugly nil list1 list2 lists))

(defun combine (list1 list2 &rest lists)
  "Combine arbitrary number of nil-terminated lists.

First, it applies `combine-reversed' that returns the reversed
combined list, therefore, `reverse-recursive-lisp' is then `map'-ped,
which reversed each element and return also reversed.

Example:

(setq l1 (list 1 2 3))
(setq l2 (list -1 -2 -3))
(combine l1 l2 l1 l2)
--> ((1 -1 1 -1) (2 -2 2 -2) (3 -3 3 -3))"
  (map 'reverse-recursive-lisp (apply 'combine-reversed list1 list2 lists) t))



(defun map-binary (func list1 list2 &optional one-elt accumulate)
  "Same as `map' for binary functions FUNC.


Example:

(setq l1 (list 0 1 2 3 4))
(setq l2 (list 0 -1 -2 -3 -4))
(map-binary '(lambda (x y) (+ x y)) l1 l2)
(map-binary '+ l1 l2)
--> (0 0 0 0 0)

(map-binary '- l1 l2)
--> (8 6 4 2 0)"
  (let ((acc accumulate)
        (head1 (car list1))
        (tail1 (cdr list1))
        (head2 (car list2))
        (tail2 (cdr list2))
        ret val)

    (if (and (listp head1) (listp head2))
        (if (eq one-elt nil)
          (setq val (apply func head1 head2))
         (setq val (funcall func head1 head2)))
       (setq val (funcall func head1 head2)))

    (setq ret (push-> val acc))
    (setq acc ret)

    (if (and (eq tail1 nil) (eq tail2 nil))
        acc
      (map-binary func tail1 tail2 one-elt acc))
    ))


(defun map-binary-reverse (func list1 list2 &optional one-elt accumulate)
  "

Example:

(setq l1 (list 0 1 2 3 4))
(setq l2 (list 0 -1 -2 -3 -4))
(map-binary-reverse '(lambda (x y) (+ x y)) l1 l2)
(map-binary-reverse '+ l1 l2)
--> (0 0 0 0 0)

(map-binary-reverse '- l1 l2)
--> (0 2 4 6 8)"
  (map-binary func
              (reverse-recursive-lisp list1)
              (reverse-recursive-lisp list2)
              one-elt accumulate))

(defun reverse-map-binary (func list1 list2 &optional one-elt accumulate)
  "

Example:

(setq l1 (list 0 1 2 3 4))
(setq l2 (list 0 -1 -2 -3 -4))
(reverse-map-binary '(lambda (x y) (+ x y)) l1 l2)
(reverse-map-binary '+ l1 l2)
--> (0 0 0 0 0)

(reverse-map-binary '- l1 l2)
--> (0 2 4 6 8)"
  (reverse-recursive-lisp (map-binary func list1 list2 one-elt accumulate)))

;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;
;;;;;;;;;;;

(defun increase (x y) (< x y))
(defun decrease (x y) (> x y))
(defun tmp/compare (x y) "ugly Hack around scope" (> x y))

(defun insert-element (list elt &optional cmp)
  "Insert ELT to LIST
such that the right neighbor of ELT satisfies the `predicate' CMP.

Expects a properly a nil-terminated list.

By default, CMP is set to `increase' (by alias).
Other choice is `decrease'.
Otherwise, any function taking 2 elements of LIST
and returning one canonical `boolean' value is valid.

e.g.,
(insert-element LIST ELT 'decrease)

or

(defun my-predicate (x y) (< x y))
(insert-element LIST ELT 'my-predicate)

WARNING: one internal function named `tmp/compare' is defined
and the remains the GLOBAL scope."
  (let ((head (car list))
        (tail (cdr list)))
    ;; be careful !!
    ;; the function `tmp/compare' is GLOBAL
    ;; and remains in the scoping (see `dynamical binding')
    ;; WARNING: elint does not like.
    (makunbound 'tmp/compare)
    (if (eq nil cmp)
        (setq tmp/compare (lambda (x y) (increase x y)))
      (setq tmp/compare (lambda (x y) (funcall cmp x y))))

    (if (eq nil tail)
        (if (funcall tmp/compare elt head)
            (list elt head)
          (list head elt))
      (if (funcall tmp/compare elt head)
          (push elt list)
        (join (list head) (insert-element tail elt 'tmp/compare))
        ))
    ))

(defun sort-by-insertion (list &optional cmp)
  "Sort LIST using insertion algorithm.

Expects a properly a nil-terminated list.

(see `insert-element')"
  (let ((head (car list))
        (tail (cdr list)))
    ;; be careful !!
    ;; the function `tmp/compare' is GLOBAL
    ;; and remains in the scoping (see `dynamical binding')
(message "head=%f" head)

;(message "%S" (funcall cmp 1 2))

(message "%S is %S | %S is %S" 'cmp cmp '(fboundp 'tmp/compare) (fboundp 'tmp/compare))
(message "symbol: %S" (symbol-function 'tmp/compare))
(message "1 < 2 : %S" (tmp/compare 1 2))

(fmakunbound 'tmp/compare)
(message "%S is %S" '(fboundp 'tmp/compare) (fboundp 'tmp/compare))

;(message "%S" (funcall cmp 1 2))

(if (eq nil cmp)
    (progn
      (fset 'tmp/compare 'increase) ;(lambda (x y) (increase x y)))
      (message "cmp is %S " cmp)
      (message "increase? %S" (symbol-function 'tmp/compare)))
      (fset 'tmp/compare '(lambda (x y) (funcall cmp x y))))

(message "%S" (symbol-function 'tmp/compare))
(message "1 < 2 : %S" (tmp/compare 1 2))

    (if (eq nil tail)
        (list head)
      (insert-element (sort-by-insertion tail 'tmp/compare) head 'tmp/compare))
    ))

(provide 'list)
