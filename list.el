

(defun get-first (list)
  "Return first element of LIST.

(see `car')"
  (car list))

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

(defun get-length (list &optional accu)
  "Return the length of LIST.

Pure Lisp implementation, using only `cdr'.
Expects a properlu nil-terminated list.
The function recursively walks through LIST until the nil `cons' cell.

WARNING:
The number of loop-recursion is limited. Try e.g.,
  (get-length (number-sequence 0 (/ max-lisp-eval-depth 3)))
See `max-lisp-eval-depth'.

(see `length' for efficient built-in implementation with recursion issue)"
  (let ((acc accu)
        (tail (cdr list)))
    (when (eq nil accu)
      (setq acc 0))
    (if (eq nil tail)
        (+ 1 acc)
      (get-length tail (+ 1 acc)))
  ))

(defun reverse-recursive-lisp (list &optional accu)
  "Reverse LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCU is by default set to `nil'.

The function recursively walks through LIST pushing each element to ACCU.
Since the number of loop-recursion is limited, the acceptable length is roughly limited.
(see `max-lisp-eval-depth' and `(elisp)')

(see `reverse' for efficient built-in implementation)
(see `nreverse' for built-in implementation modifying LIST)"
  (let ((head (car list))
        (tail (cdr list))
        (acc accu))
    (when (eq nil acc)
        (setq acc ()))

    ;; should be refactored ?
    ;; since `(push head acc)' is done in any case
    ;; however the return needs to be more careful
    (if (eq nil tail)
        (push head acc)
      (reverse-recursive-lisp tail (push head acc))
      )))

(defalias 'rreverse 'reverse-recursive-lisp)

(defun push-> (elt list)
  "Push ELT to LIST and returns the new list.

(see `cons')"
  (cons elt list))

(defun push<- (list elt &optional accu)
  "Append ELT to LIST, i.e., the 'opposite' function of `push->'.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(2n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCU is by default set to `nil'.

The function recursively walks through the LIST pushing each element to ACCU.
Therefore, the resulting list is reversed.
Then, the last step pushes ELT to ACCU and applies `reverse-recursive-lisp'.

(see `append' for efficient built-in implementation)
(however ELT needs to be a properly nil-terminated list)
(e.g., (append-element LIST ELT) _equivalent_ (append LIST (list ELT)))
"
  (let ((head (car list))
        (tail (cdr list))
        (acc accu))
    (when (eq nil acc)
        (setq acc ()))

    (push head acc)
    (if (eq nil tail)
        (reverse-recursive-lisp (push elt acc))
      (push<- tail elt acc)
    )))

(defalias 'append-element 'push<-)

(defun push-all (rlist list)
  "Push all elements from RLIST to LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(number of elements of RLIST). Do not know about memory cost.

Expects a properly nil-terminated lists.

The function recursively walks through RLIST pushing each element to LIST.
Therefore, the working RLIST length is limited (see `max-lisp-eval-depth').

(built-in implementation ?)"
    (let ((last (car rlist))
          (rest (cdr rlist)))

    (if (eq nil rest)
        (push last list)
      (push-all rest (push last list)))
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

(defun join (list1 list2 &rest lists)
  "Join several nil-terminated lists.

LISTS allows to join more than only 2 lists, e.g.,
  (join l1 l2 l3 l4)

The function applies `join-strict' to LIST1 and LIST2,
and then recursively applies itself to this new joined list with `pop'-ing the rest LISTS.
(do not modify any list in stored in LISTS)"
    (let ((joined (join-strict list1 list2))
          (list (pop lists)))
      (if (eq nil list)
          joined
        (apply 'join joined list lists))
    ))

(defun map (function list &optional accu)
  "Map FUNCTION to LIST.

The argument FUNCTION needs to be a symbol.
The argument LIST represents a nil-terminated list.

First, this function recursively walks through the LIST,
and applies the FUNCTION to each element.
The results are collected in ACCU,
therefore, the last step needs to apply `reverse-recursive-lisp'.

Example:

(setq L (list 0 1 2 3 4))
(defun +one (x) (+ x 1))
(map '+one L)
--> (1 2 3 4 5 6)"
  (let ((acc accu)
        (head (car list))
        (tail (cdr list))
        ret val)

    (setq val (funcall function head))
    (setq ret (push-> val acc))
    (setq acc ret)

    (if (eq tail nil)
        (reverse-recursive-lisp acc)
      (map function tail acc))
    ))


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
