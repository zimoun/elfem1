
(defun get-first (list)
  "Return first element of LIST.

(see `car')"
  (car list))

(defun get-last (list)
  "Return the last element of LIST.

Pure Lisp implementation, using only `car' and `cdr'.
Expects a properly nil-terminated list.
The function recursively walks through LIST until the last `cons' cell.

(see `last' for efficient built-in implementation)
(e.g., (get-last list) _equivalent_ (car (last list)))
"
  (if (eq nil (cdr list))
      (car list)
    (get-last (cdr list))
    ))

(defun reverse-manually (list &optional accu)
  "Reverse LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCU is by default set to `nil'.

The function recursively walks through LIST pushing each element to ACCU.

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
      (reverse-manually tail (push head acc))
      )))


(defun append-element (list elt &optional accu)
  "Append ELT to LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(2n). Do not know about memory cost.

Expects a properly nil-terminated list.
ACCU is by default set to `nil'.

The function recursively walks through the LIST pushing each element to ACCU.
Therefore, the resulting list is reversed.
Then, the last step pushes ELT to ACCU and applies `reverse-manually'.

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
        (reverse-manually (push elt acc))
      (append-element tail elt acc)
    )))


(defun push-all (rlist list)
  "Push all elements from RLIST to LIST.

Pure Lisp implementation, using only `car' and `cdr' and `push'.
The cost is O(number of elements of RLIST). Do not know about memory cost.

Expects a properly nil-terminated lists.

The function recursively walks through RLIST pushing each element to LIST.

(built-in implementation ?) "
    (let ((last (car rlist))
          (rest (cdr rlist)))

    (if (eq nil rest)
        (push last list)
      (push-all rest (push last list)))
    ))

(defun join (list1 list2)
  "Join the nil-terminated lists LIST1 and LIST2
and so return the joined nil-terminated list.

The function first applies `reverse-manually' then `join-recursively'.
Therefore, the cost is O(2 times number of elements of LIST1).
Do not know about memory cost."
  (let ((rlist (reverse-manually list1))
        (list list2))
    (push-all rlist list)
    ))

(defun increase (x y) (< x y))
(defun decrease (x y) (> x y))

(defun insert-element (list elt &optional tmp/compare)
  "Insert ELT to LIST
such that the right neighbor of ELT satisfies the `predicate' TMP/COMPARE.

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
        (tail (cdr list))
        tmp/compare)
    ;; be careful !!
    ;; the function tmp/compare is GLOBAL
    ;; and remains in the scoping (see `dynamical binding')
    (when (eq nil tmp/compare)
        (defun tmp/compare (x y) (increase x y))
        ;(setq tmp/compare (lambda (x y) (increase x y)))
      (setq tmp/compare (lambda (x y) (funcall cmp x y))))

    (if (eq nil tail)
        (if (tmp/compare elt head)
            (list elt head)
          (list head elt))
      (if (tmp/compare elt head)
          (push elt list)
        (join (list head) (insert-element tail elt 'tmp/compare))
        ))
    ))

(defun sort-by-insertion (list &optional cmp)
  "Sort LIST using insertion algorithm.

Expects a properly a nil-terminated list.

WARNING: one internal function named `compare' is defined
and the remains the GLOBAL scope.

(see `insert-element')"
  (let ((head (car list))
        (tail (cdr list))
        compare)
    ;; be careful !!
    ;; the function compare is GLOBAL
    ;; and remains in the scoping (see `dynamical binding')
    (if (eq nil cmp)
        (setq compare (lambda (x y) (increase x y)))
      (setq compare (lambda (x y) (funcall cmp x y))))

    (if (eq nil tail)
        (list head)
      (insert-element (sort-by-insertion tail) head 'compare))
    ))


(provide 'list)
