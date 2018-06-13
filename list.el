
(defun get-first (list)
  "Return first element of LIST.

(see `car')"
  (car list))

(defun get-last (list)
  "Return the last element of LIST.

Pure Lisp implementation, using only `car' and `cdr'.
Expects a properly nil-terminated list.
The function recursively walks through the LIST until the last `cons' cell.

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

The function recursively walks through the LIST pushing each element to ACCU.

(see `reverse' for efficient built-in implementation)
(see `nreverse' for built-in implementation modifying LIST)"
  (let ((head (car list))
        (tail (cdr list))
        acc)
    (if (eq nil accu)
        (setq acc ())
      (setq acc accu))

    ;; should be refactored ?
    ;; since `(push head acc)' is done in any case
    ;; however the return needs to be more careful
    (if (eq nil tail)
        (push head acc)
      (reverse-manually tail (push head acc))
      )))


(defun append-elt (list elt &optional accu)
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
(e.g., (append-elt LIST ELT) _equivalent_ (append LIST (list ELT)))
"
  (let ((head (car list))
        (tail (cdr list))
        acc)
    (if (eq nil accu)
        (setq acc ())
      (setq acc accu))

    (push head acc)
    (if (eq nil tail)
        (reverse-manually (push elt acc))
      (append-elt tail elt acc)
    )))


(provide 'list)
