# elfem1
How to implement co/sine using One-Dimensional Finite Element Solver in Emacs Lisp ?

---

The aim is to learn more about Emacs Lisp, starting from a subject that I
master enough to focus on the language.

Therefore, the constraints of the exercice are:
 1. consider the arithmetic operations: +,-,*,/ on integer and float
 2. consider the elementary operations on list:
     - `(list 1 2 3)` creates the list 1,2,3
     - `(car lst)` return the first element of the list
     - `(cdr lst)` returns the rest of the list
   See [Emacs Lisp, sec. List](https://www.gnu.org/software/emacs/manual/html_node/elisp/Box-Diagrams.html#Box-Diagrams).
 3. consider recursive function


## The reference solution

```shell
    sin x = imaginary part of e^ix
```

and

```shell
    e^ix = sum_n=0 (ix)^n / n!
```

So, we need:
 - Complex number and their associated operations (add, mul)
 - Power
 - Factorial


## The overkilled solution

Let solve the differential equation:
```shell
    u'' + k^2 u = 0 in [0,1] with u(0)=1 and u'(1)=ik u(1)
```
by a finite element method.

So, we need:
 - Matrix
 - Inner product
 - Matrix/Vector product
 - Linear solver (e.g., Gauss pivot)


---

# Install / Run

```shell
    emacs -Q -batch -L . -l makefile.el -e do/all
```

 - `-Q` avoids conflict with user configuration
 - `-batch` means not interactive
 - `-L .` prepends `.` to load-path; eases the `(require stuff)`
 - `-l makefile.el` loads "nice" UI functions
 - `-e do/all` launches the `do/all` functions (`do/compile`, `do/test`)
   note that `do/clean` removes all the `.elc` files
