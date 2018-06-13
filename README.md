# elfem1
How to implement co/sine using One-Dimensional Finite Element Solver in Emacs Lisp ?

---

```shell
    emacs -Q -batch -L . -l makefile.el -e do/all
```

 - `-Q` avoids conflict with user configuration
 - `-batch` means not interactive
 - `-L .` prepends `.` to load-path; eases the `(require stuff)`
 - `-l makefile.el` loads "nice" UI functions
 - `-e do/all` launches the `do/all` functions (`do/compile`, `do/test`)
   note that `do/clean` removes all the `.elc` files
