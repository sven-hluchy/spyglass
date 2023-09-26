(load "sp.fasl")

(defvar +html+
  (read (open "output.lisp")))

(let ((root (car +html+)))
  (loop for child in (node-children root)
        when (eq (node-name child) :HTML)
        do (print child)))

