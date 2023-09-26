(defun run ()
  (when (load "sp.fasl")
    (run "main.html" "output.lisp")))

