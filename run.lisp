(defun run ()
  (when (load "main.fasl")
    (run "test.html" "output.lisp")))

