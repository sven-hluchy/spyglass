(load "spyglass.lisp")
(in-package :spyglass)

;; This collects all HTML nodes of type span
(let ((+root+ (parse "test.html")))
  (collect-nodes (car +root+) #'(lambda (x) (eq (node-name x) :SPAN))))

