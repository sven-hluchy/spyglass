(load "spyglass.lisp")
(in-package :spyglass)

;; This collects all HTML nodes of type span
(let ((+root+ (parse "main.html")))
  (collect-nodes (car +root+) #'(lambda (x) (eq (node-name x) :SPAN))))

;; Get all texts of all hyperlink (<a>) objects
(let ((+root+ (parse "main.html")))
  (loop for node in (collect-nodes (car +root+) #'(lambda (x) (eq (node-name x) :A)))
        collect (node-text node)))

