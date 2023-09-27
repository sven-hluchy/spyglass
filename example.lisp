(load "spyglass.lisp")
(in-package :spyglass)

;; This collects all HTML nodes of type span
(let ((+root+ (parse "test.html")))
  (collect-nodes (car +root+) #'(lambda (x) (eq (node-name x) :SPAN))))

;; Get all texts of all hyperlink (<a>) objects
(let ((+root+ (parse "test.html")))
  (loop for node in (collect-nodes (car +root+)
                                   #'(lambda (x) (eq (node-name x) :A)))
        collect (assoc :HREF (node-attrs node))))


