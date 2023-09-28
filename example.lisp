(load "spyglass.lisp")
(in-package :spyglass)

;; All titles of the top 30 hackernews stories
(let ((+root+ (parse "news.html")))
  (loop for node in (collect-nodes (car +root+) #'(lambda (x) (node-has x :class "titleline")))
        collect (node-text (nth 0 (node-children node)))))

;; Get all texts of all hyperlink (<a>) objects
(let ((+root+ (parse "test.html")))
  (loop for node in (collect-nodes (car +root+)
                                   #'(lambda (x) (eq (node-name x) :A)))
       collect (assoc :HREF (node-attrs node))))
