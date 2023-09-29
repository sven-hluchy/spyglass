(load "spyglass.lisp")
(in-package :spyglass)

;; All titles of the top 30 hackernews stories
(let ((+root+ (parse "news.html")))
  (loop for node in (find-all-nodes +root+ #'(lambda (x) (node-has x :class "titleline")))
        collect (node-text (nth 0 (node-children node)))))

;; Get the HTML element of the topmost hackernews story
(let ((+root+ (parse "news.html")))
  (time (find-node +root+ #'(lambda (x) (node-has x :class "titleline")))))
