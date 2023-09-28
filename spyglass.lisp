(defpackage spyglass
  (:use :common-lisp)
  (:export #:make-node
           #:node
           #:node-p
           #:node-name
           #:node-attrs
           #:node-text
           #:node-children
           #:collect-nodes
           #:parse
           #:parse-into-file))

(in-package :spyglass)

(defstruct node name attrs text children)

;;; these elements are self-closing, i.e. they cannot have children.
(defparameter *self-closing-elements*
  '(:area :base :br :col :embed :hr :img :input :link
    :meta :param :source :track :wbr))

(defun read-html-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

;;; turns a string into a keyword, i.e. (parse-keyword "ABC") => :ABC
(defun parse-keyword (string)
  (intern (string-upcase string) :keyword))

(defun split-string (string char)
  (loop for i = 0 then (1+ j)
        as j = (position char string :start i)
        collect (subseq string i j)
        while j))

(defun split-at-unquoted-space (string)
  (let ((string (string-trim '(#\Newline #\Space) string))
        (result '())
        (current-token "")
        (inside-quotes nil))
    (loop for char across string
          do (cond
               ((char= char #\") (setq inside-quotes (not inside-quotes)))
               ((and (char= char #\Space) (not inside-quotes))
                (push current-token result)
                (setq current-token ""))
               (t (setq current-token (concatenate 'string current-token (string char))))))
    (push current-token result)
    (reverse result)))

;;; finds all the occurrences of a certain substring within a string
(defun find-all (string substr &optional (start 0) result)
  (let ((position (search substr string :start2 start)))
    (if position
        (find-all string substr (+ (length substr) position) (append result (list position)))
        result)))

;;; this function finds all start tags in an html document and returns them in a
;;; list in the following format: ((position, name, attributes) ...)
(defun find-all-tags (html)
  (loop for pos in (find-all html "<")
        when (every #'alphanumericp (subseq html (1+ pos) (or (position #\Space html :start pos)
                                                              (position #\> html :start pos))))
        ; when (alpha-char-p (aref html (+ 1 pos)))
        collect (let* ((end (position #\> html :start pos))
                       (spc (position #\Space html :start pos :end end)))
                  (list (1+ pos)
                        (string-trim '(#\Space #\Newline #\Tab)
                                     (subseq html (1+ pos) (or spc end)))
                        (when spc
                          (loop for item in (split-at-unquoted-space
                                              (subseq html (1+ spc) end))
                                when (position #\= item)
                                collect (let ((parts (split-string item #\=)))
                                          (cons (parse-keyword (remove #\" (car parts)))
                                                (remove #\" (cadr parts))))
                                else collect (cons item T)))))
        when (char= (aref html (1+ pos)) #\/)
        collect (list (1+ pos)
                      (string-trim '(#\Space #\Newline #\Tab)
                                   (subseq
                                     html (1+ pos) (position #\> html :start pos)))
                      nil)))

(defun close-tag (tag)
  (format nil "</~a>" tag))

(defun match-tags (html)
  (let ((lst (list (make-node :name :toplevel))))
    ;; we go to the position of the found start tag and we search for its
    ;; corresponding end-tag from that position onward.
    (loop for tag in (find-all-tags html)
          as type = (if (char= (aref (cadr tag) 0) #\/)
                        :END
                        :START)
          as closetag = (parse-keyword (subseq (cadr tag) 1))
          as node = (make-node :name (parse-keyword (cadr tag))
                                    :attrs (caddr tag)
                                    :text nil
                                    :children nil)
          when (eq type :START)
          do (progn
               (setf (node-children (car lst))
                     (append (node-children (car lst)) (list node)))
               (when (not (member (node-name node) *self-closing-elements*))
                 (setf lst (cons node lst)))
               (setf (node-text (car lst))
                     (string-trim
                       '(#\Space #\Tab #\Newline)
                       (subseq
                         html
                         (1+ (position #\> html :start (car tag)))
                         (position #\< html :start (1+ (car tag)))))))
          when (and (eq type :END) (member closetag lst :key #'node-name))
          do (setf lst (remove closetag lst :key #'node-name)))
    lst))

(defun collect-nodes (root predicate &optional (lst '()))
  (if root
      (let ((result lst))
        (when (funcall predicate root)
          (setq result (append result (list root))))
        (dolist (child (node-children root))
          (setq result (collect-nodes child predicate result)))
        result)))

(defun parse (input-file)
  (let ((+html+ (read-html-file input-file)))
    (match-tags +html+)))

(defun parse-into-file (input-file output-file)
  (let ((+html+ (read-html-file input-file)))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*standard-output* stream))
          (print (match-tags +html+)))))))
