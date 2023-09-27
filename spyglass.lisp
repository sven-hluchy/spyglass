;; inline javascript is giving me headaches; I just ended up deleting all the
;; <script> nodes. Still, there are some other issues in bigger documents,
;; probably the likes of css or svg, or something else even.

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

(defun split-string (string char)
  (loop for i = 0 then (1+ j)
        as j = (position char string :start i)
        collect (subseq string i j)
        while j))

(defun split-at-unquoted-space (string)
  (let ((inside-quotes nil)
        (len (1- (length string)))
        (pos 0))
    (loop for i to len
          when (eq (aref string i) #\")
          do (setq inside-quotes (not inside-quotes))
          when (and (eq (aref string i) #\Space)
                    (not inside-quotes))
          collect (let ((p pos))
                    (setq pos (1+ i))
                    (subseq string p i))
          when (= i len)
          collect (subseq string pos len))))
          
;;; finds all the occurrences of a certain substring within a string
(defun find-all (string substr &optional (start 0) result)
  (let ((position (search substr string :start2 start)))
    (if position
        (find-all string substr (+ (length substr) position) (append result (list position)))
        result)))

;;; turns a string into a keyword, i.e. (parse-keyword "ABC") => :ABC
(defun parse-keyword (string)
  (intern (string-upcase string) :keyword))

;;; replaces all occurrences of a character with the provided character
;;; new-char.
(defun replace-char-in-string (original-string char-to-replace new-char)
  (let ((result-string (copy-seq original-string)))
    (loop for i from 0 below (length result-string)
          do (when (char= (char result-string i) char-to-replace)
               (setf (char result-string i) new-char)))
    result-string))

(defun sanitize-string (string)
  (replace-char-in-string (remove #\Newline string)
                          #\Tab #\Space))

(defun delete-substring (string start end)
  (let ((before (subseq string 0 start))
        (after (subseq string (1+ end))))
    (concatenate 'string before after)))

(defun parse-nodes (html)
  (labels ((remove-js (html)
             (let* ((start (search "<script" html))
                    (end (position #\> html :start (or
                                                     (search "</script" html)
                                                     0))))
               (if (or (null start) (null end))
                   html
                   (remove-js (delete-substring html start end)))))
           (sanitize (html)
             (remove-js html))
           (get-all-nodes (html)
             ;; TODO I need to somehow make find-all ignore javascript
             (loop for pos in (find-all html "<")
                   as tagname = (sanitize-string (subseq html pos (1+ (position #\> html :start pos))))
                   when (char/= (aref html (1+ pos)) #\!)
                   collect (list tagname
                                 (let ((text (sanitize-string
                                               (subseq html
                                                       (1+ (position #\> html :start (1+ pos)))
                                                       (position #\< html :start (1+ pos))))))
                                   (if (string/= (string-trim " " text) "")
                                       text
                                       nil)))))
           (get-node-attrs (node)
             (let ((pos (position #\Space node)))
               (if pos
                 (loop for item in (split-at-unquoted-space (subseq node (1+ pos)))
                       ;; if there is a = in the attribute
                       when (position #\= item)
                       collect (let ((parts (split-string item #\=)))
                                 (cons (parse-keyword (remove #\" (car parts)))
                                       (remove #\" (cadr parts))))
                       else collect item))))
           (get-node-name (node)
             (parse-keyword
               (subseq node
                       1
                       (or (position #\Space node)
                           (position #\> node))))))
    (loop for node in (get-all-nodes (sanitize html))
          collect (make-node :name (get-node-name (car node))
                             :attrs (get-node-attrs (car node))
                             :text (cadr node)
                             :children nil))))

(defun parse-html (nodes)
  (let ((lst (list (make-node :name "~toplevel~"))))
    (loop for node in nodes
          as name = (node-name node)
          as type = (position #\/ (symbol-name name))
          as tag = (parse-keyword (subseq (symbol-name name) 1))
          when (not type)
          do (progn
               (setf (node-children (car lst))
                     (append (node-children (car lst)) (list node)))
               (when (not (member name *self-closing-elements*))
                 (setf lst (cons node lst))))
          when (and type
                    (member tag lst :key #'node-name))
          do (setf lst (remove tag lst :key #'node-name)))
    lst))

(defun collect-nodes (root predicate &optional (lst '()))
  (if root
      (let ((result lst))
        (when (funcall predicate root)
          (setq result (cons root result)))
        (dolist (child (node-children root))
          (setq result (collect-nodes child predicate result)))
        result)))

;;; TODO could be added into the run function as a label, as I don't see this
;;; being used anywhere else.
(defun read-html-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun parse (input-file)
  (let ((+html+ (read-html-file input-file)))
    (parse-html (parse-nodes +html+))))

(defun parse-into-file (input-file output-file)
  (let ((+html+ (read-html-file input-file)))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*standard-output* stream))
          (print (parse-html (parse-nodes +html+))))))))
