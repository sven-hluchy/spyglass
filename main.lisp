;;; in theory, this works, I would like to do some changes in the future, such
;;; as:
;;; - remove the type of the node, as this will be type :START over and over
;;; again anyhow, so I might as well remove it. I will then close the tags in
;;; the parse-html function such that it works via the / in the name of the
;;; closing tags. This should also free up some memory, because vlime just
;;; straight up dies when I try to run the program on the wikipedia homepage.
;;; - I don't really know how to incorporate the texts between the nodes into
;;; this whole ordeal but I am sure that it's actually quite an easy thing to
;;; implement
;;; - Completely forgot about the attributes of the nodes.

;;; TODO remove the type attribute
(defstruct node name attrs children type)

;;; these elements are self-closing, i.e. they cannot have children.
(defparameter *self-closing-elements*
  '(:AREA :BASE :BR :COL :EMBED :HR :IMG :INPUT :LINK :META
    :PARAM :SOURCE :TRACK :WBR))

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
          
;;; finds all the occurrences of a certain character char within a (sub)string
(defun find-all (string char &optional (start 0) result)
  (let ((position (position char string :start start)))
    (if position
        (find-all string char (1+ position) (append result (list position)))
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

(defun parse-nodes (html)
  (labels ((get-all-nodes (html)
             (loop for pos in (find-all html #\<)
                   when (char/= (aref html (1+ pos)) #\!)
                   collect (sanitize-string
                                   (subseq html pos (1+ (position #\> html :start pos))))))
           (get-node-type (node)
             (if (or (eq (aref node 1) #\/) (eq (aref node (- (length node) 2)) #\/))
                 :END
                 :START))
           (parse-attrs (node)
             (let ((pos (position #\Space node)))
               (if pos
                 (loop for item in (split-at-unquoted-space (subseq node (1+ pos)))
                       ;; if there is a = in the attribute
                       when (position #\= item)
                       collect (let ((parts (split-string item #\=)))
                                 (cons (remove #\" (car parts))
                                       (remove #\" (cadr parts))))
                       else collect item)
                   nil)))
           (get-node-attrs (node)
             (let ((pos (position #\Space node)))
               (if pos
                   (parse-attrs (subseq node pos))
                   nil)))
           (get-node-name (node)
             (parse-keyword
               (subseq node
                       (if (eq (aref node 1) #\/)
                           2
                           1)
                       (or (position #\Space node)
                           (position #\> node))))))
    (loop for node in (get-all-nodes html)
          collect (make-node :name (get-node-name node)
                             :attrs (get-node-attrs node)
                             :children nil
                             :type (get-node-type node)))))

(defun parse-html (nodes)
  (let ((lst (list (make-node :name "~toplevel~"))))
    (loop for node in nodes
          when (eq (node-type node) :START)
          do (progn
               (setf (node-children (car lst))
                     (append (node-children (car lst)) (list node))))
          (when (not (member (node-name node) *self-closing-elements*))
            (setf lst (cons node lst)))
          when (and (eq (node-type node) :END)
                    (member (node-name node) lst :key #'node-name :test #'string=))
          do (setf lst (remove (node-name node) lst :key #'node-name)))
    lst))

;;; TODO could be added into the run function as a label, as I don't see this
;;; being used anywhere else.
(defun read-html-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((string (make-string (file-length stream))))
      (read-sequence string stream)
      string)))

(defun run (input-file output-file)
  (let ((+html+ (read-html-file input-file)))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*standard-output* stream))
          (print (parse-html (parse-nodes +html+))))))))
