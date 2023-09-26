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

(defstruct node name attrs children type)

(defun find-all (string char &optional (start 0) result)
  (let ((position (position char string :start start)))
    (if position
        (find-all string char (1+ position) (append result (list position)))
        result)))

(defun parse-keyword (string)
  (intern (string-upcase string) :keyword))

(defparameter *self-closing-elements*
  '(:AREA :BASE :BR :COL :EMBED :HR :IMG :INPUT :LINK :META
    :PARAM :SOURCE :TRACK :WBR))

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
           (parse-attrs (string) nil)
           (parse-attrs-fix (string)
             (loop for pos in (find-all string #\=)
                   collect (cons
                             (parse-keyword (subseq string
                                              (1+ (position #\Space (subseq string 0 pos)
                                                            :from-end 1))
                                              pos))
                             (string-trim '(#\Space #\Newline #\")
                                          (subseq string
                                                  (1+ pos)
                                                  (1+ (position #\" string :start (+ 2 pos))))))))
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

;; (append '(1 2 3) '(5))

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

#|

<p class="ab dc" id="test"> => ... :ATTRS ((:CLASS . ("ad" "dc")) (:ID . ("id")))

(assoc :class (node-attrs node))


The naive approach I thought of goes as follows:
- you get the next tag, let's say a <div> and find the next </div>. Everything in between is the innerHTML of this div tag.
- for every tag in the innerHTML we do the same as we just did for the div.

if a tag has many different children, such as
<div>
    <p>Hello</p>
    <br />
    <p>There</p>
</div>

div.text = "Hello There"
p.text   = "Hello"
p.text   = "There"

The text of a node is the text enclosed in its own tags plus all the texts of their children.

NOTE: The parser would probably just die when it encounters a <br> instead of a <br />.

<div> ... </div>

#S(NODE :TAG :P
        :ATTRS ((:CLASS . ("ab" "ac")) (:ID . ("test")))
        :PARENT ...
        :CHILDREN NIL)

A tag can have just the tagname, i.e. no attributes, such as
    <p>Sample text</p>
A tag can have a variable number of attributes

or, a tag can have no attributes AND no closing tag, such as <br />

<TAGNAME></TAGNAME>
<TAGNAME ATTR1=\"...\" ... ATTRN=\"...\"></TAGNAME>
<TAGNAME />
|#
