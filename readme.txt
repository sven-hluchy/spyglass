The main purpose of this project is to take an HTML document and make it
lisp-readable. This allows you to perform all sorts of algorithms on it. If you
want to find all the <span>-elements in the document, you can do just that. If
you wanted to get the average of all element occurences, you could probably do
that as well.

The only remaining major weakness (I have tested for) is that JSON data in an
attribute string can cause some issues.

Example usage:

`curl https://en.wikipedia.org/wiki/Main_Page -o main.html`, which can then be
opened in Spyglass via `(parse "main.html")`. For an example see `example.lisp`.

Instead of parsing the same file over and over again, you can also save the
result of a call to parse in an external file using `parse-into-file`.

File structure:

+----------------+-------------------------------------------------------------+
| spyglass.lisp  | contains the main code for parsing and analysing the nodes  |
|                | of a supplied html document. The exported functions can be  |
|                | used in other lisp files.                                   |
+----------------+-------------------------------------------------------------+
| example.lisp   | an example use case of the spyglass library.                |
+----------------+-------------------------------------------------------------+

To use this project, the only thing you need is a working Lisp compiler. It
should be enough to simply load the project file `spyglass.lisp`. After that,
you should be able to use all of the exported functions.

Exported Symbols:

make-node:
`make-node &key name attrs text children`
creates a new instance of a struct node

node-p:
`node-p instance`
returns true when instance is of type node

node-name:
`node-name instance`
returns the name of a node

node-attrs:
`node-attrs instance`
returns the list of attributes of a node, attributes are saved in the following
format: ((:KEY . "value") ...)

node-children:
`node-children instance`
returns the list of children of a node

find-all-nodes:
`collect-nodes node predicate`
returns all child nodes of root and root itself for which the predicate function
returns true

find-node:
`find-node node predicate`
returns the first node which satisfies the predicate

node-has:
`node-has node attribute value`
returns whether or not the attribute of a node has a certain value

