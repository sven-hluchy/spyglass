The main purpose of this project is to take an HTML document and make it
lisp-readable. This allows you to perform all sorts of algorithms on it. If you
want to find all the <span>-elements in the document, you can do just that. If
you wanted to get the average of all element occurences, you could probably do
that as well. The only major thing which does not work at all is simple text
scraping. This will probably be implemented in the near future though.

File structure:

+----------------+-------------------------------------------------------------+
| spyglass.lisp  | contains the main code for parsing and analysing the nodes  |
|                | of a supplied html document. The exported functions can be  |
|                | used in other lisp files.                                   |
+----------------+-------------------------------------------------------------+
| example.lisp   | an example use case of the spyglass library.                |
+----------------+-------------------------------------------------------------+
| test.html      | an example file for a very simple HTML document. Spyglass   |
|                | can also be run on more complex websites.                   |
+----------------+-------------------------------------------------------------+

To use this project, the only thing you need is a working Lisp compiler. It
should be enough to simply load the project file `spyglass.lisp`. After that,
you should be able to use all of the exported functions.

Example usage:

`curl https://en.wikipedia.org/wiki/Main_Page -o main.html`, which can then be
opened in Spyglass via `(parse "main.html")`. For an example see `example.lisp`.
