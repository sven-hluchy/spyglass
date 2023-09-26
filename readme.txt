At the moment this thing works as follows:
You have the `main.lisp` file which contains all the code for parsing the html
document. Using `compile-file`, this file should be compiled and subsequently
loaded. Using `run`, the program can be run on (hopefully) any well-formatted
HTML document.

Synopsis of `run`:
`(run "input.html" "output.lisp")`.

Due to how `print` works, i.e. it always prints to the stdout as well, the
output will be both written to stdout and the provided output file. The idea is
that this newly created .lisp file can then be loaded to act as a lisp-readable
representation of the original HTML document.

This project is both in early stages of development and also written in a very
_interesting_ manner, if you will. This means that any actual features are not
implemented as of yet.
