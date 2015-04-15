libInput: program input representation in OCaml
===============================================

We represent an input using a data type called *Input Vector*. An input vector
is a collection of input values of different input classes such as arguments and
files. One input vector can contain multiple input values, but it should be used
for a single program execution. For example, the input vector of the following
command line
```bash
convert foo.jpg bar.gif
```
contains three arguments and one file, but it represents an input for a single
execution of the program convert.

Each input value can have an optional attribute, which specifies the property of
the corresponding input value. For example, an argument has a file-in attribute,
which indicates that the argument is used to specify an input file from the
command line.

An input value is identified by its own name. For example, the first argument of
an input vector has a name '0', whereas the second argument of the input vector
has a name '1'. Similarly, a file input of an input vector is identified by the
path of the file.

It is often useful to differentiate a symbolic input and a concrete input. We
distinguish the property of an input at a byte granualrity. Therefore, each
input byte can be either symbolic or concrete.

