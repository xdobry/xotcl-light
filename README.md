# xotcl-light
Object oriented module for TCL script programming language.
http://tcl.tk

XOTcl-Light is pure Tcl XOTcl compatible build on top of TclOO. 
http://www.xotcl.org

It allows to run XOTcl program directly with Tcl 8.6 without binary XOTcl library.

XOTcl-Light does not support all XOTcl functions.
Following functionality is not supported.

- no complex slot attributes. Only simple parameter similar to earlier XOTcl versions.
- no comlex forward syntax only OOTcl forward

XOTcl-Libht object are regular TclOO object. 
So they can be extended and manipulated directly by TclOO syntax.

XOTcl-Light seems to be twice slower than original XOTcl 
