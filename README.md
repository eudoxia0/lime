![logo](logo.png)

# Lime

[![Build Status](https://travis-ci.org/eudoxia0/lime.svg?branch=master)](https://travis-ci.org/eudoxia0/lime)
[![Coverage Status](https://coveralls.io/repos/eudoxia0/lime/badge.svg?branch=master)](https://coveralls.io/r/eudoxia0/lime?branch=master)
[![Quicklisp](http://quickdocs.org/badge/lime.svg)](http://quickdocs.org/lime/)

A Swank ([SLIME][slime]) client for Common Lisp applications, built on
[swank-protocol][swank-protocol]. It can be used to connect to a remote or local
Swank server, evaluate code, and receive responses for printing to the screen,
handling debugger events, etc.

# Usage

## Example

To load and run the REPL example, do this:

```lisp
(ql:quickload :lime-example)
(lime-example:repl)
```

It works as you'd expect:

```lisp
CL-USER> (lime-example:repl)
Starting Swank server...
Connecting...
Swank server running on sbcl 1.2.9
COMMON-LISP-USER> (+ 2 2)
4
COMMON-LISP-USER>
```

# Name

Like SLIME, but without the leading S. No relation to
[the text editor][lime-text].

[slime]: https://github.com/slime/slime
[swank-protocol]: https://github.com/eudoxia0/swank-protocol
[lime-text]: http://limetext.org/

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
