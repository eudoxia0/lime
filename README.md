# Lime

A [Swank][swank] client for Common Lisp applications, built on
[swank-protocol][swank-protocol].

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

[swank]: url
[swank-protocol]: url
[lime-text]: http://limetext.org/

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
