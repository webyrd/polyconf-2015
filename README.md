# polyconf-2015
Resources from both the PolyConf 2015 talk and relational interpreters workshop

PolyConf 2015: 
http://polyconf.com/

------

Talk

`append`, `appendo`, proof checker/theorem prover code:
`scheme-interpreter/poly-scheme.scm`

Semantics for the WHILE language adapted from 'Semantics with Applications: A Formal Introduction' by Hanne Riis Nielson and Flemming Nielson.  Wiley Professional Computing, (240 pages, ISBN 0 471 92980 8), Wiley, 1992. Revised 1999 version available online at:
http://www.daimi.au.dk/~bra8130/Wiley_book/wiley.html

Symbolic execution example adapted from Stephen Chong's slides (with content from slides by Jeff Foster):
http://www.seas.harvard.edu/courses/cs252/2011sp/slides/Lec13-SymExec.pdf

Symbolic execution code in WHILE:
`while-interpreter/poly-while.scm`

Example symbolic execution call:
`while-interpreter/poly-trans.scm`

------

Workshop

Write a Relational Scheme Interpreter in miniKanren

This workshop will cover the fundamentals of programming in miniKanren, an embedded domain specific language for
constraint logic programming. We will begin with an overview of the miniKanren language, and will write a few
simple miniKanren relations that "run backward." We will then write a more sophisticated miniKanren program: an
environment-passing Scheme interpreter, written as a relation. We will extend the interpreter in various ways, and
explore how the interpreter can be used for program synthesis.

Minimal Scheme/Call-by-Value Lambda-calculus interpreter, written in Scheme:
`interp.scm`

Minimal Scheme/Call-by-Value Lambda-calculus interpreter, written in miniKanren:
`interpo.scm`

Transcripts from the workshop:
`trans.scm`,
`trans2.scm`

miniKanren.org: 
http://minikanren.org/

Faster miniKanren implementation (Racket and Vicare Scheme):
https://github.com/michaelballantyne/faster-miniKanren

Racket:
http://racket-lang.org/

Vicare Scheme:
http://marcomaggi.github.io/vicare.html

Veneer (client-side JavaScript compiler for miniKanren):
http://tca.github.io/veneer/editor.html
