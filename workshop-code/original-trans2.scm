Vicare Scheme version 0.1d2, 64-bit
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "test-all.scm")
Testing "assign-1"
Testing "8"
Testing "9"
Testing "10"
Testing "11"
Testing "!-0"
Testing "!-correct-0"
Testing "!-1"
Testing "!-correct-1"
Testing "!-2"
Testing "!-correct-2"
Testing "!-3"
Testing "!-correct-3"
Testing "!-4"
Testing "!-correct-4"
Testing "!-5"
Testing "!-correct-5"
Testing "!-3-backwards"
Testing "!-correct-3-backwards"
Testing "!-4-backwards"
Testing "!-correct-4-backwards"
Testing "!-5-backwards-a"
Testing "!-correct-5-backwards-a"
Testing "!-5-backwards-b"
Testing "!-correct-5-backwards-b"
Testing "2"
Testing "2-correct"
Testing "3"
Testing "3-correct"
Testing "4"
Testing "5"
Testing "6"
Testing "infer first :="
Testing "!-correct infer first :="
Testing "infer first :=, no absento"
Testing "!-correct infer first :=, no absento"
Testing "13b"
Testing "13c"
Testing "13c-no-absento"
Testing "13c-no-absento-multiple-calls"
Testing "!-correct 13c-no-absento-multiple-calls-a"
Testing "!-correct 13c-no-absento-multiple-calls-b"
Testing "!-correct 13c-no-absento-multiple-calls-c"
Testing "!-correct 13c-no-absento-multiple-calls-d"
Testing "!-correct 13c-no-absento-multiple-calls-g"
Testing "7-no-absento-multiple-calls-1"
Testing "!-correct-7-no-absento-multiple-calls-1-a"
Testing "!-correct-7-no-absento-multiple-calls-1-b"
Testing "!-correct-7-no-absento-multiple-calls-1-b-second-answer"
Testing "7-no-absento-multiple-calls-1b"
Testing "7-no-absento-multiple-calls-1b-more-structured"
Testing "7-no-absento-multiple-calls-4"
Testing "7-no-absento-multiple-calls-6"
Testing "!-correct-7-no-absento-multiple-calls-6-a"
Testing "!-correct-7-no-absento-multiple-calls-6-b"
Testing "abort-1"
Testing "abort-2"
Testing "symbolic-exec-prog-1a"
Testing "symbolic-exec-prog-1b"
Testing "symbolic-exec-prog-1c"
Testing "symbolic-exec-prog-1-subexpr-1"
Testing "rember-0"
Testing "rembero-overlapping-0"
Testing "rembero-overlapping-1"
Testing "rembero-overlapping-2"
Testing "rembero-overlapping-3"
Testing "rembero-overlapping-surprise-1"
Testing "rembero-non-overlapping-0"
Testing "rembero-non-overlapping-1"
Testing "rembero-non-overlapping-2"
Testing "rembero-non-overlapping-3"
Testing "rembero-non-overlapping-surprise-1"
Testing "rember-in-racket-in-mk-0"
Testing "rember-in-racket-in-mk-1"
Testing "rember-in-racket-in-mk-2"
Testing "rember-in-racket-in-mk-3"
Testing "rember-in-racket-in-mk-surprise-1"
Testing "closure-sanity-1"
Testing "closure-sanity-2"
Testing "closure-sanity-3"
Testing "closure-sanity-4"
Testing "closure-sanity-5"
Testing "closure-sanity-6"
Testing "closure-sanity-7"
Testing "closure-sanity-8"
Testing "closure-sanity-9"
Testing "closure-sanity-10"
Testing "closure-sanity-11"
Testing "closure-sanity-12"
Testing "append-Racket-1"
appendo, good divergence behavior
Testing "appendo-good-1"
Testing "appendo-good-2"
Testing "appendo-good-3"
Testing "appendo-good-4"
Testing "appendo-good-5"
Testing "appendo-good-6"
appendo, natural goal ordering, poor divergence behavior
Testing "appendo-poor-1"
Testing "appendo-poor-2"
Testing "appendo-poor-3"
Testing "appendo-poor-4"
Testing "appendo-poor-5"
Testing "appendo-poor-6"
Testing "append-in-Racket-in-mk-1"
Testing "append-in-Racket-in-mk-2-with-quote"
Testing "append-in-Racket-in-mk-3-with-quote"
Testing "append-in-Racket-in-mk-4-with-quote"
Testing "append-in-Racket-in-mk-5-with-quote"
Testing "append-in-Racket-in-mk-6-with-quote"
Testing "append-in-Racket-in-mk-2-no-quote-a"
Testing "append-in-Racket-in-mk-2-no-quote-b"
Testing "append-in-Racket-in-mk-3-no-quote-a"
Testing "append-in-Racket-in-mk-3-no-quote-b"
Testing "append-in-Racket-in-mk-4-no-quote-a"
Testing "append-in-Racket-in-mk-4-no-quote-b"
Testing "append-in-Racket-in-mk-5-no-quote-a"
Testing "append-in-Racket-in-mk-6-no-quote-a"
Testing "append-in-Racket-in-mk-6-no-quote-b"
Testing "append-variable-in-application-position-cheating-1"
Testing "append-variable-in-application-position-less-cheating-1"
Testing "append-variable-in-application-position-no-cheating-1"
Testing "append-variable-in-application-position-no-cheating-2"
Testing "append-variable-in-s-position-cheating-1"
Testing "append-variable-in-s-position-no-cheating-1"
Testing "append-variable-in-s-position-no-cheating-2"
calculating (I love you) programs in the context of append
Testing "(I love you) in context of append 1"
Testing "(I love you) in context of append interesting a"
Testing "(I love you) in context of append interesting b"
Testing "(I love you) in context of append interesting c"
Testing "(I love you) in context of append interesting d"
Testing "(I love you) in context of append interesting e"
calculating quines in the context of append -- takes a minute
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> eval-expro
Unhandled exception
 Condition components:
   1. &undefined
   2. &who: eval
   3. &message: "unbound variable"
   4. &irritants: (eval-expro)
> eval-expo
#<procedure eval-expo [char 1447 of interp-no-match.scm]>
> evalo
#<procedure evalo [char 1385 of interp-no-match.scm]>
> (run 1 (q) (evalo '(lambda (x) x) q))
((closure (lambda (x) x)
   ((list val closure (lambda x x) ()) (not val prim . not)
     (equal? val prim . equal?) (symbol? val prim . symbol?)
     (cons val prim . cons) (null? val prim . null?)
     (car val prim . car) (cdr val prim . cdr))))
> (run* (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
((a b c d e))
> (run* (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(_.0)
> (run 1 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append ,q '(d e)))
     '(a b c d e)))
('(a b c))
> (run* (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append ,q '(d e)))
     '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append ,q '(d e)))
     '(a b c d e)))
('(a b c)
 (((lambda _.0 '(a b c))) (=/= ((_.0 quote))) (sym _.0)))
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
((a b c))
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(_.0)
> (run 1 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0)))
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0))
  ((lambda (_.0 _.1) '(a b c d e))
    (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1)))
> (run 3 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0))
  ((lambda (_.0 _.1) '(a b c d e))
    (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
  append)
> (run 1 (q)
    (absento 'a q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(append)
> (run 2 (q)
    (absento 'a q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(append
  ((lambda (_.0 _.1) (list (car _.0) 'b 'c 'd 'e))
    (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 car)) ((_.0 list))
      ((_.0 quote)) ((_.1 a)) ((_.1 car)) ((_.1 list))
      ((_.1 quote)))
    (sym _.0 _.1)))
> (run 2 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(append
  (((lambda _.0 append))
    (=/= ((_.0 a)) ((_.0 append)) ((_.0 b))) (sym _.0)))
> (run 3 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
(append
  (((lambda _.0 append))
    (=/= ((_.0 a)) ((_.0 append)) ((_.0 b))) (sym _.0))
  (((lambda _.0 append) _.1)
    (=/= ((_.0 a)) ((_.0 append)) ((_.0 b))) (num _.1)
    (sym _.0)))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        ,q)
     '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(_.0)
> (run 1 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(_.0)
> (run 1 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        ,q)
     '(a b c d e)))
('(a b c d e))
> (run 2 (q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        ,q)
     '(a b c d e)))
('(a b c d e)
  (((lambda _.0 '(a b c d e))) (=/= ((_.0 quote))) (sym _.0)))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        ,q)
     '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
((_.0 (absento (a _.0) (b _.0))))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? ,q)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(l)
> (run 2 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? ,q)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(l
  ((letrec ((_.0 (lambda _.1 _.2))) l)
    (=/= ((_.0 l)) ((_.1 a)) ((_.1 b))) (sym _.1)
    (absento (a _.0) (a _.2) (b _.0) (b _.2))))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (,q l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(null?)
> (run 1 (q)
    (fresh (x y)
      (== (list x y) q)
      (absento 'a q)
      (absento 'b q)
      (evalo
       `(letrec 
            ((append (lambda (l s)
                       (if (,x ,y)
                           s
                           (cons (car l) (append (cdr l) s))))))
          (append '(a b c) '(d e)))
       '(a b c d e))))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
((_.0 (absento (a _.0) (b _.0))))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         ,q
                         (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(s)
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons ,q (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
((car l))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         ,q
                         (cons (car l) (,q (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(((lambda (_.0 _.1) (list (car _.0) 'c 'd 'e))
   (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 b)) ((_.0 car))
     ((_.0 list)) ((_.0 quote)) ((_.1 a)) ((_.1 b))
     ((_.1 car)) ((_.1 list)) ((_.1 quote)))
   (sym _.0 _.1)))
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         ,q
                         (cons (car l) (,q (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  C-c C-cUnhandled exception
 Condition components:
   1. &interrupted
   2. &message: "received an interrupt signal"
> (run 1 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (evalo
     `(letrec 
          ((append (lambda (l s)
                     (if (null? l)
                         s
                         (cons (car l) (,q (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
(append)
> 