;(load "mk/mk.scm")
;(load "interp-no-match.scm")
;;; we load the interpreter, but not the 'match' extension
;(load "mk/test-check.scm")

(let ()

  (define rember
    (lambda (x ls)
      (cond
        ((null? ls) '())
        ((equal? (car ls) x) (cdr ls))
        (else (cons (car ls) (rember x (cdr ls)))))))

  (test "rember-0"
    (rember 'y '(w y z y))
    '(w z y))

  )

(let ()
  
  (define rembero
    (lambda (x ls out)
      (conde
        ((== '() ls) (== '() out))
        ((fresh (a d)
           (== `(,a . ,d) ls)
           (conde
             ((== x a) (== d out))
             ((fresh (res)
                (== `(,a . ,res) out)
                (rembero x d res)))))))))

  (test "rembero-overlapping-0"
    (run* (q) (rembero 'y '(w y z y) q))
    '((w z y) (w y z) (w y z y)))
  
  (test "rembero-overlapping-1"
    (run* (q) (rembero 'y '(y y z) q))
    '((y z) (y z) (y y z)))

  (test "rembero-overlapping-2"
    (run* (q) (rembero 'y '(y) q))
    '(() (y)))

  (test "rembero-overlapping-3"
    (run* (q) (rembero q '(w y z y) '(w z y)))
    '(y))
    
  (test "rembero-overlapping-surprise-1"
    (run* (q) (rembero 'y '(y) '(y)))
    '(_.0))
  
  )

(let ()
  
  (define rembero
    (lambda (x ls out)
      (conde
        ((== '() ls) (== '() out))
        ((fresh (a d)
           (== `(,a . ,d) ls)
           (conde
             ((== x a) (== d out))
             ((fresh (res)
                (=/= x a)
                (== `(,a . ,res) out)
                (rembero x d res)))))))))

  (test "rembero-non-overlapping-0"
    (run* (q) (rembero 'y '(w y z y) q))
    '((w z y)))
  
  (test "rembero-non-overlapping-1"
    (run* (q) (rembero 'y '(y y z) q))
    '((y z)))

  (test "rembero-non-overlapping-2"
    (run* (q) (rembero 'y '(y) q))
    '(()))

  (test "rembero-non-overlapping-3"
    (run* (q) (rembero q '(w y z y) '(w z y)))
    '(y))
  
  (test "rembero-non-overlapping-surprise-1"
    (run* (q) (rembero 'y '(y) '(y)))
    '())
  
  )

(test "rember-in-racket-in-mk-0"
  (run* (q)
    (evalo
      `(letrec ((rember
                 (lambda (x ls)
                   (if (null? ls)
                       '()
                       (if (equal? (car ls) x)
                           (cdr ls)
                           (cons (car ls) (rember x (cdr ls))))))))
         (rember 'y '(w y z y)))
     q))
  '((w z y)))

(test "rember-in-racket-in-mk-1"
  (run* (q)
    (evalo
      `(letrec ((rember
                 (lambda (x ls)
                   (if (null? ls)
                       '()
                       (if (equal? (car ls) x)
                           (cdr ls)
                           (cons (car ls) (rember x (cdr ls))))))))
         (rember 'y '(y y z)))
     q))
  '((y z)))

(test "rember-in-racket-in-mk-2"
  (run* (q)
    (evalo
      `(letrec ((rember
                 (lambda (x ls)
                   (if (null? ls)
                       '()
                       (if (equal? (car ls) x)
                           (cdr ls)
                           (cons (car ls) (rember x (cdr ls))))))))
         (rember 'y '(y)))
     q))
  '(()))

(test "rember-in-racket-in-mk-3"
  (run* (q)
    (evalo
      `(letrec ((rember
                 (lambda (x ls)
                   (if (null? ls)
                       '()
                       (if (equal? (car ls) x)
                           (cdr ls)
                           (cons (car ls) (rember x (cdr ls))))))))
         (rember ',q '(w y z y)))
     '(w z y)))
  '(y))

(test "rember-in-racket-in-mk-surprise-1"
  (run* (q)
    (evalo
      `(letrec ((rember
                 (lambda (x ls)
                   (if (null? ls)
                       '()
                       (if (equal? (car ls) x)
                           (cdr ls)
                           (cons (car ls) (rember x (cdr ls))))))))
         (rember 'y '(y)))
     '(y)))
  '())


;;; sanity tests, to ensure we are not mistaking closures for normal lists
;;;
;;; real solution is to use structs to represent closures

(test "closure-sanity-1"
  (run* (q)
    (evalo '(letrec ((_.0 (lambda _.1 '(I love you))))
              (apply _.0 _.0))
           q))
  '())

(test "closure-sanity-2"
  (run* (q)
    (evalo '(letrec ((_.0 (lambda _.1 '(I love you))))
              (_.0 _.0))
           q))
  '((I love you)))

(test "closure-sanity-3"
  (run* (q)
    (evalo '(letrec ((_.0 (lambda (_.1) '(I love you))))
              (apply _.0 _.0))
           q))
  '())

(test "closure-sanity-4"
  (run* (q)
    (evalo '(letrec ((_.0 (lambda (_.1) '(I love you))))
              (_.0 _.0))
           q))
  '((I love you)))

(test "closure-sanity-5"
  (run* (q)
    (evalo '((lambda (f) (apply f f))
             (lambda (x) '(I love you)))
           q))
  '())

(test "closure-sanity-6"
  (run* (q)
    (evalo '((lambda (f) (f f))
             (lambda (x) '(I love you)))
           q))
  '((I love you)))

(test "closure-sanity-7"
  (run* (q)
    (evalo '((lambda (f) (apply f f))
             (lambda (x) 5))
           q))
  '())

(test "closure-sanity-8"
  (run* (q)
    (evalo '((lambda (f) (f f))
             (lambda (x) 5))
           q))
  '(5))

(test "closure-sanity-9"
  (run* (q)
    (evalo '((lambda args (car args))
             (lambda (x) x))
           q))
  `((closure (lambda (x) x) ,initial-env)))

(test "closure-sanity-10"
  (run* (q)
    (evalo '((lambda args (cdr args))
             (lambda (x) x))
           q))
  '(()))

(test "closure-sanity-11"
  (run* (q)
    (evalo '((lambda (y) (car y))
             (lambda (x) x))
           q))
  '())

(test "closure-sanity-12"
  (run* (q)
    (evalo '(lambda (x) x)
           q))
  `((closure (lambda (x) x) ,initial-env)))


;;; append in Racket, using define

;;;   define in a 'let' to avoid shadowing 'append' used in 'mk' -- libraries or modules are a better solution
(let ()
  (define append
    (lambda (l s)
      (cond
        ((null? l) s)
        (else (cons (car l) (append (cdr l) s))))))

  (test "append-Racket-1"
    (append '(a b c) '(d e))
    '(a b c d e))
  )


(let ()
  
;;; appendo in miniKanren, using conde, good divergence order
  (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))))))

  (display "appendo, good divergence behavior")
  (newline)
  
;;;    run forwards, all ground
  (test "appendo-good-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))

;;;    run backwards, partially-instantiated terms, all fresh
  (test "appendo-good-2"
    ;; easy test
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))

  (test "appendo-good-3"
    ;; harder test
    (run* (q) (appendo q '(d e) '(a b c d e)))
    '((a b c)))

  (test "appendo-good-4"
    (run* (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-good-5"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))

  (test "appendo-good-6"
    (run* (x y) (appendo `(a b . ,x) y '(a b c d e)))
    '((() (c d e))
      ((c) (d e))
      ((c d) (e))
      ((c d e) ())))

  )


;;;    show poor divergence behavior with natural goal reordering
(let ()
  
;;; appendo in miniKanren, using conde, natural goal ordering, poor
;;; divergence behavior
  (define appendo
    (lambda (l s out)
      (conde
        ((== '() l) (== s out))
        ((fresh (a d res)
           (== `(,a . ,d) l)
           (appendo d s res)
           (== `(,a . ,res) out))))))

  (display "appendo, natural goal ordering, poor divergence behavior")
  (newline)
  
;;;    run forwards, all ground
  (test "appendo-poor-1"
    (run* (q) (appendo '(a b c) '(d e) q))
    '((a b c d e)))
  
;;;    run backwards, partially-instantiated terms, all fresh
  (test "appendo-poor-2"
    ;; easy test
    (run* (q) (appendo '(a b c) q '(a b c d e)))
    '((d e)))
  
  (test "appendo-poor-3"
    ;; harder test - run 2 diverges
    (run 1 (q) (appendo q '(d e) '(a b c d e)))
    '((a b c)))

  (test "appendo-poor-4"
    ;; run 7 diverges
    (run 6 (x y) (appendo x y '(a b c d e)))
    '((() (a b c d e))
      ((a) (b c d e))
      ((a b) (c d e))
      ((a b c) (d e))
      ((a b c d) (e))
      ((a b c d e) ())))

  (test "appendo-poor-5"
    (run 5 (x y z) (appendo x y z))
    '((() _.0 _.0)
      ((_.0) _.1 (_.0 . _.1))
      ((_.0 _.1) _.2 (_.0 _.1 . _.2))
      ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
      ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))))

  (test "appendo-poor-6"
    ;; run 5 diverges
    (run 4 (x y) (appendo `(a b . ,x) y '(a b c d e)))
    '((() (c d e))
      ((c) (d e))
      ((c d) (e))
      ((c d e) ())))

  )


;;; append in Racket, in the relational interpreter, using letrec

;;;    run forwards, all ground
(test "append-in-Racket-in-mk-1"
  (run* (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((a b c d e)))

;;;    run backwards, partially-instantiated terms, all fresh
;;;
;;;    show good divergence behavior when adding quote before a logic variable representing an unknown list

(test "append-in-Racket-in-mk-2-with-quote"
  (run* (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) ',q))
     '(a b c d e)))
  '((d e)))

(test "append-in-Racket-in-mk-3-with-quote"
  (run* (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ',q '(d e)))
     '(a b c d e)))
  '((a b c)))

(test "append-in-Racket-in-mk-4-with-quote"
  (run* (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ',x ',y))
     '(a b c d e)))
  '((() (a b c d e))
    ((a) (b c d e))
    ((a b) (c d e))
    ((a b c) (d e))
    ((a b c d) (e))
    ((a b c d e) ())))

(test "append-in-Racket-in-mk-5-with-quote"
  ;; note the absento side-conditions, due to the use of quote
  (run 5 (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ',x ',y))
     z))
  '(((() _.0 _.0)
     (absento (closure _.0)
              (prim _.0)))
    (((_.0) _.1 (_.0 . _.1))
     (absento (closure _.0) (closure _.1)
              (prim _.0) (prim _.1)))
    (((_.0 _.1) _.2 (_.0 _.1 . _.2))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    (((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3)))
    (((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3) (closure _.4)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3) (prim _.4)))))

(test "append-in-Racket-in-mk-6-with-quote"
  ;; quasiquote would be really handy here...
  (run* (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (cons 'a (cons 'b ',x)) ',y))
     '(a b c d e)))
  '((() (c d e))
    ((c) (d e))
    ((c d) (e))
    ((c d e) ())))



;;;    run* diverges with no quote, but for a surprising reason!  There are infinitely many Racket expressions that evaluate to the desired list

(test "append-in-Racket-in-mk-2-no-quote-a"
  ;; the quote in the answer gives it away...
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) ,q))
     '(a b c d e)))
  '('(d e)))

(test "append-in-Racket-in-mk-2-no-quote-b"
  (run 3 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) ,q))
     '(a b c d e)))
  '('(d e)
    (((lambda _.0 '(d e)))
     (=/= ((_.0 quote))) (sym _.0))
    (((lambda _.0 '(d e)) _.1)
     (=/= ((_.0 quote))) (num _.1)
     (sym _.0))))

(test "append-in-Racket-in-mk-3-no-quote-a"
  ;; the quote in the answer gives it away...
  (run 1 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,q '(d e)))
     '(a b c d e)))
  '('(a b c)))

(test "append-in-Racket-in-mk-3-no-quote-b"
  (run 5 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,q '(d e)))
     '(a b c d e)))
  '('(a b c)
    (((lambda _.0 '(a b c)))
     (=/= ((_.0 quote))) (sym _.0))
    (((lambda _.0 '(a b c)) _.1)
     (=/= ((_.0 quote))) (num _.1) (sym _.0))
    (((lambda _.0 '(a b c)) _.1 _.2) (=/= ((_.0 quote)))
     (num _.1 _.2) (sym _.0))
    ((letrec ((_.0 (lambda _.1 _.2))) '(a b c))
     (=/= ((_.0 quote))) (sym _.1))))


(test "append-in-Racket-in-mk-4-no-quote-a"
  ;; the quotes in the answers gives it away...
  (run 6 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())))

(test "append-in-Racket-in-mk-4-no-quote-b"
  (run 10 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     '(a b c d e)))
  '(('() '(a b c d e))
    ('(a) '(b c d e))
    ('(a b) '(c d e))
    ('(a b c) '(d e))
    ('(a b c d) '(e))
    ('(a b c d e) '())
    ('(a b c d e) (list))
    ('(a b c d) (list 'e))
    (('(a b c d e) ((lambda _.0 _.0)))
     (sym _.0))
    (('() ((lambda _.0 '(a b c d e))))
     (=/= ((_.0 quote))) (sym _.0))))

(test "append-in-Racket-in-mk-5-no-quote-a"
  ;; note the absento side-conditions, due to the use of quote
  ;;
  ;; also note that the answers are more complex that those
  ;; in 'append-in-Racket-in-mk-5-with-quote'
  (run 5 (x y z)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append ,x ,y))
     z))
  '((('() _.0 _.0) (num _.0))
    (('(_.0) _.1 (_.0 . _.1))
     (num _.1)
     (absento (closure _.0) (prim _.0)))
    (('(_.0 _.1) _.2 (_.0 _.1 . _.2))
     (num _.2)
     (absento (closure _.0) (closure _.1)
              (prim _.0) (prim _.1)))
    (('(_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3)) (num _.3)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    (('(_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
     (num _.4)
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3)))))

(test "append-in-Racket-in-mk-6-no-quote-a"
  ;; the quotes in the answers gives it away...
  ;; quasiquote would be really handy here...
  (run 4 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (cons 'a (cons 'b ,x)) ,y))
     '(a b c d e)))
  '(('() '(c d e))
    ('(c) '(d e))
    ('(c d) '(e))
    ('(c d e) '())))

(test "append-in-Racket-in-mk-6-no-quote-b"
  ;; the quotes in the answers gives it away...
  ;; quasiquote would be really handy here...
  (run 6 (x y)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (append (cons 'a (cons 'b ,x)) ,y))
     '(a b c d e)))
  '(('() '(c d e))
    ('(c) '(d e))
    ('(c d) '(e))
    ('(c d e) '())
    ('(c d e) (list))
    ('(c d) (list 'e))))




;;;    show full generality -- more general that 'appendo', since we are running in the context of a relational Racket interpeter

;;;        variable in place of `(,q '(a b c) '(d e)), with and without (absento 'a q)

(test "append-variable-in-application-position-cheating-1"
  (run 3 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(((lambda _.0 '(a b c d e)) (=/= ((_.0 quote))) (sym _.0))
    ((lambda (_.0 _.1) '(a b c d e))
     (=/= ((_.0 quote)) ((_.1 quote))) (sym _.0 _.1))
    append))

(test "append-variable-in-application-position-less-cheating-1"
  ;; infinitely many answers, equivalent to 'append'
  ;;
  ;; but also cheating answers!
  (run 6 (q)
    (absento 'a q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(append
    ((lambda (_.0 _.1) (list (car _.0) 'b 'c 'd 'e))
     (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 car)) ((_.0 list))
          ((_.0 quote)) ((_.1 a)) ((_.1 car)) ((_.1 list))
          ((_.1 quote)))
     (sym _.0 _.1))
    (((lambda _.0 append)) (=/= ((_.0 a)) ((_.0 append)))
     (sym _.0))
    (((lambda _.0 append) _.1) (=/= ((_.0 a)) ((_.0 append)))
     (num _.1) (sym _.0))
    (((lambda _.0 append) _.1 _.2)
     (=/= ((_.0 a)) ((_.0 append))) (num _.1 _.2) (sym _.0))
    ((lambda (_.0 _.1) (cons (car _.0) '(b c d e)))
     (=/= ((_.0 _.1)) ((_.0 a)) ((_.0 car)) ((_.0 cons))
          ((_.0 quote)) ((_.1 a)) ((_.1 car)) ((_.1 cons))
          ((_.1 quote)))
     (sym _.0 _.1))))

(test "append-variable-in-application-position-no-cheating-1"
  ;; infinitely many expressions equivalent to 'append'
  (run 6 (q)
    (absento 'a q)
    (absento 'b q)
    (absento 'c q)
    (absento 'd q)
    (absento 'e q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(append
    (((lambda _.0 append))
     (=/= ((_.0 a)) ((_.0 append)) ((_.0 b)) ((_.0 c))
          ((_.0 d)) ((_.0 e)))
     (sym _.0))
    (((lambda _.0 append) _.1)
     (=/= ((_.0 a)) ((_.0 append)) ((_.0 b)) ((_.0 c))
          ((_.0 d)) ((_.0 e)))
     (num _.1) (sym _.0))
    (((lambda _.0 append) _.1 _.2)
     (=/= ((_.0 a)) ((_.0 append)) ((_.0 b)) ((_.0 c))
          ((_.0 d)) ((_.0 e)))
     (num _.1 _.2) (sym _.0))
    (((lambda _.0 append) _.1 _.2 _.3)
     (=/= ((_.0 a)) ((_.0 append)) ((_.0 b)) ((_.0 c))
          ((_.0 d)) ((_.0 e)))
     (num _.1 _.2 _.3) (sym _.0))
    ((letrec ((_.0 (lambda _.1 _.2))) append)
     (=/= ((_.0 append)) ((_.1 a)) ((_.1 b)) ((_.1 c))
          ((_.1 d)) ((_.1 e)))
     (sym _.1)
     (absento (a _.0) (a _.2) (b _.0) (b _.2) (c _.0) (c _.2)
              (d _.0) (d _.2) (e _.0) (e _.2)))))

(test "append-variable-in-application-position-no-cheating-2"
  ;; refutationally complete!
  (run* (q)
    (symbolo q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        (,q '(a b c) '(d e)))
     '(a b c d e)))
  '(append))


;;;        variable in place of 's' in the definition of 'append' -- run 2, anti cheating with 'absento' and also with 'symbolo'

(test "append-variable-in-s-position-cheating-1"
  (run 5 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             ,q
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  '(s
    '(d e)
    (list 'd 'e)
    (((lambda _.0 s)) (=/= ((_.0 s))) (sym _.0))
    (((lambda _.0 s) _.1) (=/= ((_.0 s))) (num _.1) (sym _.0))))

(test "append-variable-in-s-position-no-cheating-1"
  ;; infinitely many expressions equivalent to 's'
  (run 5 (q)
    (absento 'd q)
    (absento 'e q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             ,q
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  '(s
    (((lambda _.0 s)) (=/= ((_.0 d)) ((_.0 e)) ((_.0 s)))
     (sym _.0))
    (((lambda _.0 s) _.1) (=/= ((_.0 d)) ((_.0 e)) ((_.0 s)))
     (num _.1) (sym _.0))
    (((lambda _.0 s) _.1 _.2)
     (=/= ((_.0 d)) ((_.0 e)) ((_.0 s))) (num _.1 _.2)
     (sym _.0))
    (((lambda _.0 s) _.1 _.2 _.3)
     (=/= ((_.0 d)) ((_.0 e)) ((_.0 s))) (num _.1 _.2 _.3)
     (sym _.0))))

(test "append-variable-in-s-position-no-cheating-2"
  ;; refutationally complete!  
  (run* (q)
    (symbolo q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             ,q
                             (cons (car l) (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     '(a b c d e)))
  '(s))


;;; (I love you) in context of append
(display "calculating (I love you) programs in the context of append")
(newline)

(define love-append
  (run 10000 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     '(I love you))))

(test "(I love you) in context of append 1"
  (length love-append)
  10000)

(define in-answers?
  (lambda (e ans*)
    (ormap
      (lambda (ans) (or (equal? ans e)
                   (and (pair? ans) (equal? (car ans) e))))
      ans*)))

;; ;;; interesting answers relying on 'append'
;; (test "(I love you) in context of append interesting 1"
;;   (in-answers? '(list 'I 'love (append '() 'you)) love-append)
;;   #t)

;; (test "(I love you) in context of append interesting 2"
;;   (in-answers? '(apply append '((I love) (you))) love-append)
;;   #t)

;; (test "(I love you) in context of append interesting 3"
;;   (in-answers? '(append '(I love) '(you)) love-append)
;;   #t)

;; (test "(I love you) in context of append interesting 4"
;;   (in-answers? '(apply
;;                  (lambda (_.0) (apply append _.0))
;;                  '(((I) (love you))))
;;                love-append)
;;   #t)

;; (test "(I love you) in context of append interesting 5"
;;   (in-answers? '(list
;;                  'I
;;                  'love
;;                  (apply
;;                   (lambda (_.0 _.1) (apply append _.1))
;;                   '(_.2 (() you))))
;;                love-append)
;;   #t)

;; (test "(I love you) in context of append interesting 6"
;;   (in-answers? '(apply (lambda _.0 (apply append _.0)) '((I love) (you))) love-append)
;;   #t)



(test "(I love you) in context of append interesting a"
;;; 'append' is bound to a closure, which is just as good a 'dummy'
;;; value to be ignored as any other!  
  (in-answers? '((lambda _.0 '(I love you)) append append append) love-append)
  #t)

(test "(I love you) in context of append interesting b"
  (in-answers? ''(I love you) love-append)
  #t)

(test "(I love you) in context of append interesting c"
  (in-answers? '(list 'I 'love 'you) love-append)
  #t)

(test "(I love you) in context of append interesting d"
  (in-answers? '(list 'I 'love (car (list 'you))) love-append)
  #t)

(test "(I love you) in context of append interesting e"
  (in-answers? '(letrec ((_.0 (lambda _.1 '(I love you)))) (_.0)) love-append)
  #t)




;;; quines in context of append
(display "calculating quines in the context of append -- takes a minute")
(newline)

(define quines-append
  (run 10 (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     q)))

(test "quines in context of append 1"
  (length quines-append)
  10)

(test "quines in context of append 2"
  quines-append
  '((_.0 (num _.0))
    #t
    #f
    (((lambda (_.0) (list _.0 (list 'quote _.0)))
      '(lambda (_.0) (list _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim))
          ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0)))
      '(lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 prim)) ((_.0 quote)) ((_.1 closure))
          ((_.1 prim)))
     (sym _.0 _.1))
    (((lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0)))
      '(lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list))
          ((_.0 prim)) ((_.0 quote)) ((_.1 closure))
          ((_.1 prim)))
     (sym _.0 _.1))
    (((lambda (_.0)
        (list ((lambda _.1 _.0)) (list 'quote _.0)))
      '(lambda (_.0)
         (list ((lambda _.1 _.0)) (list 'quote _.0))))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 prim)) ((_.0 quote))
          ((_.1 closure)) ((_.1 prim)))
     (sym _.0 _.1))
    (((lambda (_.0)
        ((lambda _.1 _.1) _.0 ((lambda _.2 _.2) 'quote _.0)))
      '(lambda (_.0)
         ((lambda _.1 _.1) _.0 ((lambda _.2 _.2) 'quote _.0))))
     (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 prim))
          ((_.0 quote)) ((_.1 closure)) ((_.1 prim))
          ((_.2 closure)) ((_.2 prim)))
     (sym _.0 _.1 _.2))
    (((lambda (_.0)
        (list ((lambda _.1 _.0) _.2) (list 'quote _.0)))
      '(lambda (_.0)
         (list ((lambda _.1 _.0) _.2) (list 'quote _.0))))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 prim)) ((_.0 quote))
          ((_.1 closure)) ((_.1 prim)))
     (num _.2) (sym _.0 _.1))
    (((lambda (_.0)
        (list ((lambda _.1 _.0)) ((lambda _.2 _.2) 'quote _.0)))
      '(lambda (_.0)
         (list ((lambda _.1 _.0))
               ((lambda _.2 _.2) 'quote _.0))))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 lambda))
          ((_.0 list)) ((_.0 prim)) ((_.0 quote))
          ((_.1 closure)) ((_.1 prim)) ((_.2 closure))
          ((_.2 prim)))
     (sym _.0 _.1 _.2))))


;;; member? in Racket in the relational interpreter

(test "member?-in-Racket-in-mk-1"
  (run* (q)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (member? 'cat '(dog cat cow)))
     q))
  '(#t))

(test "member?-in-Racket-in-mk-2"
  (run* (q)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (member? ',q '(dog cat cow)))
     #t))
  '(dog cat cow))

(test "member?-in-Racket-in-mk-3"
  (run* (q)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (member? ',q '(dog cat cow)))
     #f))
  '((_.0
     (=/= ((_.0 cat)) ((_.0 cow)) ((_.0 dog)))
     (absento (closure _.0) (prim _.0)))))

(test "member?-in-Racket-in-mk-4"
  (run 5 (q)
    (evalo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (member? 'cat ',q))
     #t))
  '(((cat . _.0) (absento (closure _.0) (prim _.0)))
    ((_.0 cat . _.1) (=/= ((_.0 cat)))
     (absento (closure _.0) (closure _.1)
              (prim _.0) (prim _.1)))
    ((_.0 _.1 cat . _.2) (=/= ((_.0 cat)) ((_.1 cat)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((_.0 _.1 _.2 cat . _.3)
     (=/= ((_.0 cat)) ((_.1 cat)) ((_.2 cat)))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3)))
    ((_.0 _.1 _.2 _.3 cat . _.4)
     (=/= ((_.0 cat)) ((_.1 cat)) ((_.2 cat)) ((_.3 cat)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4)))))
