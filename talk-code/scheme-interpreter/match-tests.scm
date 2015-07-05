(load "interp-match.scm")
(load "mk/test-check.scm")


(test "env-match-1"
  (run* (q)
    (eval-expo
     '((lambda (w)
         (match '(lambda (y) (y z))
           [`(lambda (,x) ,body) (cons w body)]))
       6)
      '()
      q))
  '((6 y z)))



(test "match-0"
  (run* (q) (eval-expo '(match 5) '() q))
  '())

(test "match-1a"
  (run* (q) (eval-expo '(match 5 [5 6]) '() q))
  '(6))

(test "match-1b"
  (run* (q) (eval-expo '(match 5 [x 6]) '() q))
  '(6))

(test "match-1c"
  (run* (q) (eval-expo '(match 5 [x x]) '() q))
  '(5))

(test "match-1d"
  (run* (q) (eval-expo '(match 5 [5 6] [7 8]) '() q))
  '(6))

(test "match-1e"
  (run* (q) (eval-expo '(match 5 [x 6] [y 7]) '() q))
  '(6))

(test "match-1f"
  (run* (q) (eval-expo '(match 5 [x 6] [x 7]) '() q))
  '(6))



(test "match-2"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) 7]) '() q))
  '(7))

(test "match-3"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) x]) '() q))
  '(5))

(test "match-4"
  (run* (q) (eval-expo '(match (cons 5 6) [`(,x . ,y) y]) '() q))
  '(6))

(test "match-5"
  (run* (q) (eval-expo '(match (cons 5 6) [7 8]) '() q))
  '())

(test "match-6"
  (run* (q) (eval-expo '(match 4 [7 8]) '() q))
  '())

(test "match-7"
  (run* (q) (eval-expo '(match '(lambda (y) (y z)) [`(lambda (,x) ,body) (cons x body)]) '() q))
  '((y y z)))

(test "match-8"
  (run* (q) (eval-expo '(match '((lambda (y) (y z)) 5) [`(,rator ,rand) (cons rator (cons rand '()))]) '() q))
  '(((lambda (y) (y z)) 5)))

(test "match-9"
  (run* (q) (eval-expo
              '(match '((lambda (y) (y z)) 5)
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((app-expr (lambda (y) (y z)) 5)))

(test "match-10"
  (run* (q) (eval-expo
              '(match '(lambda (y) (y z))
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              '()
              q))
  '((lambda-expr y (y z))))

(test "match-11"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 7)))

(test "match-12"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-13"
  (run* (q) (eval-expo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((second 5 6 7)))

(test "match-14"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-15"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              '()
              q))
  '((first 5 6 5)))

(test "match-16"
  (run* (q) (eval-expo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              '()
              q))
  '((first 5 6 5)))


(test "match-17"
  (run* (q) (eval-expo '(match '#t [#f 6] [#t 8]) '() q))
  '(8))



;; Racket-compatible 'symbol?' predicate syntax
;;
;; `(lambda (,(? symbol? x)) ,body)
;;
(test "match-symbol-0a"
  (run* (q) (eval-expo
             '(match 'w
                [(? symbol? y) y])
             '()
             q))
  '(w))

(test "match-symbol-1"
  (run* (q) (eval-expo
             '(match '(lambda (y) (y z))
                [`(lambda (,(? symbol? x)) ,body) (cons x body)])
             '()
             q))
  '((y y z)))

(test "match-symbol-2"
  (run 1 (pat out) (eval-expo `(match ,pat [`(lambda (,(? symbol? x)) ,body) (cons x body)]) '() out))
  '((('(lambda (_.0) _.1)
      (_.0 . _.1))
     (=/= ((_.0 closure)))
   (sym _.0)
   (absento (closure _.1)))))

(test "match-symbol-3"
  (run 3 (pat out) (eval-expo `(match ,pat [`(lambda (,(? symbol? x)) ,body) (cons x body)]) '() out))
  '((('(lambda (_.0) _.1) (_.0 . _.1))
     (=/= ((_.0 closure)))
     (sym _.0)
     (absento (closure _.1)))
    ((((lambda (_.0) '(lambda (_.1) _.2)) _.3) (_.1 . _.2))
     (=/= ((_.0 quote)) ((_.1 closure)))
     (num _.3)
     (sym _.0 _.1)
     (absento (closure _.2)))
    ((((lambda (_.0) '(lambda (_.1) _.2)) #f) (_.1 . _.2))
     (=/= ((_.0 quote)) ((_.1 closure)))
     (sym _.0 _.1)
     (absento (closure _.2)))))

(test "match-symbol-4"
  (run 3 (body) (eval-expo `(match '(lambda (y) (y z)) [`(lambda (,(? symbol? x)) ,body) ,body]) '() '(y y z)))
  '('(y y z)
    (cons 'y body)
    (cons 'y '(y z))))




(test "match-1a-backwards"
  (run* (q) (eval-expo `(match 5
                          [,q 6])
                       '()
                       '6))
  '(5
    `5
    (_.0 (sym _.0))
    ((? number? _.0) (sym _.0))
    (`,_.0 (sym _.0))
    (`,(? number? _.0) (sym _.0))))


(test "match-1c-backwards"
  (run* (q) (eval-expo `(match 5 [,q x]) '() 5))
  '(x
    (? number? x)
    `,x
    `,(? number? x)))

(test "match-8-backwards-verify-a"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-b"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-c"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote foo) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-d"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . (unquote foo)) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-e"
  (run* (q)
    (eval-expo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . ,foo) (cons rator (cons rand '()))])
      '()
      q))
  '(((lambda (y) (y z)) 5)))



(test "eval-expo-1"
  (run* (q) (eval-expo '5 '() q))
  '(5))

(test "eval-expo-2"
  (run* (q) (eval-expo 'x '() q))
  '())

(test "eval-expo-3"
  (run* (q) (eval-expo '(lambda (x) x) '() q))
  '((closure x x ())))

(test "eval-expo-4"
  (run* (q) (eval-expo '((lambda (x) x) 5) '() q))
  '(5))

(test "eval-expo-5"
  (run* (q) (eval-expo '((lambda (x) (lambda (y) x)) 5) '() q))
  '((closure y x ((x . 5)))))



(test "quine-1"
  (run 6 (q) (eval-expo q '() q))
  '((_.0 (num _.0))
    #f
    #t
    (((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 quote)))
     (sym _.0))
    (((lambda (_.0)
        (cons _.0
              (cons (cons 'quote (cons _.0 '()))
                    ((lambda (_.1) '()) _.2))))
      '(lambda (_.0)
         (cons _.0
               (cons (cons 'quote (cons _.0 '()))
                     ((lambda (_.1) '()) _.2)))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 lambda))
          ((_.0 quote)) ((_.1 closure)) ((_.1 quote)))
     (num _.2) (sym _.0 _.1))
    (((lambda (_.0)
        (cons _.0
              (cons (cons 'quote (cons _.0 '()))
                    ((lambda (_.1) '()) #f))))
      '(lambda (_.0)
         (cons _.0
               (cons (cons 'quote (cons _.0 '()))
                     ((lambda (_.1) '()) #f)))))
     (=/= ((_.0 closure)) ((_.0 cons)) ((_.0 lambda))
          ((_.0 quote)) ((_.1 closure)) ((_.1 quote)))
     (sym _.0 _.1))))

(test "closure-generation"
  (run 10 (q)
    (eval-expo
     q
     '()
     '(closure x x ())))
  '((lambda (x) x)
    ((match _.0 (_.0 (lambda (x) x)) . _.1)
     (num _.0))
    (match #f (#f (lambda (x) x)) . _.0)
    ((match _.0 (`_.0 (lambda (x) x)) . _.1)
     (num _.0))
    ((match _.0 (_.1 _.2) (_.0 (lambda (x) x)) . _.3)
     (=/= ((_.0 _.1))) (num _.0 _.1))
    (match #t (#t (lambda (x) x)) . _.0)
    (((lambda (_.0) _.0) (lambda (x) x))
     (sym _.0))
    (match #f (`#f (lambda (x) x)) . _.0)
    ((match '_.0 (_.0 (lambda (x) x)) . _.1)
     (num _.0))
    ((match _.0 (#f _.1) (_.0 (lambda (x) x)) . _.2)
     (num _.0))))


(printf "Long running tests...\n")

(printf "This test takes a while...\n")
(test "match-8-backwards"
  (run* (q)
    (eval-expo
      `(match '((lambda (y) (y z)) 5)
         [,q (cons rator (cons rand '()))])
      '()
      '((lambda (y) (y z)) 5)))
  '(`(,rator ,rand)
    `(,rator ,(? number? rand))
    (`(,rator ,rand unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))
    (`(,rator ,(? number? rand) unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))))

(printf "This test takes a while...\n")
(test "match-8-backwards-b"
  (run* (q)
    (eval-expo
     `(match '((lambda (y) (y z)) w)
        [,q (cons rator (cons rand '()))])
     '()
     '((lambda (y) (y z)) w)))
  '(`(,rator ,rand)
    `(,rator ,(? symbol? rand))
    (`(,rator ,rand unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))
    (`(,rator ,(? symbol? rand) unquote _.0)
     (=/= ((_.0 cons)) ((_.0 quote))
          ((_.0 rand)) ((_.0 rator)))
     (sym _.0))))


#!eof

;; higher-order interpreter

;; ideal version
;;
;; letrec
;; multi-arg lambda/application
;; quote
;; equal?
;; if
;; error
(letrec ((eval-expr
          (lambda (expr env)
            (match expr
              [(? symbol? x) (env x)]
              [`(lambda (,(? symbol? x)) ,body)
               (lambda (a)
                 (eval-expr body (lambda (y)
                                   (if (equal? x y)
                                       a
                                       (env y)))))]
              [`(,rator ,rand)
               ((eval-expr rator env) (eval-expr rand env))]))))
  (eval-expr '((lambda (y) w) (lambda (z) z)) (lambda (y) (error 'unbound-variable y))))


;; hack to avoid adding 'error': instead of representing empty env as
;; (lambda (y) (error 'unbound-variable)), can force failure by applying a
;; function with the wrong number of arguments
;;
;; letrec
;; multi-arg lambda/application
;; quote
;; equal?
;; if
(letrec ((eval-expr
          (lambda (expr env)
            (match expr
              [(? symbol? x) (env x)]
              [`(lambda (,(? symbol? x)) ,body)
               (lambda (a)
                 (eval-expr body (lambda (y)
                                   (if (equal? x y)
                                       a
                                       (env y)))))]
              [`(,rator ,rand)
               ((eval-expr rator env) (eval-expr rand env))]))))
  (eval-expr '((lambda (y) w) (lambda (z) z)) (lambda (y) ((lambda (z) z)))))


;; another possible approach, assuming 'match' is extended to handle
;; ,(quote ,datum).  This seems a bit weird, though, in how we would
;; handle ,datum.
;;
;; letrec
;; multi-arg lambda/application
;; quote
(letrec ((eval-expr
          (lambda (expr env)
            (match expr
              [(? symbol? x) (env x)]
              [`(lambda (,(? symbol? x)) ,body)
               (lambda (a)
                 (eval-expr body (lambda (y)
                                   (match y
                                     [,(quote ,x) a]
                                     [,else (env y)]))))]
              [`(,rator ,rand)
               ((eval-expr rator env) (eval-expr rand env))]))))
  (eval-expr '((lambda (y) w) (lambda (z) z)) (lambda (y) ((lambda (z) z)))))
