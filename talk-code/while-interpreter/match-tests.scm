;(load "mk/mk.scm")
;(load "interp-no-match.scm")
;(load "match-extension.scm")
;(load "mk/test-check.scm")

(test "deconstruct-closure-1"
  (run* (q)
    (evalo
     '(match (lambda (y) (y z))
        [`(,x ,y ,z) (cons x y)])
     q))
  '())

(test "deconstruct-closure-2"
  (run* (q)
    (evalo
     '(match (lambda (y) (y z))
        [`(closure ,y ,z) (cons y z)])
     q))
  '())

(test "deconstruct-list-with-closure-1"
  (run* (q)
    (evalo
     '(match (list (lambda (y) (y z)))
        [`((,x ,y ,z)) (cons x y)])
     q))
  '())

(test "deconstruct-list-with-closure-2"
  (run* (q)
    (evalo
     '(match (list (lambda (y) (y z)))
        [`((closure ,y ,z)) (cons y z)])
     q))
  '())

(test "deconstruct-list-with-closure-3"
  (run* (q)
    (evalo
     '(match (list (lambda (y) (y z)))
        [`(,x) x])
     q))
  `((closure (lambda (y) (y z)) ,initial-env)))



(test "env-match-1"
  (run* (q)
    (evalo
     '((lambda (w)
         (match '(lambda (y) (y z))
           [`(lambda (,x) ,body) (cons w body)]))
       6)
      q))
  '((6 y z)))



(test "match-0"
  (run* (q) (evalo '(match 5) q))
  '())

(test "match-1a"
  (run* (q) (evalo '(match 5 [5 6]) q))
  '(6))

(test "match-1b"
  (run* (q) (evalo '(match 5 [x 6]) q))
  '(6))

(test "match-1c"
  (run* (q) (evalo '(match 5 [x x]) q))
  '(5))

(test "match-1d"
  (run* (q) (evalo '(match 5 [5 6] [7 8]) q))
  '(6))

(test "match-1e"
  (run* (q) (evalo '(match 5 [x 6] [y 7]) q))
  '(6))

(test "match-1f"
  (run* (q) (evalo '(match 5 [x 6] [x 7]) q))
  '(6))



(test "match-2"
  (run* (q) (evalo '(match (cons 5 6) [`(,x . ,y) 7]) q))
  '(7))

(test "match-3"
  (run* (q) (evalo '(match (cons 5 6) [`(,x . ,y) x]) q))
  '(5))

(test "match-4a"
  (run* (q) (evalo '(match (cons 5 6) [`(,x . ,y) y]) q))
  '(6))

(test "match-4b"
  (run* (q) (evalo
              '(match (cons 5 6)
                 [`(,x . ,y) y]
                 [`(,x . ,y) x])
              q))
  '(6))

(test "match-5"
  (run* (q) (evalo '(match (cons 5 6) [7 8]) q))
  '())

(test "match-6"
  (run* (q) (evalo '(match 4 [7 8]) q))
  '())

(test "match-7"
  (run* (q) (evalo '(match '(lambda (y) (y z)) [`(lambda (,x) ,body) (cons x body)]) q))
  '((y y z)))

(test "match-8"
  (run* (q) (evalo '(match '((lambda (y) (y z)) 5) [`(,rator ,rand) (cons rator (cons rand '()))]) q))
  '(((lambda (y) (y z)) 5)))

(test "match-9"
  (run* (q) (evalo
              '(match '((lambda (y) (y z)) 5)
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              q))
  '((app-expr (lambda (y) (y z)) 5)))

(test "match-10"
  (run* (q) (evalo
              '(match '(lambda (y) (y z))
                 [`(lambda (,x) ,body) (cons 'lambda-expr (cons x (cons body '())))]
                 [`(,rator ,rand) (cons 'app-expr (cons rator (cons rand '())))])
              q))
  '((lambda-expr y (y z))))

(test "match-11"
  (run* (q) (evalo
              '(match '(5 6 7)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              q))
  '((first 5 6 7)))

(test "match-12"
  (run* (q) (evalo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              q))
  '((second 5 6 7)))

(test "match-13"
  (run* (q) (evalo
              '(match '(5 6 7)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              q))
  '((second 5 6 7)))

(test "match-14"
  (run* (q) (evalo
              '(match '(5 6 5)
                 [`(,x ,y ,z) (cons 'first (cons x (cons y (cons z '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              q))
  '((first 5 6 5)))

(test "match-15"
  (run* (q) (evalo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,u ,v ,w) (cons 'second (cons u (cons v (cons w '()))))])
              q))
  '((first 5 6 5)))

(test "match-16a"
  (run* (q) (evalo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (cons 'first (cons x (cons y (cons x '()))))]
                 [`(,x ,y ,z) (cons 'second (cons x (cons y (cons z '()))))])
              q))
  '((first 5 6 5)))

(test "match-16b"
  (run* (q) (evalo
              '(match '(5 6 5)
                 [`(,x ,y ,x) (list 'first x y x)]
                 [`(,x ,y ,z) (list 'second x y z)])
              q))
  '((first 5 6 5)))

(test "match-17"
  (run* (q) (evalo '(match '#t [#f 6] [#t 8]) q))
  '(8))



;; Racket-compatible 'symbol?' predicate syntax
;;
;; `(lambda (,(? symbol? x)) ,body)
;;
(test "match-symbol-0a"
  (run* (q) (evalo
             '(match 'w
                [(? symbol? y) y])
             q))
  '(w))

(test "match-symbol-1a"
  (run* (q) (evalo
             '(match '(lambda (y) (y z))
                [`(lambda (,(? symbol? x)) ,body) (cons x body)])
             q))
  '((y y z)))

(test "match-symbol-1b"
  (run* (q)
    (evalo
     '(match '(lambda (y) (y z))
        [`(lambda (,(? symbol? x)) ,e) (cons x e)])
     q))
  '((y y z)))

(test "match-1b-backwards-1"
  (run* (q)
    (evalo
      `(match '(lambda (y) (y z))
         [,q (cons x e)])
     '(y y z)))
  '(`(lambda (,x) ,e) (`(lambda (,x) ,e unquote _.0) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) `(lambda (,(? symbol? x)) ,e) (`(lambda (,(? symbol? x)) ,e unquote _.0) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,_.0 (,x) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,(? symbol? _.0) (,x) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(lambda (,x unquote _.0) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(lambda (,(? symbol? x) unquote _.0) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(lambda (,x unquote _.0) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(lambda (,x unquote _.0) ,e unquote _.0) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,_.0 (,x) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,x) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(lambda (,(? symbol? x) unquote _.0) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(lambda (,(? symbol? x) unquote _.0) ,e unquote _.0) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,_.0 (,(? symbol? x)) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,(? symbol? _.0) (,(? symbol? x)) ,e) (=/= ((_.0 cons)) ((_.0 e)) ((_.0 x))) (sym _.0)) (`(,_.0 (,(? symbol? x)) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,(? symbol? x)) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,_.0 (,x unquote _.1) ,e) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,x unquote _.1) ,e) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,_.0 (,(? symbol? x) unquote _.1) ,e) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,(? symbol? x) unquote _.1) ,e) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,_.0 (,x unquote _.1) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,x unquote _.1) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,_.0 (,x unquote _.1) ,e unquote _.2) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 _.2)) ((_.1 cons)) ((_.1 e)) ((_.1 x)) ((_.2 cons)) ((_.2 e)) ((_.2 x))) (sym _.0 _.1 _.2)) (`(,(? symbol? _.0) (,x unquote _.1) ,e unquote _.2) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 _.2)) ((_.1 cons)) ((_.1 e)) ((_.1 x)) ((_.2 cons)) ((_.2 e)) ((_.2 x))) (sym _.0 _.1 _.2)) (`(,_.0 (,(? symbol? x) unquote _.1) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,(? symbol? _.0) (,(? symbol? x) unquote _.1) ,e unquote _.1) (=/= ((_.0 _.1)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 cons)) ((_.1 e)) ((_.1 x))) (sym _.0 _.1)) (`(,_.0 (,(? symbol? x) unquote _.1) ,e unquote _.2) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 _.2)) ((_.1 cons)) ((_.1 e)) ((_.1 x)) ((_.2 cons)) ((_.2 e)) ((_.2 x))) (sym _.0 _.1 _.2)) (`(,(? symbol? _.0) (,(? symbol? x) unquote _.1) ,e unquote _.2) (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 cons)) ((_.0 e)) ((_.0 x)) ((_.1 _.2)) ((_.1 cons)) ((_.1 e)) ((_.1 x)) ((_.2 cons)) ((_.2 e)) ((_.2 x))) (sym _.0 _.1 _.2))))

(test "match-symbol-2"
  (run 1 (pat out) (evalo `(match ,pat [`(lambda (,(? symbol? x)) ,body) (cons x body)]) out))
  '((('(lambda (_.0) _.1) (_.0 . _.1)) (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0) (absento (closure _.1) (prim _.1)))))

(test "match-symbol-3"
  (run 4 (pat out) (evalo `(match ,pat [`(lambda (,(? symbol? x)) ,body) (cons x body)]) out))
  '((('(lambda (_.0) _.1) (_.0 . _.1)) (=/= ((_.0 closure)) ((_.0 prim))) (sym _.0) (absento (closure _.1) (prim _.1))) ((((lambda _.0 '(lambda (_.1) _.2))) (_.1 . _.2)) (=/= ((_.0 quote)) ((_.1 closure)) ((_.1 prim))) (sym _.0 _.1) (absento (closure _.2) (prim _.2))) ((((lambda _.0 '(lambda (_.1) _.2)) _.3) (_.1 . _.2)) (=/= ((_.0 quote)) ((_.1 closure)) ((_.1 prim))) (num _.3) (sym _.0 _.1) (absento (closure _.2) (prim _.2))) ((((lambda _.0 '(lambda (_.1) _.2)) _.3 _.4) (_.1 . _.2)) (=/= ((_.0 quote)) ((_.1 closure)) ((_.1 prim))) (num _.3 _.4) (sym _.0 _.1) (absento (closure _.2) (prim _.2)))))

(test "match-symbol-4"
  (run 7 (body) (evalo `(match '(lambda (y) (y z)) [`(lambda (,(? symbol? x)) ,body) ,body]) '(y y z)))
  '('(y y z) (((lambda _.0 '(y y z))) (=/= ((_.0 quote))) (sym _.0)) (((lambda _.0 '(y y z)) _.1) (=/= ((_.0 quote))) (num _.1) (sym _.0)) (list x x 'z) (((lambda _.0 '(y y z)) _.1 _.2) (=/= ((_.0 quote))) (num _.1 _.2) (sym _.0)) (((lambda _.0 '(y y z)) _.1 _.2 _.3) (=/= ((_.0 quote))) (num _.1 _.2 _.3) (sym _.0)) (((lambda _.0 '(y y z)) body) (=/= ((_.0 quote))) (sym _.0))))




(test "match-1a-backwards"
  (run* (q) (evalo `(match 5
                          [,q 6])
                       '6))
  '(5
    `5
    (_.0 (sym _.0))
    ((? number? _.0) (sym _.0))
    (`,_.0 (sym _.0))
    (`,(? number? _.0) (sym _.0))))


(test "match-1c-backwards"
  (run* (q) (evalo `(match 5 [,q x]) 5))
  '(x
    (? number? x)
    `,x
    `,(? number? x)))

(test "match-8-backwards-verify-a"
  (run* (q)
    (evalo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-b"
  (run* (q)
    (evalo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote _.0) (cons rator (cons rand '()))])
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-c"
  (run* (q)
    (evalo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand unquote foo) (cons rator (cons rand '()))])
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-d"
  (run* (q)
    (evalo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . (unquote foo)) (cons rator (cons rand '()))])
      q))
  '(((lambda (y) (y z)) 5)))

(test "match-8-backwards-verify-e"
  (run* (q)
    (evalo
      '(match '((lambda (y) (y z)) 5) [`(,rator ,rand . ,foo) (cons rator (cons rand '()))])
      q))
  '(((lambda (y) (y z)) 5)))



(test "evalo-1"
  (run* (q) (evalo '5 q))
  '(5))

(test "evalo-2"
  (run* (q) (evalo 'x q))
  '())

(test "evalo-3"
  (run* (q) (evalo '(lambda (x) x) q))
  `((closure (lambda (x) x) ,initial-env)))

(test "evalo-4"
  (run* (q) (evalo '((lambda (x) x) 5) q))
  '(5))

(test "evalo-5"
  (run* (q) (evalo '((lambda (x) (lambda (y) x)) 5) q))
  `((closure (lambda (y) x) ((x val . 5) . ,initial-env))))



(test "quine-1"
  (run 6 (q) (evalo q q))
  '((_.0 (num _.0)) #t #f (((lambda (_.0) (list _.0 (list 'quote _.0))) '(lambda (_.0) (list _.0 (list 'quote _.0)))) (=/= ((_.0 closure)) ((_.0 list)) ((_.0 prim)) ((_.0 quote))) (sym _.0)) (((lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0))) '(lambda (_.0) ((lambda _.1 _.1) _.0 (list 'quote _.0)))) (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list)) ((_.0 prim)) ((_.0 quote)) ((_.1 closure)) ((_.1 prim))) (sym _.0 _.1)) (((lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0))) '(lambda (_.0) (list _.0 ((lambda _.1 _.1) 'quote _.0)))) (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list)) ((_.0 prim)) ((_.0 quote)) ((_.1 closure)) ((_.1 prim))) (sym _.0 _.1))))

(test "closure-generation"
  (run 10 (q)
    (evalo
     q
     `(closure (lambda (x) x) ,initial-env)))
  '((lambda (x) x) ((lambda () (lambda (x) x))) ((match _.0 (_.0 (lambda (x) x)) . _.1) (num _.0)) (and (lambda (x) x)) (car (list (lambda (x) x))) ((car (list (lambda (x) x) _.0)) (num _.0)) ((car (list (lambda (x) x) _.0 _.1)) (num _.0 _.1)) ((lambda () ((lambda () (lambda (x) x))))) ((match _.0 (`_.0 (lambda (x) x)) . _.1) (num _.0)) (or (lambda (x) x))))

(printf "Long running tests...\n")

(printf "This test takes a while...\n")
(test "match-8-backwards"
  (run* (q)
    (evalo
      `(match '((lambda (y) (y z)) 5)
         [,q (cons rator (cons rand '()))])
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
    (evalo
     `(match '((lambda (y) (y z)) w)
        [,q (cons rator (cons rand '()))])
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



#|

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

|#
