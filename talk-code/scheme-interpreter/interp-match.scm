(load "mk/mk.scm")

;; match grammar
;;
;;
;; (match ,expr ,clause ,clauses ...)
;;
;; clause ::= (,toppattern ,expr)
;;
;; toppattern ::= selfevalliteral | pattern | (quasiquote ,quasipattern)
;;
;; pattern ::= var | (? ,pred ,var)
;;
;; quasipattern ::= literal | (,quasipattern . ,quasipattern) | (unquote ,pattern)
;;
;; selfevalliteral ::= number | #t | #f
;;
;; literal ::= selfevalliteral | symbol | ()
;;
;; var ::= <symbol>
;;
;; pred ::= symbol? | number?



;; really should be a constraint built into miniKanren
(define not-symbolo
  (lambda (t)
    (conde
      [(== #f t)]
      [(== #t t)]
      [(numbero t)]
      [(fresh (a d)
         (== `(,a . ,d) t))])))

(define not-numbero
  (lambda (t)
    (conde
      [(== #f t)]
      [(== #t t)]
      [(symbolo t)]
      [(fresh (a d)
         (== `(,a . ,d) t))])))

(define self-eval-literalo
  (lambda (t)
    (conde
      [(numbero t)]
      [(booleano t)])))

(define literalo
  (lambda (t)
    (conde
      [(numbero t)]
      [(symbolo t)]
      [(booleano t)]
      [(== '() t)])))

(define booleano
  (lambda (t)
    (conde
      [(== #f t)]
      [(== #t t)])))

(define (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))

(define (lookupo x env val)
  (fresh (y v env^)
    (== `((,y . ,v) . ,env^) env)
    (conde
      [(== x y) (== v val)]
      [(=/= x y) (lookupo x env^ val)])))

(define (not-in-envo x env)
  (conde
    [(== '() env)]
    [(fresh (y v env^)
       (== `((,y . ,v) . ,env^) env)
       (=/= x y)
       (not-in-envo x env^))]))

(define (eval-expo expr env val)
  (conde
    [(numbero expr)
     (== expr val)]
    [(booleano expr)
     (== expr val)]
    [(== `(quote ,val) expr)
     (absento 'closure val)
     (not-in-envo 'quote env)]
    [(symbolo expr)
     (lookupo expr env val)]
    [(fresh (x body)
       (== `(lambda (,x) ,body) expr)
       (== `(closure ,x ,body ,env) val)
       (symbolo x)
       (not-in-envo 'lambda env))]
    [(fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v1 . ,v2) val)
       (not-in-envo 'cons env)
       (eval-expo e1 env v1)
       (eval-expo e2 env v2))]
    [(fresh (rator rand x body env^ arg)
       (== `(,rator ,rand) expr)
       (symbolo x)
       (eval-expo rator env `(closure ,x ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `((,x . ,arg) . ,env^) val))]
    [(fresh (against-expr against-val clause clauses)
       (== `(match ,against-expr ,clause . ,clauses) expr)
       (not-in-envo 'match env)
       (eval-expo against-expr env against-val)
       (match-clauses against-val `(,clause . ,clauses) env val))]))

(define (match-clauses against-val clauses env val)
  (fresh (top-pattern result-expr d penv)
    (== `((,top-pattern ,result-expr) . ,d) clauses)
    (conde
      [(fresh (env^)
         (top-pattern-matches top-pattern against-val '() penv)
         (appendo penv env env^)
         (eval-expo result-expr env^ val))]
      [(top-pattern-but-doesnt-match top-pattern against-val '() penv)
       (match-clauses against-val d env val)])))



(define (top-pattern-matches top-pattern against-val penv penv-out)
  (conde
    [(self-eval-literalo top-pattern) (== top-pattern against-val) (== penv penv-out)]
    [(pattern-matches top-pattern against-val penv penv-out)]
    [(fresh (quasi-pattern)
      (== (list 'quasiquote quasi-pattern) top-pattern)
      (quasi-pattern-matches quasi-pattern against-val penv penv-out))]))

(define (top-pattern-but-doesnt-match top-pattern against-val penv penv-out)
  (conde
    [(self-eval-literalo top-pattern) (=/= top-pattern against-val) (== penv penv-out)]
    [(pattern-but-doesnt-match top-pattern against-val penv penv-out)]
    [(fresh (quasi-pattern)
      (== (list 'quasiquote quasi-pattern) top-pattern)
      (quasi-pattern-but-doesnt-match quasi-pattern against-val penv penv-out))]))


(define (var-pattern-matches var against-val penv penv-out)
  (fresh (val)
    (symbolo var)
    (conde
      [(== against-val val)
       (== penv penv-out)
       (lookupo var penv val)]
      [(== `((,var . ,against-val) . ,penv) penv-out)
       (not-in-envo var penv)])))

(define (var-pattern-but-doesnt-match var against-val penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= against-val val)
    (== penv penv-out)
    (lookupo var penv val)))



(define (pattern-matches pattern against-val penv penv-out)
  (conde
    [(var-pattern-matches pattern against-val penv penv-out)]
    [(fresh (var pred val)
      (== `(? ,pred ,var) pattern)
      (conde
        [(== 'symbol? pred)
         (symbolo against-val)]
        [(== 'number? pred)
         (numbero against-val)])
      (var-pattern-matches var against-val penv penv-out))]))

(define (pattern-but-doesnt-match pattern against-val penv penv-out)
  (conde
    [(var-pattern-but-doesnt-match pattern against-val penv penv-out)]
    [(fresh (var pred val)
       (== `(? ,pred ,var) pattern)       
       (== penv penv-out)
       (symbolo var)       
       (conde
         [(== 'symbol? pred)
          (conde
            [(not-symbolo against-val)]
            [(symbolo against-val)
             (var-pattern-but-doesnt-match var against-val penv penv-out)])]
         [(== 'number? pred)
          (conde
            [(not-numbero against-val)]
            [(numbero against-val)
             (var-pattern-but-doesnt-match var against-val penv penv-out)])]))]))



(define (quasi-pattern-matches quasi-pattern against-val penv penv-out)
  (conde
    [(== quasi-pattern against-val)
     (== penv penv-out)
     (literalo quasi-pattern)]   
    [(fresh (pattern)
      (== (list 'unquote pattern) quasi-pattern)
      (pattern-matches pattern against-val penv penv-out))]
    [(fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-pattern)
       (== `(,v1 . ,v2) against-val)
       (=/= 'unquote a)
       (quasi-pattern-matches a v1 penv penv^)
       (quasi-pattern-matches d v2 penv^ penv-out))]))

(define (quasi-pattern-but-doesnt-match quasi-pattern against-val penv penv-out)
  (conde
    [(=/= quasi-pattern against-val)
     (== penv penv-out)
     (literalo quasi-pattern)]
    [(fresh (pattern)
       (== (list 'unquote pattern) quasi-pattern)
       (pattern-but-doesnt-match pattern against-val penv penv-out))]
    [(fresh (a d)
       (== `(,a . ,d) quasi-pattern)
       (=/= 'unquote a)
       (conde
         [(== penv penv-out)
          (literalo against-val)]
         [(fresh (v1 v2 penv^)
            (== `(,v1 . ,v2) against-val)
            (conde
              [(quasi-pattern-but-doesnt-match a v1 penv penv^)]
              [(quasi-pattern-matches a v1 penv penv^)
               (quasi-pattern-but-doesnt-match d v2 penv^ penv-out)]))]))]))
