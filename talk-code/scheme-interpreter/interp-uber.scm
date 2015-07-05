;; TODO
;;
;; * add quasiquote/unquote so we can easily and efficiently write
;; 'lambda' and 'fold' as macros.
;;
;; Moved apply earlier so we can run (I love you) and quines queries
;; inside of 'append' definition.

(load "mk/mk.scm")

;; supports variadic lambda: (lambda x x)


;; letrec is based on Dan Friedman's code, using the "half-closure"
;; approach from Reynold's definitional interpreters

(define empty-env '())

(define lookupo
  (lambda (x env t)
    (conde
      ((fresh (y v rest)
       (== `(ext-env ,y ,v ,rest) env)
       (conde
         ((== y x) (== v t))
         ((=/= y x) (lookupo x rest t)))))
      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (lookup-ext-reco x defs env rest t))))))

(define lookup-ext-reco
  (lambda (x defs env rest t)
    (fresh (y lam-exp others)
      (conde
        ((== '() defs) (lookupo x rest t))
        ((== `((,y ,lam-exp) . ,others) defs)
         (conde
           ((== y x) (== `(closure ,lam-exp ,env) t))
           ((=/= y x) (lookup-ext-reco x others env rest t))))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== empty-env env))
      ((fresh (y v rest)
         (== `(ext-env ,y ,v ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((fresh (defs rest)
         (== `(ext-rec ,defs ,rest) env)
         (not-in-defso x defs)
         (not-in-envo x rest))))))

(define not-in-defso
  (lambda (x defs)
    (conde
      ((== '() defs))
      ((fresh (y lam-exp others)
         (== `((,y ,lam-exp) . ,others) defs)
         (=/= y x)
         (not-in-defso x others))))))

(define eval-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d v-a v-d)
         (== `(,a . ,d) exp)
         (== `(,v-a . ,v-d) val)
         (eval-expo a env v-a)
         (eval-listo d env v-d))))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(define list-of-symbolso
  (lambda (los)
    (conde
      ((== '() los))
      ((fresh (a d)
         (== `(,a . ,d) los)
         (symbolo a)
         (list-of-symbolso d))))))

(define listo
  (lambda (ls)
    (conde
      ((== '() ls))
      ((fresh (a d)
         (== `(,a . ,d) ls)
         (listo d))))))

(define evalo
  (lambda (exp val)
    (eval-expo exp empty-env val)))

(define eval-expo
  (lambda (exp env val)
    (conde

      ((== `(quote ,val) exp)
       (absento 'closure val)
       (not-in-envo 'quote env))

      ((numbero exp) (== exp val))

      ((symbolo exp) (lookupo exp env val))
      
      ;; should possibly combine these lambda clauses, application clauses, apply clauses, and letrec clauses

      ((fresh (x body)
         (== `(lambda ,x ,body) exp)
         (== `(closure (lambda ,x ,body) ,env) val)
         (symbolo x)
         (not-in-envo 'lambda env)))
      
      ((fresh (x* body)
         (== `(lambda ,x* ,body) exp)
         (== `(closure (lambda ,x* ,body) ,env) val)
         (list-of-symbolso x*)
         (not-in-envo 'lambda env)))
      
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (eval-listo a* env val)))
      
      ;; apply for variadic procedure
      ((fresh (e e* x body env^ a* res)
         (== `(apply ,e ,e*) exp)
         (not-in-envo 'apply env)
         (symbolo x)
         (== `(ext-env ,x ,a* ,env^) res)
         (eval-expo e env `(closure (lambda ,x ,body) ,env^))
         (eval-expo e* env a*)
         (listo a*)
         (eval-expo body res val)))

      ;; apply for mult-argument procedure
      ((fresh (e e* x x* body env^ a* res)
         (== `(apply ,e ,e*) exp)
         (not-in-envo 'apply env)
         (symbolo x)
         (ext-env*o `(,x . ,x*) a* env^ res)
         (eval-expo e env `(closure (lambda (,x . ,x*) ,body) ,env^))
         (eval-expo e* env a*)
         (listo a*)
         (eval-expo body res val)))
      
      ((fresh (against-expr against-val clause clauses)
         (== `(match ,against-expr ,clause . ,clauses) exp)
         (not-in-envo 'match env)
         (eval-expo against-expr env against-val)
         (match-clauses against-val `(,clause . ,clauses) env val)))

      ((fresh (rator x rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (symbolo x)
         (== `(ext-env ,x ,a* ,env^) res)
         (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
         
         (eval-expo body res val) ;; perfect example of two serious
                                  ;; calls in which it isn't clear
                                  ;; which one should come first         
         (eval-listo rands env a*)))
      
      ((fresh (rator x* rands body env^ a* res)
         (== `(,rator . ,rands) exp)
         (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
         (eval-listo rands env a*)
         (ext-env*o x* a* env^ res)
         (eval-expo body res val)))

      ((fresh (p-name x body letrec-body)
         (== `(letrec ((,p-name (lambda ,x ,body))) ;; single-function variadic letrec version
                ,letrec-body)
             exp)
         (symbolo x)
         (not-in-envo 'letrec env)
         (eval-expo letrec-body
                    `(ext-rec ((,p-name (lambda ,x ,body))) ,env)
                    val)))
      
      ((fresh (p-name x* body letrec-body)
         (== `(letrec ((,p-name (lambda ,x* ,body))) ;; single-function multiple-argument letrec version
                ,letrec-body)
             exp)
         (list-of-symbolso x*)
         (not-in-envo 'letrec env)
         (eval-expo letrec-body
                    `(ext-rec ((,p-name (lambda ,x* ,body))) ,env)
                    val)))
      
      ;;; don't comment this out accidentally!!!
      ((prim-expo exp env val))
      
      )))

(define ext-env*o
  (lambda (x* a* env out)
    (conde
      ((== '() x*) (== '() a*) (== env out))
      ((fresh (x a dx* da* env2)
         (== `(,x . ,dx*) x*)
         (== `(,a . ,da*) a*)
         (== `(ext-env ,x ,a ,env) env2)
         (symbolo x)
         (ext-env*o dx* da* env2 out))))))

(define prim-expo
  (lambda (exp env val)
    (conde
      ((boolean-primo exp env val))
      ((and-primo exp env val))
      ((or-primo exp env val))
      ((null?-primo exp env val))
      ((symbol?-primo exp env val))
      ((not-primo exp env val))
      ((car-primo exp env val))
      ((cdr-primo exp env val))
      ((cons-primo exp env val))
      ((equal?-primo exp env val))
      ((if-primo exp env val)))))

(define boolean-primo
  (lambda (exp env val)
    (conde
      ((== #t exp) (== #t val))
      ((== #f exp) (== #f val)))))

(define and-primo
  (lambda (exp env val)
    (fresh (e*)
      (== `(and . ,e*) exp)
      (not-in-envo 'and env)
      (ando e* env val))))

(define ando
  (lambda (e* env val)
    (conde
      ((== '() e*) (== #t val))
      ((fresh (e)
         (== `(,e) e*)
         (eval-expo e env val)))
      ((fresh (e1 e2 e-rest v)
         (== `(,e1 ,e2 . ,e-rest) e*)         
         (conde
           ((== #f v)
            (== #f val)
            (eval-expo e1 env v))
           ((=/= #f v)
            (eval-expo e1 env v)
            (ando `(,e2 . ,e-rest) env val))))))))

(define or-primo
  (lambda (exp env val)
    (fresh (e*)
      (== `(or . ,e*) exp)
      (not-in-envo 'or env)
      (oro e* env val))))

(define oro
  (lambda (e* env val)
    (conde
      ((== '() e*) (== #f val))
      ((fresh (e)
         (== `(,e) e*)
         (eval-expo e env val)))
      ((fresh (e1 e2 e-rest v)
         (== `(,e1 ,e2 . ,e-rest) e*)         
         (conde
           ((=/= #f v)
            (== v val)
            (eval-expo e1 env v))
           ((== #f v)
            (eval-expo e1 env v)
            (oro `(,e2 . ,e-rest) env val))))))))


(define equal?-primo
  (lambda (exp env val)
    (fresh (e1 e2 v1 v2)
      (== `(equal? ,e1 ,e2) exp)
      (conde
        ((== v1 v2) (== #t val))
        ((=/= v1 v2) (== #f val)))
      (not-in-envo 'equal? env)
      (eval-expo e1 env v1)
      (eval-expo e2 env v2))))

(define cons-primo
  (lambda (exp env val)
    (fresh (a d v-a v-d)
      (== `(cons ,a ,d) exp)
      (== `(,v-a . ,v-d) val)
      (not-in-envo 'cons env)
      (eval-expo a env v-a)
      (eval-expo d env v-d))))

(define car-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(car ,p) exp)
      (== a val)
      (=/= 'closure a)
      (not-in-envo 'car env)
      (eval-expo p env `(,a . ,d)))))

(define cdr-primo
  (lambda (exp env val)
    (fresh (p a d)
      (== `(cdr ,p) exp)
      (== d val)
      (=/= 'closure a)
      (not-in-envo 'cdr env)
      (eval-expo p env `(,a . ,d)))))

(define not-primo
  (lambda (exp env val)
    (fresh (e b)
      (== `(not ,e) exp)
      (conde
        ((=/= #f b) (== #f val))
        ((== #f b) (== #t val)))         
      (not-in-envo 'not env)
      (eval-expo e env b))))

(define symbol?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(symbol? ,e) exp)
      (conde
        ((symbolo v) (== #t val))
        ((numbero v) (== #f val))
        ((fresh (a d)
           (== `(,a . ,d) v)
           (== #f val))))
      (not-in-envo 'symbol? env)
      (eval-expo e env v))))

(define null?-primo
  (lambda (exp env val)
    (fresh (e v)
      (== `(null? ,e) exp)
      (conde
        ((== '() v) (== #t val))
        ((=/= '() v) (== #f val)))
      (not-in-envo 'null? env)
      (eval-expo e env v))))

(define if-primo
  (lambda (exp env val)
    (fresh (e1 e2 e3 t)
      (== `(if ,e1 ,e2 ,e3) exp)
      (not-in-envo 'if env)
      (eval-expo e1 env t)
      (conde
        ((=/= #f t) (eval-expo e2 env val))
        ((== #f t) (eval-expo e3 env val))))))



;; match-related code

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


(define (regular-env-appendo env1 env2 env-out)
  (conde
    [(== empty-env env1) (== env2 env-out)]
    [(fresh (y v rest res)
       (== `(ext-env ,y ,v ,rest) env1)
       (== `(ext-env ,y ,v ,res) env-out)
       (regular-env-appendo rest env2 res))]))


(define (match-clauses against-val clauses env val)
  (fresh (top-pattern result-expr d penv)
    (== `((,top-pattern ,result-expr) . ,d) clauses)
    (conde
      [(fresh (env^)
         (top-pattern-matches top-pattern against-val '() penv)
         (regular-env-appendo penv env env^)
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
      [(== `(ext-env ,var ,against-val ,penv) penv-out)
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
