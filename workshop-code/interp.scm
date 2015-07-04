(load "pmatch.scm")

; Call-by-value lambda calculus

; n         ; number
; x         ; variables
; (lambda (x) e) ; lambda expressions (abstraction)
; (e1 e2)   ; application

; environment : ((x . 5) (y . 6))
; association list representation

; closure : code + environment
; (lambda (x) (* x x))    ((y . 6))
;
; (closure x          ; formal argument
;          (* x x)    ; body
;          ((y . 6))) ; environment

(define lookup
  (lambda (x env)
    (cond
      [(null? env)
       (error 'lookup "unbound variable")]
      [(pmatch env
         [((,y . ,val) . ,rest)
          (if (eq? y x)
              val
              (lookup x rest))])])))

(define eval-expr
  (lambda (expr env)
    (pmatch expr
      [,n (guard (number? n))
       n]
      [(* ,e1 ,e2)
       (* (eval-expr e1 env)
          (eval-expr e2 env))]
      [(if ,te ,ce ,ae)
       (if (eval-expr te env)
           (eval-expr ce env)
           (eval-expr ae env))]
      [,x (guard (symbol? x)) ; z
       (lookup x env)]      
      [(lambda (,x) ,body) ; (lambda (z) (* z 5))
       (list 'closure
             x
             body
             env)]
      [(,e1 ,e2)
       ; evaluate e1 in env - (closure x body env^)
       ; evaluate e2 in env - val
       ; evaluate body in an extended environment:
       ;    ((x . val) . env^)
       (let ((proc (eval-expr e1 env)))
         (let ((val (eval-expr e2 env)))
           (pmatch proc
             [(closure ,x ,body ,env^)
              (eval-expr body
                         (cons (cons x val) env^))])))])))
