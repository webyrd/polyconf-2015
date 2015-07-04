(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(define lookupo
  (lambda (x env out)
    (fresh (y val rest)
      (== `((,y . ,val) . ,rest) env)
      (conde
        [(== y x) (== val out)]
        [(=/= y x)
         (lookupo x rest out)]))))

(define not-in-envo
  (lambda (x env)
    (conde
      [(== '() env)]
      [(fresh (y val rest)
         (== `((,y . ,val) . ,rest) env)
         (=/= x y)
         (not-in-envo x rest))])))

(define eval-expro
  (lambda (expr env out)
    (conde
      [(fresh (datum)
         (== `(quote ,datum) expr)
         (== out datum)
         (not-in-envo 'quote env)
         (absento 'closure datum))]
      [(fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) out)
         (not-in-envo 'cons env)
         (eval-expro e1 env v1)
         (eval-expro e2 env v2))]
      [(symbolo expr)
       (lookupo expr env out)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (== `(closure ,x ,body ,env) out))]
      [(fresh (e1 e2 val x body env^)
         (== `(,e1 ,e2) expr)
         (eval-expro e1 env `(closure ,x ,body ,env^))
         (eval-expro e2 env val)
         (eval-expro body `((,x . ,val) . ,env^) out))])))
