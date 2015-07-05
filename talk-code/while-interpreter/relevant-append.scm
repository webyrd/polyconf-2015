;;; Interesting and relevant answers from running the (I love you)
;;; query in the context of 'append', with numbers, 'and', and 'or'
;;; commented out of the relational interpreter.  As a result, not all
;;; of these answers may be present in a query involving any or all of
;;; these excluded clauses.

;;; The answers are taken from the 10k answers in 10k-love.scm, which
;;; were produced by the query:
;;;
;;; Note the use of a dummy query variable with a dummy disequality
;;; constraint, so we can safely map down the caar of the answers.

(run 1000 (q dummy)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l) (append (cdr l) s))))))
        ,q)
     '(I love you))
    (=/= 5 dummy)
    (absento 'or q)
    (absento 'and q))


(define deep-member?
  (lambda (x t)
    (cond
      [(null? t) #f]
      [(symbol? t) (equal? x t)]
      [(pair? t) (or (deep-member? x (car t)) (deep-member? x (cdr t)))]      
      [else #f])))

> (define p (open-input-file "10k-love.scm"))
> (define ls (read p))
> (define e* (map caar ls))
> (length e*)
10000
;;; sanity test for generated answers
> (andmap (lambda (expr) (equal? (eval expr) '(I love you))) e*)
;;; This for-each prints out only those answers that actually use 'append' in a meaningful way.
;;; We test this by seeing if the answer would change were 'append' to be replaced with 'cons'.
> (for-each (lambda (e) (when (not (equal? (eval `(let ((append cons)) ,e)) '(I love you))) (printf "~s\n" e))) e*)

;;; This for-each prints out only those answers that *don't* use 'append' in a meaningful way.
> (for-each (lambda (e) (when (equal? (eval `(let ((append cons)) ,e)) '(I love you)) (printf "~s\n" e))) e*)

;;; This for-each prints out only those answers that *don't* contain
;;; 'append' at all (other than in side-conditions).
> (for-each (lambda (e) (when (not (deep-member? 'append e)) (printf "~s\n" e))) e*)

;;; This for-each prints out only those answers that *don't* contain
;;; 'apply' or 'append' at all (other than in side-conditions).
> (for-each (lambda (e) (when (not (or (deep-member? 'append e) (deep-member? 'apply e))) (printf "~s\n" e))) e*)


(apply append (quote (() (I love you))))
(apply append (quote ((I) (love you))))
(list (quote I) (quote love) (apply append (quote (() you))))
(append (quote ()) (quote (I love you)))
(apply append (quote ((I love) (you))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply append (quote (() you))))
(apply append (quote ((I love you) ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply append (quote (() you))))
(append (quote (I)) (quote (love you)))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote (_.1)))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply append (quote (() you))))
(list (quote I) (quote love) (apply (lambda _.0 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply append (quote (() you))))
(list (quote I) (quote love) (append (quote ()) (quote you)))
(apply (lambda (_.0) (apply append _.0)) (quote ((() (I love you)))))
(append (quote (I love)) (quote (you)))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote ()))
(apply (lambda _.0 (apply append _.0)) (quote (() (I love you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (quote love) (apply append (quote (() you))))
(append (quote (I love you)) (quote ()))
(apply (lambda _.0 (list (quote I) (quote love) (apply append (quote (() you))))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2))) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote ())) (apply append (quote (() you))))
(list (quote I) (quote love) (apply (lambda _.0 (apply append (quote (() you)))) (quote (_.1))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1 _.2))) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (append (quote ()) (quote you)))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (append (quote ()) (quote you)))
(apply (lambda (_.0) (apply append (quote (() (I love you))))) (quote (_.1)))
(list (quote I) (quote love) (apply (lambda (_.0) (apply append _.0)) (quote ((() you)))))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 (() (I love you)))))
(apply (lambda _.0 (apply append (quote ((I love) (you))))) (quote ()))
(apply (lambda _.0 (append _.0 (quote (I love you)))) (quote ()))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1)))
(apply (lambda (_.0) (apply append _.0)) (quote (((I) (love you)))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote (_.1 _.2)))
(list (quote I) (quote love) (apply (lambda (_.0) (apply append (quote (() you)))) (quote (_.1))))
(apply append (list (quote ()) (quote (I love you))))
((lambda _.0 _.0) (quote I) (quote love) (apply append (quote (() you))))
(list (quote I) (quote love) (apply (lambda _.0 (apply append _.0)) (quote (() you))))
(list ((lambda _.0 (quote I))) (quote love) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote (_.1 _.2 _.3)))
(list (quote I) ((lambda _.0 (quote love))) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote ((I love you) ())))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote (_.3))) (apply append (quote (() you))))
(letrec ((_.0 (lambda _.1 _.2))) (apply append (quote (() (I love you)))))
(apply (lambda _.0 (list (quote I) (quote love) (apply append (quote (() you))))) (quote (_.1)))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() (I love you))))) _.0)) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (apply (lambda _.3 (quote love)) (quote ())) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply (lambda _.2 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2 _.3))) (apply append (quote (() you))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply (lambda _.2 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply (lambda _.2 (apply append (quote (() you)))) (quote ())))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) _.0) (quote love) (apply append (quote (() you))))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (append (quote ()) (quote you)))
(apply (lambda _.0 (list (quote I) (apply (lambda _.1 (quote love)) _.0) (apply append (quote (() you))))) (quote ()))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (append (quote ()) (quote you)))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() you)))) _.0)) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (append (quote ()) (quote you)))
(apply (lambda (_.0) (list _.0 (quote love) (apply append (quote (() you))))) (quote (I)))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda (_.1) (apply append _.1)) (quote ((() you)))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda (_.1) (apply append _.1)) (quote ((() you)))))
(apply (lambda (_.0 _.1) (apply append (quote (() (I love you))))) (quote (_.2 _.3)))
((lambda () (apply append (quote (() (I love you))))))
(apply append (list (quote (I)) (quote (love you))))
(list (quote I) (quote love) (apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 (() you)))))
(apply (lambda _.0 (apply append (quote ((I love) (you))))) (quote (_.1)))
(apply (lambda (_.0) (apply append _.0)) (quote (((I love) (you)))))
(apply append (list (quote (I love)) (quote (you))))
(list (quote I) (quote love) (apply (lambda _.0 (append _.0 (quote you))) (quote ())))
(list (quote I) (quote love) (apply (lambda _.0 (apply append (quote (() you)))) (quote (_.1 _.2))))
(apply (lambda _.0 (apply append _.0)) (quote ((I) (love you))))
(apply (lambda (_.0 _.1 _.2) (apply append _.2)) (quote (_.3 _.4 (() (I love you)))))
(apply (lambda (_.0) (apply append (quote ((I) (love you))))) (quote (_.1)))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 ((I) (love you)))))
(apply append (list (quote (I love you)) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda (_.1) (apply append (quote (() you)))) (quote (_.2))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda (_.1) (apply append (quote (() you)))) (quote (_.2))))
(apply (lambda _.0 (list (quote I) (quote love) (apply append _.0))) (quote (() you)))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply append _.1)) (quote (() you))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply append _.1)) (quote (() you))))
((lambda _.0 _.0) (apply (lambda _.1 (quote I)) (quote ())) (quote love) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote (_.1 _.2 _.3 _.4)))
(list (apply (lambda _.0 (quote I)) (quote ())) ((lambda _.1 (quote love))) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote ((I love you) ())))) (quote (_.1)))
(list ((lambda _.0 (quote I)) (quote _.1)) (quote love) (apply append (quote (() you))))
((lambda _.0 _.0) (quote I) (apply (lambda _.1 (quote love)) (quote ())) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (apply (lambda _.3 (quote love)) (quote (_.4))) (apply append (quote (() you))))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() (I love you))))) (quote ()))) (quote ()))
(list ((lambda _.0 (quote I))) (apply (lambda _.1 (quote love)) (quote ())) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply (lambda _.2 (apply append (quote (() you)))) (quote (_.3))))
(list (quote I) (quote love) (apply (lambda _.0 (apply append (quote (() you)))) (quote (_.1 _.2 _.3))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote (_.3 _.4))) (apply append (quote (() you))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply (lambda _.2 (apply append (quote (() you)))) (quote (_.3))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (quote love) (apply (lambda _.3 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply (lambda _.2 (apply append (quote (() you)))) (quote (_.3))))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() (I love you))))) _.0)) (quote (_.2)))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2))) (apply (lambda _.3 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote ())) (apply (lambda _.3 (apply append (quote (() you)))) (quote ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1 _.2))) (apply (lambda _.3 (apply append (quote (() you)))) (quote ())))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) (quote ())) (quote love) (apply append (quote (() you))))) (quote ()))
(apply (lambda (_.0) (apply append _.0)) (quote (((I love you) ()))))
(list (quote I) ((lambda _.0 (quote love)) (quote _.1)) (apply append (quote (() you))))
(apply (lambda (_.0) (append _.0 (quote (I love you)))) (quote (())))
(apply (lambda (_.0) (list (quote I) _.0 (apply append (quote (() you))))) (quote (love)))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) _.0) (quote love) (apply append (quote (() you))))) (quote (_.2)))
(list (quote I) (quote love) (letrec ((_.0 (lambda _.1 _.2))) (apply append (quote (() you)))))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() you)))) (quote ()))) (quote ())))
(apply (lambda (_.0) (apply (lambda _.1 (apply append (quote (() (I love you))))) _.0)) (quote (())))
(apply (lambda _.0 (list (quote I) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) _.0))) (quote ()))
(apply (lambda _.0 (list (quote I) (apply (lambda _.1 (quote love)) (quote ())) (apply append (quote (() you))))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (quote love) (append (quote ()) (quote you)))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2))) (append (quote ()) (quote you)))
(list (quote I) (quote love) (apply (lambda (_.0 _.1) (apply append (quote (() you)))) (quote (_.2 _.3))))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply (lambda _.2 (apply append (quote (() you)))) _.1)) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote ())) (append (quote ()) (quote you)))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply (lambda _.2 (apply append (quote (() you)))) _.1)) (quote ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1 _.2))) (append (quote ()) (quote you)))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() you)))) _.0)) (quote (_.2))))
(list (quote I) (quote love) ((lambda () (apply append (quote (() you))))))
(apply (lambda _.0 (list (quote I) (apply (lambda _.1 (quote love)) _.0) (apply append (quote (() you))))) (quote (_.2)))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) _.0) (apply (lambda _.2 (quote love)) _.0) (apply append (quote (() you))))) (quote ()))
(apply (lambda (_.0) (list _.0 (apply (lambda _.1 (quote love)) (quote ())) (apply append (quote (() you))))) (quote (I)))
(list (quote I) (quote love) (apply (lambda (_.0) (apply (lambda _.1 (apply append (quote (() you)))) _.0)) (quote (()))))
(apply (lambda (_.0 _.1) (list _.1 (quote love) (apply append (quote (() you))))) (quote (_.2 I)))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply (lambda (_.2) (apply append _.2)) (quote ((() you)))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply (lambda (_.2) (apply append _.2)) (quote ((() you)))))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply (lambda (_.2) (apply append _.2)) (quote ((() you)))))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda (_.1) (apply append _.1)) (quote ((() you))))) (quote ())))
(apply (lambda (_.0) (apply append (quote ((I love) (you))))) (quote (_.1)))
(apply (lambda _.0 (apply (lambda (_.1) (apply append _.1)) (quote ((() (I love you)))))) (quote ()))
(apply (lambda _.0 (append (quote ()) (quote (I love you)))) (quote ()))
(apply (lambda (_.0 _.1) (apply append _.0)) (quote ((() (I love you)) _.2)))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda (_.1 _.2) (apply append _.2)) (quote (_.3 (() you)))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda (_.1 _.2) (apply append _.2)) (quote (_.3 (() you)))))
(apply (lambda _.0 (apply append _.0)) (quote ((I love) (you))))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 ((I love) (you)))))
(apply (lambda (_.0 _.1 _.2) (apply append (quote (() (I love you))))) (quote (_.3 _.4 _.5)))
(list (quote I) (quote love) (apply (lambda (_.0 _.1) (apply append _.0)) (quote ((() you) _.2))))
(apply (lambda (_.0) (apply append (quote ((I love you) ())))) (quote (_.1)))
(apply (lambda _.0 (list (quote I) (quote love) (append _.0 (quote you)))) (quote ()))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (append _.1 (quote you))) (quote ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (append _.1 (quote you))) (quote ())))
(apply (lambda _.0 (list (quote I) (quote love) (apply append (quote (() you))))) (quote (_.1 _.2)))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2 _.3))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2 _.3))))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1 _.2)))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1 _.2 _.3)))
(letrec ((_.0 (lambda _.1 _.2))) (apply append (quote ((I) (love you)))))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote ((I) (love you))))) _.0)) (quote ()))
(apply (lambda (_.0 _.1) (apply append (quote ((I) (love you))))) (quote (_.2 _.3)))
((lambda () (apply append (quote ((I) (love you))))))
(list (quote I) (quote love) (apply (lambda (_.0) (append _.0 (quote you))) (quote (()))))
((lambda (_.0) (apply append _.0)) (quote (() (I love you))))
(apply (lambda (_.0 _.1) (list _.1 (quote love) (apply append _.0))) (quote ((() you) I)))
(list (quote I) (quote love) (apply append (list (quote ()) (quote you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply (lambda (_.2) (apply append (quote (() you)))) (quote (_.3))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply (lambda (_.2) (apply append (quote (() you)))) (quote (_.3))))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply (lambda (_.2) (apply append (quote (() you)))) (quote (_.3))))
(car (apply append (quote (() ((I love you) . _.0)))))
(apply (lambda (_.0) (apply append _.0)) (list (quote (() (I love you)))))
(apply (lambda _.0 (apply (lambda _.1 (apply append _.1)) _.0)) (quote (() (I love you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (quote love) (apply (lambda _.2 (apply append _.2)) (quote (() you))))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda (_.1) (apply append (quote (() you)))) (quote (_.2)))) (quote ())))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1))) (apply (lambda _.2 (apply append _.2)) (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote ())) (apply (lambda _.2 (apply append _.2)) (quote (() you))))
((lambda _.0 _.0) (apply (lambda _.1 (quote I)) (quote (_.2))) (quote love) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2 _.3))) (quote love) (apply append (quote (() you))))
(apply (lambda _.0 (apply append _.0)) (quote ((I love you) ())))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) ((lambda _.2 (quote love))) (apply append (quote (() you))))
(apply (lambda _.0 (apply append (quote (() (I love you))))) (quote (_.1 _.2 _.3 _.4 _.5)))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) _.0) (quote love) (apply append _.0))) (quote (() you)))
((lambda _.0 _.0) (quote I) (apply (lambda _.1 (quote love)) (quote (_.2))) (apply append (quote (() you))))
(apply (lambda (_.0) (list (quote I) (quote love) (apply append _.0))) (quote ((() you))))
(list ((lambda _.0 (quote I))) (apply (lambda _.1 (quote love)) (quote (_.2))) (apply append (quote (() you))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1 _.2 _.3))) (apply append (quote (() you))))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() (I love you))))) (quote ()))) (quote (_.2)))
(letrec ((_.0 (lambda () _.1))) (apply append (quote (() (I love you)))))
(list (quote I) (quote love) (apply (lambda _.0 (apply (lambda _.1 (apply append _.1)) _.0)) (quote (() you))))
(apply (lambda (_.0 _.1 _.2) (apply append _.2)) (quote (_.3 _.4 ((I) (love you)))))
((lambda _.0 _.0) (quote I) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote ())))
((lambda _.0 _.0) (apply (lambda _.1 (quote I)) (quote ())) (apply (lambda _.2 (quote love)) (quote ())) (apply append (quote (() you))))
(apply (lambda _.0 (apply (lambda _.1 (apply append (quote (() (I love you))))) (quote (_.2)))) (quote ()))
(list ((lambda _.0 (quote I))) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (quote love) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2 _.3 _.4))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (quote love) (apply (lambda _.3 (apply append (quote (() you)))) (quote (_.4))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote ())) (apply (lambda _.1 (apply append (quote (() you)))) (quote (_.2 _.3 _.4))))
(apply (lambda _.0 (list (quote I) (quote love) (apply append (quote (() you))))) (quote (_.1 _.2 _.3)))
(list (quote I) (quote love) (apply (lambda _.0 (apply append (quote (() you)))) (quote (_.1 _.2 _.3 _.4))))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2))) (apply (lambda _.3 (apply append (quote (() you)))) (quote (_.4))))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (apply (lambda _.3 (quote love)) (quote (_.4 _.5))) (apply append (quote (() you))))
(list (quote I) ((lambda _.0 (quote love))) (apply (lambda _.1 (apply append (quote (() you)))) (quote ())))
(list ((lambda _.0 (quote I)) (quote _.1)) (apply (lambda _.2 (quote love)) (quote ())) (apply append (quote (() you))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote ())) (apply (lambda _.3 (apply append (quote (() you)))) (quote (_.4))))
(list (quote I) (apply (lambda _.0 (quote love)) (quote (_.1 _.2))) (apply (lambda _.3 (apply append (quote (() you)))) (quote (_.4))))
(list (apply (lambda _.0 (quote I)) (quote (_.1))) (apply (lambda _.2 (quote love)) (quote (_.3))) (apply (lambda _.4 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote (_.1 _.2))) (apply (lambda _.3 (quote love)) (quote ())) (apply (lambda _.4 (apply append (quote (() you)))) (quote ())))
(list (apply (lambda _.0 (quote I)) (quote ())) (apply (lambda _.1 (quote love)) (quote (_.2 _.3))) (apply (lambda _.4 (apply append (quote (() you)))) (quote ())))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 ((I love you) ()))))
(apply (lambda _.0 (list (quote I) (apply (lambda _.1 (quote love)) _.0) (apply append _.0))) (quote (() you)))
((lambda () (list (quote I) (quote love) (apply append (quote (() you))))))
(list (apply (lambda _.0 (quote I)) (quote ())) ((lambda _.1 (quote love)) (quote _.2)) (apply append (quote (() you))))
(apply (lambda _.0 (list (apply (lambda _.1 (quote I)) (quote ())) (quote love) (apply append (quote (() you))))) (quote (_.2)))
(apply (lambda (_.0) (apply (lambda _.1 (apply append (quote (() (I love you))))) _.0)) (quote ((_.2))))


;;; interesting answers in which 'append' is relevant
(list (quote I) (quote love) (append (quote ()) (quote you)))
(list (quote I) (quote love) (apply append (quote (() you))))
(apply append (quote ((I love) (you))))
(append (quote (I love)) (quote (you)))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1)))
(apply (lambda (_.0) (apply append _.0)) (quote (((I) (love you)))))
((lambda _.0 _.0) (quote I) (quote love) (apply append (quote (() you))))
(apply append (list (quote (I)) (quote (love you))))
(list (quote I) (quote love) (apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 (() you)))))
(apply (lambda _.0 (apply append (quote ((I love) (you))))) (quote (_.1)))
(apply (lambda (_.0) (apply append _.0)) (quote (((I love) (you)))))
(apply append (list (quote (I love)) (quote (you))))
(apply (lambda _.0 (apply append _.0)) (quote ((I) (love you))))
(apply (lambda (_.0) (apply append (quote ((I) (love you))))) (quote (_.1)))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 ((I) (love you)))))
(apply (lambda (_.0) (apply append (quote ((I love) (you))))) (quote (_.1)))
(apply (lambda _.0 (apply append _.0)) (quote ((I love) (you))))
(apply (lambda (_.0 _.1) (apply append _.1)) (quote (_.2 ((I love) (you)))))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1 _.2)))
(apply (lambda _.0 (apply append (quote ((I) (love you))))) (quote (_.1 _.2 _.3)))
(letrec ((_.0 (lambda _.1 _.2))) (apply append (quote ((I) (love you)))))
(apply (lambda (_.0 _.1) (apply append (quote ((I) (love you))))) (quote (_.2 _.3)))
((lambda () (apply append (quote ((I) (love you))))))
(list (quote I) (quote love) (apply (lambda (_.0) (append _.0 (quote you))) (quote (()))))
(apply (lambda (_.0 _.1 _.2) (apply append _.2)) (quote (_.3 _.4 ((I) (love you)))))


;;; with ' instead of (quote ...)

; okay
(list 'I 'love (append '() 'you))

;(list 'I 'love (apply append '(() you)))

;; nice
(apply append '((I love) (you)))

;; nice
(append '(I love) '(you))

; (apply (lambda _.0 (apply append '((I) (love you)))) '(_.1))

;; nice
(apply
 (lambda (_.0) (apply append _.0))
 '(((I) (love you))))

; ((lambda _.0 _.0) 'I 'love (apply append '(() you)))

;; nice
(apply append (list '(I) '(love you)))

;; maybe
(list
 'I
 'love
 (apply
  (lambda (_.0 _.1) (apply append _.1))
  '(_.2 (() you))))

; (apply (lambda _.0 (apply append '((I love) (you)))) '(_.1))

;; (apply
;;  (lambda (_.0) (apply append _.0))
;;  '(((I love) (you))))

; (apply append (list '(I love) '(you)))

;;(apply (lambda _.0 (apply append _.0)) '((I) (love you)))

;; (apply
;;  (lambda (_.0) (apply append '((I) (love you))))
;;  '(_.1))

;; (apply
;;  (lambda (_.0 _.1) (apply append _.1))
;;  '(_.2 ((I) (love you))))

;; (apply
;;  (lambda (_.0) (apply append '((I love) (you))))
;;  '(_.1))

;; complicated but nice
(apply (lambda _.0 (apply append _.0)) '((I love) (you)))

;; (apply
;;  (lambda (_.0 _.1) (apply append _.1))
;;  '(_.2 ((I love) (you))))

;; (apply
;;  (lambda _.0 (apply append '((I) (love you))))
;;  '(_.1 _.2))

;; (apply
;;  (lambda _.0 (apply append '((I) (love you))))
;;  '(_.1 _.2 _.3))

;; (letrec ([_.0 (lambda _.1 _.2)])
;;   (apply append '((I) (love you))))

;; (apply
;;  (lambda (_.0 _.1) (apply append '((I) (love you))))
;;  '(_.2 _.3))

;; ((lambda () (apply append '((I) (love you)))))

;; fun
(list
 'I
 'love
 (apply (lambda (_.0) (append _.0 'you)) '(())))

;; (apply
;;  (lambda (_.0 _.1 _.2) (apply append _.2))
;;  '(_.3 _.4 ((I) (love you))))




;;; interesting other queries

;;; 'append' is bound to a closure, which is just as good a 'dummy'
;;; value to be ignored as any other!
((lambda _.0 '(I love you)) append append append)


'(I love you) (list 'I 'love 'you)
(list 'I 'love (car (list 'you)))
(letrec ((_.0 (lambda _.1 '(I love you)))) (_.0))

;;; mk discovers one way to implement 'list' in Racket
((lambda _.0 _.0) 'I 'love (car '(you . _.1)))
