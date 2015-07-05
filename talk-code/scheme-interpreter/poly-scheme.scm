(load "interp-with-variadic-lambda-and-or-and-match.scm")
(load "mk/test-check.scm")
(load "mk/matche.scm")

(define my-append
  (lambda (l s)
    (cond
      ((null? l) s)
      (else (cons (car l) (my-append (cdr l) s))))))


(define appendo
  (lambda (l s out)
    (conde
      ((== '() l) (== s out))
      ((fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))))))










(letrec ((append (lambda (l s)
                   (if (null? l)
                       s
                       (cons (car l) (append (cdr l) s))))))
  (append '(a b c) '(d e)))


;; Forward

;; We are asking the proof checker to check our proof of C, using the
;; assumptions A, A => B, and B => C.  Note that we give the entire
;; proof tree as the input to 'proof?'.

; A
; A -> B
; ------
; B

(run* (q)
  (eval-expo
   `(letrec ((member? (lambda (x ls)
                        (if (null? ls)
                            #f
                            (if (equal? (car ls) x)
                                #t
                                (member? x (cdr ls)))))))
      (letrec ((proof? (lambda (proof)
                         (match proof
                           [`(assumption ,assms () ,A)
                            (member? A assms)]
                           [`(modus-ponens
                              ,assms
                              ((,r1 ,assms ,ants1 (if ,A ,B))
                               (,r2 ,assms ,ants2 ,A))
                              ,B)
                            (and (proof? (list r1 assms ants1 (list 'if A B)))
                                 (proof? (list r2 assms ants2 A)))]))))
        (proof? '(modus-ponens
                  (A (if A B) (if B C))
                  ((assumption (A (if A B) (if B C)) () (if B C))
                   (modus-ponens
                    (A (if A B) (if B C))
                    ((assumption (A (if A B) (if B C)) () (if A B))
                     (assumption (A (if A B) (if B C)) () A)) B))
                  C))))
   '()
   q))




;; The real test!  We are no longer unifying 'prf' with the answer.
;; The proof checker is now inferring the proof tree for the theorem
;; we are trying to prove (C) given a set of assumptions (A, A => B,
;; and B => C).  The proof checker *function* is now acting as a
;; *relation*, which lets us use it as a theorem prover.

(run 1 (prf)
  (fresh (rule assms ants)
    ;; We want to prove that C holds...
    (== `(,rule ,assms ,ants C) prf)
    ;; ...given the assumptions A, A => B, and B => C.
    (== `(A (if A B) (if B C)) assms)
    (eval-expo
     `(letrec ((member? (lambda (x ls)
                          (if (null? ls)
                              #f
                              (if (equal? (car ls) x)
                                  #t
                                  (member? x (cdr ls)))))))
        (letrec ((proof? (lambda (proof)
                           (match proof
                             [`(assumption ,assms () ,A)
                              (member? A assms)]
                             [`(modus-ponens
                                ,assms
                                ((,r1 ,assms ,ants1 (if ,A ,B))
                                 (,r2 ,assms ,ants2 ,A))
                                ,B)
                              (and (proof? (list r1 assms ants1 (list 'if A B)))
                                   (proof? (list r2 assms ants2 A)))]))))
          (proof? ',prf)))
     '()
     #t)))
