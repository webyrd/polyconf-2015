;(load "mk/mk.scm")
;(load "interp-no-match.scm")
;(load "match-extension.scm")
;(load "mk/test-check.scm")

;; We use the relational Racket interpreter, extended to support 'and'
;; and 'or', to allow us to write a simple proof checker for
;; propositional logic as a Racket function.  Because we can treat the
;; Racket function as a relation, this proof *checker* can act as a
;; theorem prover, finding a proof tree to prove a theorem.

;; The proof checker uses 'and', so we have added both 'and' and 'or'
;; to the relational interpreter.  We can't just add 'and' as a helper
;; function, as we do with 'member?', since 'and' uses short-circuit
;; evaluation.

;; Let's test 'and' and 'or':

;; and tests
(test "and-0"
  (run* (q) (evalo '(and) q))
  '(#t))

(test "and-1"
  (run* (q) (evalo '(and 5) q))
  '(5))

(test "and-2"
  (run* (q) (evalo '(and #f) q))
  '(#f))

(test "and-3"
  (run* (q) (evalo '(and 5 6) q))
  '(6))

(test "and-4"
  (run* (q) (evalo '(and #f 6) q))
  '(#f))

(test "and-5"
  (run* (q) (evalo '(and (null? '()) 6) q))
  '(6))

(test "and-6"
  (run* (q) (evalo '(and (null? '(a b c)) 6) q))
  '(#f))


;; or tests
(test "or-0"
  (run* (q) (evalo '(or) q))
  '(#f))

(test "or-1"
  (run* (q) (evalo '(or 5) q))
  '(5))

(test "or-2"
  (run* (q) (evalo '(or #f) q))
  '(#f))

(test "or-3"
  (run* (q) (evalo '(or 5 6) q))
  '(5))

(test "or-4"
  (run* (q) (evalo '(or #f 6) q))
  '(6))

(test "or-5"
  (run* (q) (evalo '(or (null? '()) 6) q))
  '(#t))

(test "or-6"
  (run* (q) (evalo '(or (null? '(a b c)) 6) q))
  '(6))


;; We now port the proof checker to use the subset of Racket
;; supported by our relational interpreter.
;;
;; minimalist proof checker for propositional logic:

#|
(define (proof? proof)
  (match proof
    ((assumption ,assms () ,A) (member? A assms))
    ((modus-ponens
      ,assms (,(and ant1 ‘(,_ ,assms1 ,_ (if ,A ,B)))
              ,(and ant2 ‘(,_ ,assms2 ,_ ,C))) ,D)
     (and (equal? A C) (equal? B D)
          (equal? assms assms1) (equal? assms assms2)
          (proof? ant1)
          (proof? ant2)))))
|#

#|
;;; Racket code
(define member?
  (lambda (x ls)
    (if (null? ls)
        #f
        (if (equal? (car ls) x)
            #t
            (member? x (cdr ls))))))

(define proof?
  (lambda (proof)
    (match proof
      [`(assumption ,assms () ,A)
       (member? A assms)]
      [`(modus-ponens
         ,assms
         ((,r1 ,assms ,ants1 (if ,A ,B))
          (,r2 ,assms ,ants2 ,A))
         ,B)
       (and (proof? (list r1 assms ants1 (list 'if A B)))
            (proof? (list r2 assms ants2 A)))])))

(proof? '(modus-ponens
            (A (if A B) (if B C))
            ((assumption (A (if A B) (if B C)) () (if B C))
             (modus-ponens
              (A (if A B) (if B C))
              ((assumption (A (if A B) (if B C)) () (if A B))
               (assumption (A (if A B) (if B C)) () A)) B))
            C))
;; => #t
|#

;; Here is our port of the proof checker to our interpreter.  We use
;; 'letrec' instead of 'define', we define 'member?' as a helper
;; function, and use Racket's pattern-matching syntax.  The resulting
;; 'letrec' expression runs without modification in Racket, since the
;; expression does not include any logic variables.

;; We are asking the proof checker to check our proof of C, using the
;; assumptions A, A => B, and B => C.  Note that we give the entire
;; proof tree as the input to 'proof?'.
(test "proof-1"
  (run* (q)
    (evalo
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
     q))
  '(#t))

;; Getting ready to run the proof checker as a theorem prover.  To
;; make sure our query has the right syntactic structure, we unify
;; 'prf' with the answer.  So we are still running the proof checker
;; "forwards," although we are using logic variables, so this code
;; doesn't run directly in Racket.
(test "proof-2a"
  (run* (prf)
    (fresh (rule assms ants)
      (== '(modus-ponens
             (A (if A B) (if B C))
             ((assumption (A (if A B) (if B C)) () (if B C))
              (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
             C)
          prf)
      (evalo
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
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C)))

;; Another test to ensure we are instantiating 'prf' and 'assms' to
;; the correct terms before we try running the proof checker as a
;; theorem prover.  Once again, this test runs forwards.
(test "proof-2b"
  (run* (prf)
    (fresh (rule assms ants)
      (== `(,rule ,assms ,ants C) prf)
      (== `(A (if A B) (if B C)) assms)
      (== '(modus-ponens
             (A (if A B) (if B C))
             ((assumption (A (if A B) (if B C)) () (if B C))
              (modus-ponens
                (A (if A B) (if B C))
                ((assumption (A (if A B) (if B C)) () (if A B))
                 (assumption (A (if A B) (if B C)) () A)) B))
             C)
          prf)
      (evalo
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
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C)))

;; The real test!  We are no longer unifying 'prf' with the answer.
;; The proof checker is now inferring the proof tree for the theorem
;; we are trying to prove (C) given a set of assumptions (A, A => B,
;; and B => C).  The proof checker *function* is now acting as a
;; *relation*, which lets us use it as a theorem prover.

;; run 2 seems to diverge
(test "proof-2c"
  (run 1 (prf)
    (fresh (rule assms ants)
      ;; We want to prove that C holds...
      (== `(,rule ,assms ,ants C) prf)
      ;; ...given the assumptions A, A => B, and B => C.
      (== `(A (if A B) (if B C)) assms)
      (evalo
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
       #t)))
  '((modus-ponens (A (if A B) (if B C))
      ((assumption (A (if A B) (if B C)) () (if B C))
       (modus-ponens (A (if A B) (if B C))
         ((assumption (A (if A B) (if B C)) () (if A B))
          (assumption (A (if A B) (if B C)) () A))
         B))
      C)))

;; Here we run the proof checker/theorem prover with a fresh logic variable
;; representing the proof tree.  This allows us to generate valid
;; proof trees, where each proof tree contains a theorem and the
;; assumptions used to prove that theorem.
;;
;; From the answers it is clear the prover tends to generate "proofs
;; by assumption", assuming the theorem to be proved.  This isn't
;; surprising, since such proofs require relatively little
;; computation.  A couple of the proof trees do use modus ponens,
;; however.
(test "generate-theorems/proofs"
  (run 20 (prf)
    (evalo
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
     #t))
  '(((assumption (_.0 . _.1) () _.0)
     (absento (closure _.0) (closure _.1)
              (prim _.0) (prim _.1)))
    ((assumption (_.0 _.1 . _.2) () _.1) (=/= ((_.0 _.1)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((assumption (_.0 _.1 _.2 . _.3) () _.2)
     (=/= ((_.0 _.2)) ((_.1 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3)))
    ((assumption (_.0 _.1 _.2 _.3 . _.4) () _.3)
     (=/= ((_.0 _.3)) ((_.1 _.3)) ((_.2 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3) (closure _.4)
              (prim _.0) (prim _.1) (prim _.2) (prim _.3) (prim _.4)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 . _.5) () _.4)
     (=/= ((_.0 _.4)) ((_.1 _.4)) ((_.2 _.4)) ((_.3 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 . _.6) () _.5)
     (=/= ((_.0 _.5)) ((_.1 _.5)) ((_.2 _.5)) ((_.3 _.5))
          ((_.4 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 . _.7) () _.6)
     (=/= ((_.0 _.6)) ((_.1 _.6)) ((_.2 _.6)) ((_.3 _.6))
          ((_.4 _.6)) ((_.5 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 . _.8) ()
                 _.7)
     (=/= ((_.0 _.7)) ((_.1 _.7)) ((_.2 _.7)) ((_.3 _.7))
          ((_.4 _.7)) ((_.5 _.7)) ((_.6 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)
              (prim _.6) (prim _.7) (prim _.8)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 . _.9)
                 () _.8)
     (=/= ((_.0 _.8)) ((_.1 _.8)) ((_.2 _.8)) ((_.3 _.8))
          ((_.4 _.8)) ((_.5 _.8)) ((_.6 _.8)) ((_.7 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 . _.10) () _.9)
     (=/= ((_.0 _.9)) ((_.1 _.9)) ((_.2 _.9)) ((_.3 _.9))
          ((_.4 _.9)) ((_.5 _.9)) ((_.6 _.9)) ((_.7 _.9))
          ((_.8 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 . _.11)
      () _.10)
     (=/= ((_.0 _.10)) ((_.1 _.10)) ((_.10 _.2)) ((_.10 _.3))
          ((_.10 _.4)) ((_.10 _.5)) ((_.10 _.6)) ((_.10 _.7))
          ((_.10 _.8)) ((_.10 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
           . _.12)
      () _.11)
     (=/= ((_.0 _.11)) ((_.1 _.11)) ((_.10 _.11))
          ((_.11 _.2)) ((_.11 _.3)) ((_.11 _.4)) ((_.11 _.5))
          ((_.11 _.6)) ((_.11 _.7)) ((_.11 _.8)) ((_.11 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           . _.13)
      () _.12)
     (=/= ((_.0 _.12)) ((_.1 _.12)) ((_.10 _.12))
          ((_.11 _.12)) ((_.12 _.2)) ((_.12 _.3)) ((_.12 _.4))
          ((_.12 _.5)) ((_.12 _.6)) ((_.12 _.7)) ((_.12 _.8))
          ((_.12 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 . _.14)
      () _.13)
     (=/= ((_.0 _.13)) ((_.1 _.13)) ((_.10 _.13))
          ((_.11 _.13)) ((_.12 _.13)) ((_.13 _.2)) ((_.13 _.3))
          ((_.13 _.4)) ((_.13 _.5)) ((_.13 _.6)) ((_.13 _.7))
          ((_.13 _.8)) ((_.13 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.12)
              (prim _.13) (prim _.14) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 . _.15)
      () _.14)
     (=/= ((_.0 _.14)) ((_.1 _.14)) ((_.10 _.14))
          ((_.11 _.14)) ((_.12 _.14)) ((_.13 _.14)) ((_.14 _.2))
          ((_.14 _.3)) ((_.14 _.4)) ((_.14 _.5)) ((_.14 _.6))
          ((_.14 _.7)) ((_.14 _.8)) ((_.14 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.13) (prim _.14)
              (prim _.15) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((modus-ponens ((if _.0 _.1) _.0 . _.2)
                   ((assumption ((if _.0 _.1) _.0 . _.2) () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.0 . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 . _.16)
      () _.15)
     (=/= ((_.0 _.15)) ((_.1 _.15)) ((_.10 _.15))
          ((_.11 _.15)) ((_.12 _.15)) ((_.13 _.15))
          ((_.14 _.15)) ((_.15 _.2)) ((_.15 _.3)) ((_.15 _.4))
          ((_.15 _.5)) ((_.15 _.6)) ((_.15 _.7)) ((_.15 _.8))
          ((_.15 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.14) (prim _.15) (prim _.16) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 _.16 . _.17)
      () _.16)
     (=/= ((_.0 _.16)) ((_.1 _.16)) ((_.10 _.16))
          ((_.11 _.16)) ((_.12 _.16)) ((_.13 _.16))
          ((_.14 _.16)) ((_.15 _.16)) ((_.16 _.2)) ((_.16 _.3))
          ((_.16 _.4)) ((_.16 _.5)) ((_.16 _.6)) ((_.16 _.7))
          ((_.16 _.8)) ((_.16 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.17) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.12)
              (prim _.13) (prim _.14) (prim _.15) (prim _.16)
              (prim _.17) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 _.16 _.17 . _.18)
      () _.17)
     (=/= ((_.0 _.17)) ((_.1 _.17)) ((_.10 _.17))
          ((_.11 _.17)) ((_.12 _.17)) ((_.13 _.17))
          ((_.14 _.17)) ((_.15 _.17)) ((_.16 _.17)) ((_.17 _.2))
          ((_.17 _.3)) ((_.17 _.4)) ((_.17 _.5)) ((_.17 _.6))
          ((_.17 _.7)) ((_.17 _.8)) ((_.17 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.17) (closure _.18) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.13) (prim _.14)
              (prim _.15) (prim _.16) (prim _.17) (prim _.18)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15 _.16 _.17 _.18 . _.19)
      () _.18)
     (=/= ((_.0 _.18)) ((_.1 _.18)) ((_.10 _.18))
          ((_.11 _.18)) ((_.12 _.18)) ((_.13 _.18))
          ((_.14 _.18)) ((_.15 _.18)) ((_.16 _.18))
          ((_.17 _.18)) ((_.18 _.2)) ((_.18 _.3)) ((_.18 _.4))
          ((_.18 _.5)) ((_.18 _.6)) ((_.18 _.7)) ((_.18 _.8))
          ((_.18 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.17) (closure _.18) (closure _.19)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.14) (prim _.15) (prim _.16) (prim _.17)
              (prim _.18) (prim _.19) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))))


;; Since the proof checker/theorem prover tends to generate trivial
;; proof trees that just assume the theorem to be proved, lets
;; restrict the outer proof rule to be modus ponens.
(test "generate-theorems/proofs-using-modus-ponens"
  (run 20 (prf)
    (fresh (assms ants conseq)
      (== `(modus-ponens ,assms ,ants ,conseq) prf)
      (evalo
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
       #t)))
  '(((modus-ponens ((if _.0 _.1) _.0 . _.2)
                   ((assumption ((if _.0 _.1) _.0 . _.2) () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.0 . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((modus-ponens ((if _.0 _.1) _.2 _.0 . _.3)
                   ((assumption ((if _.0 _.1) _.2 _.0 . _.3) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.0 . _.3) () _.0))
                   _.1)
     (=/= ((_.0 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((modus-ponens (_.0 (if _.0 _.1) . _.2)
                   ((assumption (_.0 (if _.0 _.1) . _.2) () (if _.0 _.1))
                    (assumption (_.0 (if _.0 _.1) . _.2) () _.0))
                   _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.0 . _.4)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.0 . _.4) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.0 . _.4) () _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5) ()
                                (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.4 _.0 . _.5) ()
                                _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)))
    ((modus-ponens ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                   ((assumption ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                                () (if _.0 _.1))
                    (assumption ((if _.0 _.1) _.2 _.3 _.4 _.5 _.0 . _.6)
                                () _.0))
                   _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7) ()
        (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.0 . _.7) () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7)))
    ((modus-ponens (_.0 (if _.1 _.2) _.1 . _.3)
                   ((assumption (_.0 (if _.1 _.2) _.1 . _.3) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.1 . _.3) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((modus-ponens (_.0 _.1 (if _.0 _.2) . _.3)
                   ((assumption (_.0 _.1 (if _.0 _.2) . _.3) ()
                                (if _.0 _.2))
                    (assumption (_.0 _.1 (if _.0 _.2) . _.3) () _.0))
                   _.2)
     (=/= ((_.1 (if _.0 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8) ()
        (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.0 . _.8) ()
        _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)
              (prim _.6) (prim _.7) (prim _.8)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0 . _.9)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0 . _.9)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.0
         . _.9)
        () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.1 . _.4)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.1 . _.4) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.1 . _.4) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
       . _.10)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
         . _.10)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.0
         . _.10)
        () _.0))
      _.1)
     (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.0
       . _.11)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.0 . _.11)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.0 . _.11)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4))
          ((_.0 _.5)) ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8))
          ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5) ()
                                (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.4 _.1 . _.5) ()
                                _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3))
          ((_.1 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)))
    ((modus-ponens (_.0 _.1 (if _.1 _.2) . _.3)
                   ((assumption (_.0 _.1 (if _.1 _.2) . _.3) ()
                                (if _.1 _.2))
                    (assumption (_.0 _.1 (if _.1 _.2) . _.3) () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
       _.0 . _.12)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.0 . _.12)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.0 . _.12)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.11)) ((_.0 _.2)) ((_.0 _.3))
          ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6)) ((_.0 _.7))
          ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
       _.12 _.0 . _.13)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.0 . _.13)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.0 . _.13)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.11)) ((_.0 _.12)) ((_.0 _.2))
          ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5)) ((_.0 _.6))
          ((_.0 _.7)) ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7) (prim _.8) (prim _.9)))
    ((modus-ponens (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                   ((assumption (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                                () (if _.1 _.2))
                    (assumption (_.0 (if _.1 _.2) _.3 _.4 _.5 _.1 . _.6)
                                () _.1))
                   _.2)
     (=/= ((_.0 _.1)) ((_.0 (if _.1 _.2))) ((_.1 _.3))
          ((_.1 _.4)) ((_.1 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6)))
    ((modus-ponens
      ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11
       _.12 _.13 _.0 . _.14)
      ((assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.13 _.0 . _.14)
        () (if _.0 _.1))
       (assumption
        ((if _.0 _.1) _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10
         _.11 _.12 _.13 _.0 . _.14)
        () _.0))
      _.1)
     (=/= ((_.0 _.10)) ((_.0 _.11)) ((_.0 _.12)) ((_.0 _.13))
          ((_.0 _.2)) ((_.0 _.3)) ((_.0 _.4)) ((_.0 _.5))
          ((_.0 _.6)) ((_.0 _.7)) ((_.0 _.8)) ((_.0 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.12)
              (prim _.13) (prim _.14) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))))

;; Here we generate *incorrect* proof trees.  That is, proof trees
;; that *do not* prove the theorem from the given set of assumptions.
;; We do this simply by changing the last argument of 'evalo' to
;; #f instead of #t.  In other words, we are inferring proofs for
;; which the 'proof?' function in Racket would return #f.
(test "generate-non-theorems/proofs"
  (run 20 (prf)
    (evalo
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
     #f))
  '(((assumption () () _.0) (absento (closure _.0) (prim _.0)))
    ((assumption (_.0) () _.1) (=/= ((_.0 _.1)))
     (absento (closure _.0) (closure _.1) (prim _.0)
              (prim _.1)))
    ((assumption (_.0 _.1) () _.2)
     (=/= ((_.0 _.2)) ((_.1 _.2)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (prim _.0) (prim _.1) (prim _.2)))
    ((assumption (_.0 _.1 _.2) () _.3)
     (=/= ((_.0 _.3)) ((_.1 _.3)) ((_.2 _.3)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((assumption (_.0 _.1 _.2 _.3) () _.4)
     (=/= ((_.0 _.4)) ((_.1 _.4)) ((_.2 _.4)) ((_.3 _.4)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4)))
    ((assumption (_.0 _.1 _.2 _.3 _.4) () _.5)
     (=/= ((_.0 _.5)) ((_.1 _.5)) ((_.2 _.5)) ((_.3 _.5))
          ((_.4 _.5)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5) () _.6)
     (=/= ((_.0 _.6)) ((_.1 _.6)) ((_.2 _.6)) ((_.3 _.6))
          ((_.4 _.6)) ((_.5 _.6)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6) () _.7)
     (=/= ((_.0 _.7)) ((_.1 _.7)) ((_.2 _.7)) ((_.3 _.7))
          ((_.4 _.7)) ((_.5 _.7)) ((_.6 _.7)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7)))
    ((modus-ponens ()
                   ((assumption () () (if _.0 _.1)) (_.2 () _.3 _.0)) _.1)
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7) () _.8)
     (=/= ((_.0 _.8)) ((_.1 _.8)) ((_.2 _.8)) ((_.3 _.8))
          ((_.4 _.8)) ((_.5 _.8)) ((_.6 _.8)) ((_.7 _.8)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)
              (prim _.6) (prim _.7) (prim _.8)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8) () _.9)
     (=/= ((_.0 _.9)) ((_.1 _.9)) ((_.2 _.9)) ((_.3 _.9))
          ((_.4 _.9)) ((_.5 _.9)) ((_.6 _.9)) ((_.7 _.9))
          ((_.8 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((assumption (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9) ()
                 _.10)
     (=/= ((_.0 _.10)) ((_.1 _.10)) ((_.10 _.2)) ((_.10 _.3))
          ((_.10 _.4)) ((_.10 _.5)) ((_.10 _.6)) ((_.10 _.7))
          ((_.10 _.8)) ((_.10 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10) () _.11)
     (=/= ((_.0 _.11)) ((_.1 _.11)) ((_.10 _.11))
          ((_.11 _.2)) ((_.11 _.3)) ((_.11 _.4)) ((_.11 _.5))
          ((_.11 _.6)) ((_.11 _.7)) ((_.11 _.8)) ((_.11 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.2)
              (prim _.3) (prim _.4) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))
    ((modus-ponens (_.0)
                   ((assumption (_.0) () (if _.1 _.2)) (_.3 (_.0) _.4 _.1))
                   _.2)
     (=/= ((_.0 (if _.1 _.2))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (prim _.0) (prim _.1)
              (prim _.2) (prim _.3) (prim _.4)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11) ()
      _.12)
     (=/= ((_.0 _.12)) ((_.1 _.12)) ((_.10 _.12))
          ((_.11 _.12)) ((_.12 _.2)) ((_.12 _.3)) ((_.12 _.4))
          ((_.12 _.5)) ((_.12 _.6)) ((_.12 _.7)) ((_.12 _.8))
          ((_.12 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12)
      () _.13)
     (=/= ((_.0 _.13)) ((_.1 _.13)) ((_.10 _.13))
          ((_.11 _.13)) ((_.12 _.13)) ((_.13 _.2)) ((_.13 _.3))
          ((_.13 _.4)) ((_.13 _.5)) ((_.13 _.6)) ((_.13 _.7))
          ((_.13 _.8)) ((_.13 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.2) (prim _.3) (prim _.4) (prim _.5) (prim _.6)
              (prim _.7) (prim _.8) (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13)
      () _.14)
     (=/= ((_.0 _.14)) ((_.1 _.14)) ((_.10 _.14))
          ((_.11 _.14)) ((_.12 _.14)) ((_.13 _.14)) ((_.14 _.2))
          ((_.14 _.3)) ((_.14 _.4)) ((_.14 _.5)) ((_.14 _.6))
          ((_.14 _.7)) ((_.14 _.8)) ((_.14 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.2) (closure _.3)
              (closure _.4) (closure _.5) (closure _.6)
              (closure _.7) (closure _.8) (closure _.9) (prim _.0)
              (prim _.1) (prim _.10) (prim _.11) (prim _.12)
              (prim _.13) (prim _.14) (prim _.2) (prim _.3)
              (prim _.4) (prim _.5) (prim _.6) (prim _.7) (prim _.8)
              (prim _.9)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14)
      () _.15)
     (=/= ((_.0 _.15)) ((_.1 _.15)) ((_.10 _.15))
          ((_.11 _.15)) ((_.12 _.15)) ((_.13 _.15))
          ((_.14 _.15)) ((_.15 _.2)) ((_.15 _.3)) ((_.15 _.4))
          ((_.15 _.5)) ((_.15 _.6)) ((_.15 _.7)) ((_.15 _.8))
          ((_.15 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5)
              (closure _.6) (closure _.7) (closure _.8)
              (closure _.9) (prim _.0) (prim _.1) (prim _.10)
              (prim _.11) (prim _.12) (prim _.13) (prim _.14)
              (prim _.15) (prim _.2) (prim _.3) (prim _.4)
              (prim _.5) (prim _.6) (prim _.7) (prim _.8) (prim _.9)))
    ((modus-ponens (_.0 _.1)
                   ((assumption (_.0 _.1) () (if _.2 _.3))
                    (_.4 (_.0 _.1) _.5 _.2))
                   _.3)
     (=/= ((_.0 (if _.2 _.3))) ((_.1 (if _.2 _.3))))
     (absento (closure _.0) (closure _.1) (closure _.2)
              (closure _.3) (closure _.4) (closure _.5) (prim _.0)
              (prim _.1) (prim _.2) (prim _.3) (prim _.4) (prim _.5)))
    ((assumption
      (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 _.9 _.10 _.11 _.12
           _.13 _.14 _.15)
      () _.16)
     (=/= ((_.0 _.16)) ((_.1 _.16)) ((_.10 _.16))
          ((_.11 _.16)) ((_.12 _.16)) ((_.13 _.16))
          ((_.14 _.16)) ((_.15 _.16)) ((_.16 _.2)) ((_.16 _.3))
          ((_.16 _.4)) ((_.16 _.5)) ((_.16 _.6)) ((_.16 _.7))
          ((_.16 _.8)) ((_.16 _.9)))
     (absento (closure _.0) (closure _.1) (closure _.10)
              (closure _.11) (closure _.12) (closure _.13)
              (closure _.14) (closure _.15) (closure _.16)
              (closure _.2) (closure _.3) (closure _.4)
              (closure _.5) (closure _.6) (closure _.7)
              (closure _.8) (closure _.9) (prim _.0) (prim _.1)
              (prim _.10) (prim _.11) (prim _.12) (prim _.13)
              (prim _.14) (prim _.15) (prim _.16) (prim _.2)
              (prim _.3) (prim _.3) (prim _.5) (prim _.6) (prim _.7)
              (prim _.8) (prim _.9)))))
