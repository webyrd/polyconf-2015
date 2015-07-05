
;;; careful!  Symbols in arithmetic expression position represent
;;; variables, which by default are bound to zero.  This may result in
;;; surprising behavior!
(test "assign-1"
  (run* (q)
    (->o
      `((:= y unbound-var)
        ())
      q))
  '(((y num ()))))

;; ;;; factorial statement (factorial of x is y) (p.23 of book)
;; (define ! '(seq (:= y 1) (while (not (= x 1)) (seq (:= y (* y x)) (:= x (- x 1))))))

;; ;;; calculate factorial of 3
;; (run* (q) (->o `(,! ((x . 3))) q))


;;; factorial statement (factorial of x is y) (p.23 of book)
;;;
;;; This defn of fact is broken!  Doesn't handle when x is zero
(define !
  `(seq (:= y ,(num 1))
        (while (not (= x ,(num 1)))
          (seq (:= y (* y x))
               (:= x (- x ,(num 1)))))))

#|
in store
((x . 5))

y := 1;
while not(x = 1)
  y := y * x;
  x := x - 1

out store
((y . 120) (x . 1))
|#

(define !-correct
  `(seq (:= y ,(num 1))
        (while (and (not (= x ,(num 0)))
                    (not (= x ,(num 1))))
          (seq (:= y (* y x))
               (:= x (- x ,(num 1)))))))



;;; misc simple tests...

(test "8"
  (run* (q)
    (->o `((seq (:= y (* y x))
                (:= x (- x (num ,(build-num 1)))))
           ((x . (num ,(build-num 3)))))
         q))
  '(((y num ()) (x num (0 1)))))

(test "9"
  (run* (q)
    (->o `((:= x (- x (num ,(build-num 1))))
           ((x . (num ,(build-num 3)))))
         q))
  '(((x num (0 1)))))

(test "10"
  (run* (q)
    (->o `((:= x (num ,(build-num 1)))
           ((x . (num ,(build-num 3)))))
         q))
  '(((x num (1)))))

(test "11"
  (run* (q)
    (->o `((:= x x)
           ((x . (num ,(build-num 3)))))
         q))
  '(((x num (1 1)))))


;;; factorial

(test "!-0"
;;; fail!  Factorial should return 1 for zero 
  (run* (q)
    (->o `(,! ((x . (num ,(build-num 0)))))
         q))
  `())

(test "!-correct-0"
  (run* (q)
    (->o `(,!-correct ((x . (num ,(build-num 0)))))
         q))
  `(((y . ,(num 1))
     (x . ,(num 0)))))


(test "!-1"
  (run* (q)
    (->o `(,! ((x . (num ,(build-num 1)))))
         q))
  `(((y . ,(num 1))
     (x . ,(num 1)))))

(test "!-correct-1"
  (run* (q)
    (->o `(,!-correct ((x . (num ,(build-num 1)))))
         q))
  `(((y . ,(num 1))
     (x . ,(num 1)))))


(test "!-2"
  (run* (q)
    (->o `(,! ((x . (num ,(build-num 2)))))
         q))
  `(((y . ,(num 2))
     (x . ,(num 1)))))

(test "!-correct-2"
  (run* (q)
    (->o `(,!-correct ((x . (num ,(build-num 2)))))
         q))
  `(((y . ,(num 2))
     (x . ,(num 1)))))


(test "!-3"
  (run* (q)
    (->o `(,! ((x . (num ,(build-num 3)))))
         q))
  `(((y . ,(num 6))
     (x . ,(num 1)))))

(test "!-correct-3"
  (run* (q)
    (->o `(,!-correct ((x . (num ,(build-num 3)))))
         q))
  `(((y . ,(num 6))
     (x . ,(num 1)))))


(test "!-4"
  (run* (q)
    (->o `(,! ((x . ,(num 4))))
         q))
  `(((y . ,(num 24))
     (x . ,(num 1)))))

(test "!-correct-4"
  (run* (q)
    (->o `(,!-correct ((x . ,(num 4))))
         q))
  `(((y . ,(num 24))
     (x . ,(num 1)))))


(test "!-5"
  (run* (q)
    (->o `(,! ((x . ,(num 5))))
         q))
  `(((y . ,(num 120))
     (x . ,(num 1)))))

(test "!-correct-5"
  (run* (q)
    (->o `(,!-correct ((x . ,(num 5))))
         q))
  `(((y . ,(num 120))
     (x . ,(num 1)))))


(test "!-3-backwards"
  (run 1 (q)
    (->o `(,! ((x . (num ,q))))
         `((y . ,(num 6)) (x . ,(num 1)))))
  `(,(build-num 3)))

(test "!-correct-3-backwards"
  (run 1 (q)
    (->o `(,!-correct ((x . (num ,q))))
         `((y . ,(num 6)) (x . ,(num 1)))))
  `(,(build-num 3)))



(test "!-4-backwards"
  (run 1 (q)
    (->o `(,! ((x . (num ,q))))
         `((y . ,(num 24)) (x . ,(num 1)))))
  `(,(build-num 4)))

(test "!-correct-4-backwards"
  (run 1 (q)
    (->o `(,!-correct ((x . (num ,q))))
         `((y . ,(num 24)) (x . ,(num 1)))))
  `(,(build-num 4)))



(test "!-5-backwards-a"
  (run 1 (q)
    (->o `(,! ((x . (num ,q))))
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(,(build-num 5)))

(test "!-correct-5-backwards-a"
  (run 1 (q)
    (->o `(,!-correct ((x . (num ,q))))
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(,(build-num 5)))



(test "!-5-backwards-b"
  (run 1 (q)
    (->o `(,! ((x . ,q)))
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(,(num 5)))

(test "!-correct-5-backwards-b"
  (run 1 (q)
    (->o `(,!-correct ((x . ,q)))
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(,(num 5)))



(test "2"
  (run 1 (q)
    (->o `(,! ,q)
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(((y . _.0) (x . ,(num 5)))))

(test "2-correct"
  (run 1 (q)
    (->o `(,!-correct ,q)
         `((y . ,(num 120)) (x . ,(num 1)))))
  `(((y . _.0) (x . ,(num 5)))))



(test "3"
  (run* (q)
    (->o `(,! ((x . ,(num 3))))
         `((y . ,(num 6)) (x . ,(num 1)))))
  '(_.0))

(test "3-correct"
  (run* (q)
    (->o `(,!-correct ((x . ,(num 3))))
         `((y . ,(num 6)) (x . ,(num 1)))))
  '(_.0))



(test "4"
  (run* (q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 3))))
         `((y . ,(num 6))
           (x . ,(num 1)))))
  '(_.0))

(test "5"
  (run 1 (q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* ,q x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 3))))
         `((y . ,(num 6))
           (x . ,(num 1)))))
  '(y))

(test "6"
  (run* (q)
    (->o `((seq (:= y (num ,(build-num 1)))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 5))))
         `((y . (num (0 0 0 1 1 1 1)))
           (x . (num (1))))))
  '(_.0))


(test "infer first :="
  (run 1 (q)
    (absento 0 q)
    (->o `((seq ,q
                (while (not (= ,(num 1) x))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  `((:= y ,(num 1))))

(test "!-correct infer first :="
  (run 1 (q)
    (absento 0 q)
    (->o `((seq ,q
                (while (and (not (= x ,(num 0)))
                            (not (= x ,(num 1))))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  `((:= y ,(num 1))))


(test "infer first :=, no absento"
  (run 1 (q)
    (->o `((seq ,q
                (while (not (= ,(num 1) x))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  `((:= y ,(num 1))))

(test "!-correct infer first :=, no absento"
  (run 1 (q)
    (->o `((seq ,q
                (while (and (not (= x ,(num 0)))
                            (not (= x ,(num 1))))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  `((:= y ,(num 1))))


(test "13b"
  (run 1 (q)
    (absento 0 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,q))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  '((num (1))))

(test "13c"
  (run 1 (q)
    (absento 0 q)
    (->o `((seq (:= y ,(num 1))
                (while (not ,q)
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  '((= (num (1)) x)))

(test "13c-no-absento"
  ;;; mk cheats!!!!
  (run 1 (q)
    (->o `((seq (:= y ,(num 1))
                (while (not ,q)
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  ;; answer should be:
  ;; ((= (num (1)) x))
  '((= (num (0 0 0 1 1)) y)))


(test "13c-no-absento-multiple-calls"
;;; the right way to avoid cheating -- no absento call
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not ,q)
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((= (num (1)) x)))

(test "!-correct 13c-no-absento-multiple-calls-a"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (and (not (= x ,(num 0)))
                            (not ,q))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((= (num (1)) x)))

(test "!-correct 13c-no-absento-multiple-calls-b"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (and (not (= x ,(num 0)))
                            (not ,q))
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))      
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))      
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((= (num (1)) x)))

(test "!-correct 13c-no-absento-multiple-calls-c"
  (run 3 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((not (= (num (1)) x))
    (<= (num (0 1)) x)
    (not (= x (num (1))))))

(test "!-correct 13c-no-absento-multiple-calls-d"
  (run 3 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)      
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))      
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((not (= (num (1)) x))
    (not (= x (num (1))))
    (< (num (1)) x)))

;;; notice the specific ordering of the example calls---this is
;;; critical for reasonable performance.  The last two (essentially,
;;; base cases) can be swapped, but they should come last!  And the
;;; the other calls should be in the order 2, 3, 4.
(test "!-correct 13c-no-absento-multiple-calls-g"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))      
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))      
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))
      ))
  '((<= (num (0 1)) x)))



(test "7-no-absento-multiple-calls-1"
;;; the right way to avoid cheating -- no absento call
;;; and much faster!
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((* y x)))

(test "!-correct-7-no-absento-multiple-calls-1-a"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (and (not (= x ,(num 0)))
                            (not (= x ,(num 1))))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((* y x)))

(test "!-correct-7-no-absento-multiple-calls-1-b"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (and (not (= x ,(num 0)))
                            (not (= x ,(num 1))))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))
            (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))
      ))
  '((* y x)))

(test "!-correct-7-no-absento-multiple-calls-1-b-second-answer"
  (run 2 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (and (not (= x ,(num 0)))
                            (not (= x ,(num 1))))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))
      ))
  '((* y x) (* x y)))


(test "7-no-absento-multiple-calls-1b"
;;; There is a second answer!
;;;
;;; (run 3 is taking a while, although (* y (* x 1)) should be an answer)
  (run 2 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((* y x) (* x y)))

(test "7-no-absento-multiple-calls-1b-more-structured"
;;; Similar to 7-no-absento-multiple-calls-1b,
;;; but with more structure to constrain the search.
;;;  
;;; run 7 seems to take a looooong time
  (run 6 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* y ,q))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '(x
    (+ (num ()) x)
    (* (num (1)) x)
    (* x (num (1)))
    (+ x (num ()))
    (- x (num ()))))


;;; Infer the test for the while loop of factorial
(test "7-no-absento-multiple-calls-4"
  (run 3 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((not (= (num (1)) x))
    (<= (num (0 1)) x)
    (not (= x (num (1))))))

;;; Infer the assignment to y at the beginning of the factorial
;;; definition
(test "7-no-absento-multiple-calls-6"
  (run 1 (q)
    (fresh (prog)
      (== `(seq ,q
             (while (not (= x ,(num 1)))
               (seq (:= y (* y x))
                    (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((:= y (num (1)))))

(test "!-correct-7-no-absento-multiple-calls-6-a"
  (run 1 (q)
    (fresh (prog)
      (== `(seq ,q
             (while (and (not (= x ,(num 0)))
                         (not (= x ,(num 1))))
               (seq (:= y (* y x))
                    (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((:= y (num (1)))))

(test "!-correct-7-no-absento-multiple-calls-6-b"
  (run 1 (q)
    (fresh (prog)
      (== `(seq ,q
             (while (and (not (= x ,(num 0)))
                         (not (= x ,(num 1))))
               (seq (:= y (* y x))
                    (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))))
  '((:= y (num (1)))))


#|

;;; These tests take too long to run


;;; takes rougly a minute to infer missing multiplication -- CLP(FD) might speed things up?
;;
;; 122 collections
;; 62582 ms elapsed cpu time, including 289 ms collecting
;; 62598 ms elapsed real time, including 289 ms collecting
;; 1026212880 bytes allocated
(test "7a"
  (run 1 (q)
    (absento 1 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 5))))
         `((y . ,(num 120))
           (x . ,(num 1)))))
  '((* y x)))


;; 91 collections
;; 38132 ms elapsed cpu time, including 185 ms collecting
;; 38194 ms elapsed real time, including 187 ms collecting
;; 757092896 bytes allocated
(test "7b"
  (run 1 (q)
    (absento 1 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  '((* y x)))

;;; interesting!  factorial of 3 isn't large enough to tell + from *
(test "7c"
  (run 1 (q)
    (absento 1 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y ,q)
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 3))))
         `((y . ,(num 6))
           (x . ,(num 1)))))
  '((+ y x)))



#!eof

;;; too slow (or divergent)

;;; There are infinitely many answers, and first two come back immediately.
;;; Would expect something like (* y (* x 1))
(test "7-no-absento-multiple-calls-1c"
  (run 3 (a b)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* ,a ,b))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((* y x) (* x y) ???))

;;; slow or divergent, alas
(test "7-no-absento-multiple-calls-2"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq ,q
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((:= y (* y x))))

;;; too slow or diverges, alas
(test "7-no-absento-multiple-calls-3"  
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq (:= y (* y x))
                       (:= x ,q))))
          prog)
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((* y x)))


(test "12a"
  (run 1 (q)
    (absento 0 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq ,q
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 5))))
         `((y . ,(num 120))
           (x . ,(num 1)))))
  '???)

(test "12b"
  (run 1 (q)
    (absento 0 q)
    (->o `((seq (:= y ,(num 1))
                (while (not (= x ,(num 1)))
                  (seq ,q
                       (:= x (- x ,(num 1))))))
           ((x . ,(num 4))))
         `((y . ,(num 24))
           (x . ,(num 1)))))
  '???)





(test "!-correct 13c-no-absento-multiple-calls-e"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((<= (num (0 1)) x)))

(test "!-correct 13c-no-absento-multiple-calls-f"
  (run 1 (q)
    (fresh (prog)
      (== `(seq (:= y ,(num 1))
                (while ,q
                  (seq (:= y (* y x))
                       (:= x (- x ,(num 1))))))
          prog)
      (->o `(,prog
             ((x . ,(num 0))))
           `((y . ,(num 1))
             (x . ,(num 0))))      
      (->o `(,prog
             ((x . ,(num 1))))
           `((y . ,(num 1))
             (x . ,(num 1))))      
      (->o `(,prog
             ((x . ,(num 2))))
           `((y . ,(num 2))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 3))))
           `((y . ,(num 6))
             (x . ,(num 1))))
      (->o `(,prog
             ((x . ,(num 4))))
           `((y . ,(num 24))
             (x . ,(num 1))))))
  '((<= (num (0 1)) x)))

|#
