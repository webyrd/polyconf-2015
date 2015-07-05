(load "mk/mk-vicare.scm")
(load "mk/mk.scm")
(load "mk/test-check.scm")
(load "mk/numbers.scm")

(load "while-abort.scm")

;;; symbolic execution example from slide 7 of Stephen Chong's slides
;;; on symbolic execution (contains contents from Jeff Foster's
;;; slides)
;;;
;;; http://www.seas.harvard.edu/courses/cs252/2011sp/slides/Lec13-SymExec.pdf

;;;  1. int a = α, b = β, c = γ
;;;  2.             // symbolic
;;;  3. int x = 0, y = 0, z = 0;
;;;  4. if (a) {
;;;  5.   x = -2;
;;;  6. }
;;;  7. if (b < 5) {
;;;  8.   if (!a && c)  { y = 1; }
;;;  9.   z = 2;
;;; 10. }
;;; 11. assert(x+y+z!=3)

;;; we will model the 'assert' using 'if' and 'abort'


;;; Slightly modified version that we are actually modelling:

;;;  1. int a = α, b = β, c = γ
;;;  2.             // symbolic
;;;  3. int x = 0, y = 0, z = 0;
;;;  4. if (a) {
;;;  5.   x = 5;
;;;  6. }
;;;  7. if (b <= 4) {
;;;  8.   if (!a && c)  { y = 1; }
;;;  9.   z = 2;
;;; 10. }
;;; 11. assert(x+y+z!=3)

;;;  1. int a := α, b := β, c := γ
;;;  4. if !(a = 0) {
;;;  5.   x := 5;
;;;  6. }
;;;  7. if (b <= 4) {
;;;  8.   if ((a = 0) && !(c = 0))  { y := 1; }
;;;  9.   z := 2;
;;; 10. }
;;; 11. if !(x+(y+z) = 3) {
;;;       abort
;;;     }


(define symbolic-exec-prog-1
  `(seq
     (if (not (= ,(num 0) a))
         (:= x ,(num 5)) ;; lol negative numbers!
         (skip))
     (seq
       (if (<= b ,(num 4)) ;; might want to use numbero to automatically convert numbers to Oleg form
           (seq
             (if (and (= ,(num 0) a)
                      (not
                        (= ,(num 0) c)))
                 (:= y ,(num 1))
                 (skip))
             (:= z ,(num 2)))
           (skip))
       (if (= (+ x (+ y z)) ,(num 3))
           (abort)
           (skip)))))

(run 1 (alpha beta gamma)
  (fresh (s)
    (->o
     `(,symbolic-exec-prog-1
       ((a . (num ,alpha))
        (b . (num ,beta))
        (c . (num ,gamma))))
     `(abort ,s))))
