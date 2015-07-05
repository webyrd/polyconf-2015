Vicare Scheme version 0.1d2, 64-bit
Copyright (c) 2006-2010 Abdulaziz Ghuloum and contributors

> (load "poly-while.scm")
> (run 1 (alpha beta gamma)
    (fresh (s)
      (->o
       `(,symbolic-exec-prog-1
         ((a . (num ,alpha))
          (b . (num ,beta))
          (c . (num ,gamma))))
       `(abort ,s))))
(((() ; 0
   (0 0 1) ; 4
   _.0) ; positive int
  (=/= ((_.0 ()))) (absento (abort _.0))))
> 