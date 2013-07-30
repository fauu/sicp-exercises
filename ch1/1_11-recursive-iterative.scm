#!/usr/bin/guile -s
!#

(define (my-func-r n)
  (if (< n 3)
      n
      (+ (my-func-r (- n 1))
         (* 2 (my-func-r (- n 2)))
         (* 3 (my-func-r (- n 3))))))

(define (my-func-i n)
  (define (iter a b c count)
    (if (< count 3)
        c
        (iter b 
              c 
              (+ c 
                 (* 2 b) 
                 (* 3 a))
              (- count 1))))
  (iter 0 1 2 n))

(display (my-func-r 6))
(newline)
(display (my-func-i 6))
(newline)
