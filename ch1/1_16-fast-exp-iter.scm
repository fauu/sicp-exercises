#!/usr/bin/guile -s
!#

(define (square x) (* x x)) 
(define (odd? x)
  (= (remainder x 2) 1))

(define (fast-exp-iter b n a)
  (cond ((= n 0) a)
        ((odd? n) (fast-exp-iter b 
                                 (- n 1) 
                                 (* a b)))
        (else (fast-exp-iter (square b) 
                             (/ n 2) 
                             a))))

(define (fast-exp b n)
  (fast-exp-iter b n 1))

(display (fast-exp 3 3))
(newline)
