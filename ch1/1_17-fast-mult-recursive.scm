#!/usr/bin/guile -s
!#

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mult-iter a b c)
  (cond ((= b 0) 0) 
        ((= b 1) (+ c a))
        ((even? b) (fast-mult-iter (double a)
                                   (halve b)
                                   c))
        (else (fast-mult-iter a
                              (- b 1)
                              (+ c a)))))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(display (fast-mult 11 12))
(newline)
