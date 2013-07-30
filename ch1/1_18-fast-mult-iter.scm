#!/usr/bin/guile -s
!#

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-mult a b)
  (cond ((= b 0) 0) 
        ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(display (fast-mult 11 12))
(newline)
