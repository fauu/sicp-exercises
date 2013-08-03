#!/usr/bin/guile -s
!#

(define (inc n) (+ 1 n))
(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))

(display ((compose square inc) 6)) ; 3
(newline)

(newline)
