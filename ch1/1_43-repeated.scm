#!/usr/bin/guile -s
!#

(define (inc n) (+ 1 n))
(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(display ((repeated square 2) 5)) ; 625
(newline)

(newline)
