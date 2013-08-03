#!/usr/bin/guile -s
!#

(define (inc n) (+ 1 n))

(define (double f)
  (lambda (x) (f (f x))))

(display ((double inc) 1)) ; 3
(newline)
(display (((double (double double)) inc) 5)) ;21
(newline)

(newline)
