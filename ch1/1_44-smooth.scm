#!/usr/bin/guile -s
!#

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (avg-3 a b c) 
  (/ (+ a b c)
     3))

(define dx 0.00001)
(define (smooth f)
  (lambda (x) (avg-3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (my-func x)
  (+ (* x x x x x) 5))

(display (my-func 5))
(newline)
(display ((smooth my-func) 5))
(newline)
(display ((smooth (smooth my-func)) 5))
(newline)
(display ((n-fold-smooth my-func 2) 5))
(newline)

(newline)
