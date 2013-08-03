#!/usr/bin/guile -s
!#

(define (average a b) (/ (+ a b) 2))

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "Approximation: ")
      (display (exact->inexact next))
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "WITHOUT average damping:\n")
(display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2))
(newline)
(newline)
(display "WITH average damping:\n")
(display (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))
(newline)

(newline)
