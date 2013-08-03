#!/usr/bin/guile -s
!#

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (let ((next-guess (improve guess)))
      (if (good-enough? guess next-guess)
          next-guess
          ((iterative-improve good-enough? improve) next-guess)))))

(define (fixed-point f first-guess)
  ((iterative-improve 
     (lambda (v1 v2) (< (abs (- v1 v2)) 0.000001)) 
     f) 
   first-guess))

(define (sqrt x)
  (exact->inexact ((iterative-improve (lambda (v1 v2) (< (abs (- v1 v2)) 0.000001))
                                      (average-damp (lambda (y) (/ x y))))
                   1.0)))

(define (sqrt-fixed-point x)
  (exact->inexact
    (fixed-point (average-damp
                   (lambda (y) (/ x y)))
                 1.0)))

(display (sqrt 64)) (newline)
(display (sqrt-fixed-point 64)) (newline)
