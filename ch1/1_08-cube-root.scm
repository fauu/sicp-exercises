#!/usr/bin/guile -s
!#
(define (my-cube x)
  (define (square x)
    (* x x))
  (define (good-enough? guess prev-guess)
    (< (abs (- guess
               prev-guess))
       0.001))
  (define (improve guess)
    (/ (+ (/ x
             (square guess))
          (* 2 guess))
       3))
  (define (cube-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (cube-iter (improve guess)
                   guess)))
  (cube-iter 1.0 0.0))

(display (my-cube 8))
(display (newline))
