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
(define (pow b n)
  (fast-exp-iter b n 1))

(define (average a b) (/ (+ a b) 2))

(define (compose f g)
  (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

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
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (exact->inexact (fixed-point-of-transform (lambda (y) (/ x y))
                                            average-damp
                                            1.0)))

; 2log(n) damps seem to be enough
(define (root n x)
  (exact->inexact (fixed-point-of-transform (lambda (y) (/ x (pow y (- n 1))))
                                            (repeated average-damp 
                                                      (round (* 2 (log n))))
                                            1.0)))

(display (root 3 8)) (newline) ; 2
(display (root 30 1073741824)) (newline) ; 2
