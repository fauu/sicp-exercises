#!/usr/bin/guile -s
!#
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (display "(")
  (display (exact->inexact (x-point p)))
  (display ",")
  (display (exact->inexact (y-point p)))
  (display ")"))

(define (make-segment a b) (cons a b))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (midpoint-segment s)
  (make-point
    (/ (+ (x-point (start-segment s))
          (x-point (end-segment s)))
       2)
    (/ (+ (y-point (start-segment s))
          (y-point (end-segment s)))
       2)))

(define my-segment
  (make-segment
    (make-point 1 1)
    (make-point 2 2)))

(display "Start point: ")
(print-point (start-segment my-segment)) ; (1.0,1.0)
(newline)
(display "End point: ")
(print-point (end-segment my-segment)) ; (2.0,2.0)
(newline)
(display "Mid point: ")
(print-point (midpoint-segment my-segment)) ; (1.5,1.5)
(newline)
