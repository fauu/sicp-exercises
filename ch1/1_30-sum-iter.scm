#!/usr/bin/guile -s
!#

(define (cube n) (* n n n))

(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(display (integral cube 0 1 0.00001)) ; ~0.25
(newline)
