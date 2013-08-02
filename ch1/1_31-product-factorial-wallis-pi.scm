#!/usr/bin/guile -s
!#

(define (inc n) (+ n 1))

(define (identity n) n)

(define (square n) (* n n))

(define (product term a next b)
  (if (= a b)
      (term a) 
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (= a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (if (= n 0)
      1
      (product identity 1 inc n)))

(define (wallis-pi n)
  (define (wallis-term x)
    (/ (* (square x) 4) (- (* (square x) 4) 1)))
  (exact->inexact (* 2 (product wallis-term 1 inc n))))

(define (wallis-pi-iter-product n)
  (define (wallis-term x)
    (/ (* (square x) 4) (- (* (square x) 4) 1)))
  (exact->inexact (* 2 (product-iter wallis-term 1 inc n))))

(display (product identity 1 inc 4))
(newline)
(display (factorial 4))
(newline)
(display (wallis-pi 1000))
(newline)
(display (wallis-pi-iter-product 1000))
(newline)

(newline)
