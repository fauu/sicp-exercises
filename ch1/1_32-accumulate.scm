#!/usr/bin/guile -s
!#

(define (inc n) (+ n 1))

(define (identity n) n)

(define (square n) (* n n))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter 1 null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(display (sum identity 1 inc 4)) ; 10
(newline)
(display (product square 1 inc 4)) ; 576
(newline)
(display (sum-iter identity 1 inc 4)) ; 10
(newline)
(display (product-iter square 1 inc 4)) ; 576
(newline)

(newline)
