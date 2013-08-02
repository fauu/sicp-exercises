#!/usr/bin/guile -s
!#

(define true #t)
(define false #f)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (inc n) (+ n 1))

(define (identity n) n)

(define (square n) (* n n))

(define (filtered-accumulate combiner null-value term a next b satisfies-condition?)
  (if (> a b)
      null-value
      (combiner (if (satisfies-condition? a)
                    (term a)
                    null-value) 
                (filtered-accumulate combiner 
                                     null-value 
                                     term 
                                     (next a) 
                                     next 
                                     b 
                                     satisfies-condition?))))

(define (sum-squared-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-relative-primes n)
  (define (relative-prime-to-n? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime-to-n?))

(display (sum-squared-primes 1 4)) ; 2^2 + 3^2 = 13 
(newline)
(display (product-relative-primes 10)) ; 1 * 3 * 7 * 9 = 189
(newline)

(newline)
