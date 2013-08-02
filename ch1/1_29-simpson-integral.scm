#!/usr/bin/guile -s
!#

(define (cube n) (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (even? n)
  (= (remainder n 2) 0))

(define (get-factor counter n)
  (cond ((or (= counter 1) (= counter n)) 1)
        ((even? counter) 4)
        (else 2)))

(define (simpson-sum-iter f a b n counter sum)
  (if (= counter 0)
      sum
      (simpson-sum-iter f 
                        a 
                        b 
                        n
                        (- counter 1)
                        (+ sum (* (get-factor counter n) 
                                  (f (+ a (* counter (/ (- b a) n)))))))))

(define (simpson-integral f a b n)
  (* (/ (/ (- b a) n) 3)
     (simpson-sum-iter f a b n n 0)))

(display "simpson-integral, n=100: ")
(display (exact->inexact (simpson-integral cube 0 1 100)))
(newline)
(display "simpson-integral, n=1000: ")
(display (exact->inexact (simpson-integral cube 0 1 1000)))
(newline)
(display "integral, dx=0.001: ")
(display (integral cube 0 1 0.001))
(newline)

(newline)
