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
(define (fast-exp b n)
  (fast-exp-iter b n 1))

(define pi 3.14159)

(define (cont-frac n d k)
  (define (cont-frac-recur n d k max)
    (if (= k 0)
         0
         (/ (n (+ 1 (- max k))) 
            (+ (d (+ 1 (- max k)))
               (cont-frac-recur n d (- k 1) max)))))
  (cont-frac-recur n d k k))

(define (tan-cf x k)
  (* -1
     (cont-frac (lambda (i) (* -1 (fast-exp x i))) 
                (lambda (i) (- (* i 2) 1)) 
                k)))

(display (tan-cf (* -1 (/ pi 6)) 100)) ; ~ -0.57735026919

(newline)
