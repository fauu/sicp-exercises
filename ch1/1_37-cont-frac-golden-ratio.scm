#!/usr/bin/guile -s
!#

(define (cont-frac n d k)
  (define (cont-frac-recur n d k max)
    (if (= k 0)
         0
         (/ (n (+ 1 (- max k))) 
            (+ (d (+ 1 (- max k)))
               (cont-frac-recur n d (- k 1) max)))))
  (cont-frac-recur n d k k))

(define (cont-frac-iter n d k)
  (define (iter k result)
      (if (= k 0)
          result
          (iter (- k 1)
                (/ (n k)
                   (+ (d k)
                      result)))))
  (iter k 0))

(define (golden-ratio n)
  (exact->inexact (/ 1
                     (cont-frac (lambda (i) 1)
                                (lambda (i) 1)
                                n))))

(define (golden-ratio-iter n)
  (exact->inexact (/ 1
                     (cont-frac-iter (lambda (i) 1)
                                     (lambda (i) 1)
                                     n))))
(display (golden-ratio 100))
(newline)
(display (golden-ratio-iter 100))

(newline)
