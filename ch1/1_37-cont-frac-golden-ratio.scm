#!/usr/bin/guile -s
!#

(define (cont-frac n d k)
  (let ((max (- k 1)))
    (if (= k 0)
         0
         (/ (n (- max k)) 
            (+ (d (- max k))
               (cont-frac n d (- k 1)))))))

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
