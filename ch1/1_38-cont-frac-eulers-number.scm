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

(define (eulers-number k)
  (exact->inexact (+ 2
                     (cont-frac (lambda (i) 1)
                                (lambda (i) (if (= (modulo i 3) 2)
                                           (* (+ 1 (quotient i 3))
                                              2)
                                            1))
                                 k))))

(display (eulers-number 100))

(newline)
