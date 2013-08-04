#!/usr/bin/guile -s
!#
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((> (* n d) 0) (cons
                            (/ n g) 
                            (/ d g)))
          (else (cons
                  (* -1 (abs (/ n g)))
                  (abs (/ d g)))))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define a (make-rat 10 4))
(define b (make-rat 1 -5))
(define c (make-rat -3 -6))

(print-rat a) (newline)
(print-rat b) (newline)
(print-rat c) (newline)
