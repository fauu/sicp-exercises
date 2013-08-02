#!/usr/bin/guile -s
!#

(define true #t)
(define false #f)

(define (square n) (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (fermat-test-iter n a)
    (cond ((= a (- n 1)) true)
          ((= (expmod a n n) a) (fermat-test-iter n (+ 1 a)))
          (else false)))
  (fermat-test-iter n 1))

(if (fermat-test 1729) (display "1729 passed the test\n")) ; true
(if (fermat-test 6000) (display "6000 passed the test\n")) ; false
(newline)
