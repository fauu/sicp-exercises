#!/usr/bin/guile -s
!#

(define true #t)
(define false #f)

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
           (let ((squared-expmod (square (expmod base (/ exp 2) m))))
             (if (and (not (= squared-expmod 1))
                      (not (= squared-expmod (- m 1)))
                      (= squared-expmod m) 1)
                 0
                 (remainder (square (expmod base (/ exp 2) m))
                            m))))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(if (fast-prime? 11 5) (display "11 passed the test")) ; true
(if (fast-prime? 6601 1000) (display "6601 passed the test")) ; false, even though passes Fermat's test
(if (fast-prime? 6610 1000) (display "6610 passed the test")) ; false

(newline)
