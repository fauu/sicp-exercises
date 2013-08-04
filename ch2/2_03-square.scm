#!/usr/bin/guile -s
!#
(define (square n) (* n n))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (display "(")
  (display (exact->inexact (x-point p)))
  (display ",")
  (display (exact->inexact (y-point p)))
  (display ")"))

(define (make-segment a b) (cons a b))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (midpoint-segment s)
  (make-point
    (/ (+ (x-point (start-segment s))
          (x-point (end-segment s)))
       2)
    (/ (+ (y-point (start-segment s))
          (y-point (end-segment s)))
       2)))

(define (segment-length s)
  (sqrt (+ (square (- (x-point (end-segment s))
                      (x-point (start-segment s))))
           (square (- (y-point (end-segment s))
                      (y-point (start-segment s)))))))

; Square as it's diagonal
(define (make-square-diag diagonal)
  (cons
    diagonal
    (lambda (s)
      (car s)))) ; returns square's diagonal

; Square as four points, clockwise 
(define (make-square-points a b c d)
  (cons
    (cons
      (cons a b)
      (cons c d))
    (lambda (s)
      (make-segment
        (car (car (car s))) ; point a
        (car (cdr (car s))))))) ; point c

(define (side-length s)
  (/ (segment-length ((cdr s) s)) ; length of the diagonal
     (sqrt 2)))

(define (square-perimeter s)
  (* 4 (side-length s)))

(define (square-area s) 
  (square (side-length s)))

; ---

(define a (make-square-diag
            (make-segment
              (make-point 2 2)
              (make-point 4 4))))

(define b (make-square-points
            (make-point 2 4)
            (make-point 4 4)
            (make-point 4 2)
            (make-point 2 2)))

(display (square-perimeter a)) ; 8.0
(newline)
(display (square-perimeter b)) ; 8.0
(newline)
(display (square-area b)) ; 4.0
(newline)
(display (square-area b)) ; 4.0
(newline)
