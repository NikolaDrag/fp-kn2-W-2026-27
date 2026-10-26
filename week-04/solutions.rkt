#lang racket

(define (substr? a b)
  (define (ends-with? a b)
    (or (and (< a 10)
             (= a (remainder b 10)))
        (and (= (remainder a 10) (remainder b 10))
             (ends-with? (quotient a 10) (quotient b 10)))))
  (and (<= a b)
       (or (ends-with? a b)
           (substr? a (quotient b 10)))))

(substr? 12 123)
(substr? 12 312)
(substr? 123 5123783)
(substr? 1234 5123783)
(substr? 0 51203783)

(define (derive f eps)
  (define (fx x)
    (/ (- (f (+ x eps)) (f x)) eps))
  fx)
; lambda === Ctrl + \ -> λ
(define (derive-1 f eps)
  (lambda (x) (/ (- (f (+ x eps)) (f x)) eps)))
((derive-1 (λ (x) (* x x x)) 0.001) 2) ; -> ~12
((derive-1 (λ (x) (* x x x)) 0.001) 3) ; -> ~27


(define (derive-n f n eps)
  (if (= n 0)
      f
      (derive-n (derive-1 f eps) (- n 1) eps)))
((derive-n (λ (x) (* x x x)) 1 0.001) 3) ; -> ~27
((derive-n (λ (x) (* x x x)) 2 0.001) 3) ; -> ~18
((derive-n (λ (x) (* x x x)) 3 0.001) 3) ; -> ~6
((derive-n (λ (x) (* x x x)) 4 0.001) 3) ; -> ~0


(define (switchsum f g n)
  (define (helper f g i ai sum)
    (define ai+1 (f ai))
    (if (>= i n)
        sum
        (helper g f (+ i 1) ai+1 (+ sum ai+1))))
  (λ (x) (helper f g 0 x 0)))

((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2) ; → 3
((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 2) 2) ; → 9
((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 3) 2) ; → 16
((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 4) 2) ; → 30
