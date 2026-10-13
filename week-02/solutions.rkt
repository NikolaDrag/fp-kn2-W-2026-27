#lang racket

(require racket/trace)

; (quotient 10 3) -> 3 
; 10 % 3 == (remainder 10 3)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(gcd 21 14)
(gcd 14 21)


(define (mymaxdivisor x)
  (define (helper d)
    (if (= (remainder x d) 0)
        d
        (helper (- d 1))))
  (if (<= x 1)
      (error "x <= 1")
      (helper (- x 1))))
; (mymaxdivisor -1)
(mymaxdivisor 21)


(define (sum-odds a b)
  (cond [(> a b)               0]
        [(= 1 (remainder a 2)) (+ a (sum-odds (+ a 1) b))]
        [else                  (sum-odds (+ a 1) b)]))
(trace sum-odds)
(sum-odds 1 5)


(define (prime? n)
  (and (> n 1) (= 1 (mymaxdivisor n))))
(prime? 1)
(prime? 2)
(prime? 3)
(prime? 4)


; 4321 = ((((0 * 10) + 4) * 10) + 3) * 10 + 2) * 10 + 1
(define (reverse-number n)
  (define (helper res k)
    (if (= k 0)
        res
        (helper (+ (* res 10) (remainder k 10)) (quotient k 10))))
  ; (trace helper)
  (helper 0 n))
(reverse-number 1234) ; -> 4321
(reverse-number 120) ; -> 21

(define (palindrome? n)
  (= n (reverse-number n)))
(palindrome? 123) ; -> #f
(palindrome? 12321) ; -> #t

(define (count-palindromes a b)
  (define (helper a count)
    (cond [(> a b)         count]
          [(palindrome? a) (helper (+ a 1) (+ count 1))]
          [else            (helper (+ a 1) count)]))
  ; (trace helper)
  (helper a 0))
(count-palindromes 1 1000)


(define (count-divisors n)
  (define (helper d count)
    (cond [(> d n) count]
          [(= 0 (remainder n d)) (helper (+ d 1) (+ count 1))]
          [else (helper (+ d 1) count)]))
  (trace helper)
  (helper 1 0))
(count-divisors 21)
