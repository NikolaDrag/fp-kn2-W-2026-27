#lang racket

(require racket/trace)

(define (prime? n)
  (define (mymaxdivisor x)
    (define (helper-prime d)
      (if (= (remainder x d) 0)
          d
          (helper-prime (- d 1))))
    (trace helper-prime)
    (helper-prime (- x 1)))
  (and (> n 1) (= 1 (mymaxdivisor n))))

(define (sum-prime-divisors n)
  (define (helper d)
    (cond [(> d n) 0]
          [(and (= 0 (remainder n d)) (prime? d))
           (+ d (helper (+ d 1)))]
          [else (helper (+ d 1))]))
  (trace helper)
  (helper 2))
(sum-prime-divisors 21)
; (sum-prime-divisors 9240)
; 9240 = 2 * 2 * 2 * 3 * 5 * 7 * 11


(define (sum-prime-divisors-iter n)
  (define (helper d prev-d sum k)
    (cond [(> d k) sum]
          [(= 0 (remainder k d))
           (helper
            d
            d
            (if (= d prev-d)
                sum
                (+ sum d))
            (quotient k d))]
          [else (helper (+ d 1) prev-d sum k)]))
  (trace helper)
  (helper 2 1 0 n))
(sum-prime-divisors-iter 9240)
; (sum-prime-divisors-iter 21)


(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))
(pow 2 10)


(define (count-occurences d n)
  (cond [(< n 10) (if (= d n) 1 0)]
        [(= d (remainder n 10)) (+ 1 (count-occurences d (quotient n 10)))]
        [else (count-occurences d (quotient n 10))]))
(count-occurences 2 2022)


(define (ascending? n)
  (or (< n 10)
      (and (<= (remainder (quotient n 10) 10) (remainder n 10))
           (ascending? (quotient n 10)))))
(ascending? 1234)
(ascending? 1324)


(define (perfect-number? n)
  (define (sum-divisors n)
    (define (helper d sum)
      (cond [(>= d n) sum]
            [(= 0 (remainder n d)) (helper (+ d 1) (+ d sum))]
            [else (helper (+ d 1) sum)]))
    (helper 1 0))
  (= n (sum-divisors n)))
(perfect-number? 28)
(perfect-number? 8128)
(perfect-number? 8129)

(range 10) ; -> '(0 1 2 3 4 5 6 7 8 9)
(range 5 10) ; -> '(5 6 7 8 9)
(range 1 10 3) ; -> '(1 4 7)
(define (perfect-number-1? n)
  (define (sum-divisors n)
    (apply + (filter (λ (d) (= 0 (remainder n d))) (range 1 n))))
  (= n (sum-divisors n)))

(perfect-number-1? 8128)


(define (calc-sum x n)
  (define (helper i ai sum)
    (if (= i n)
        (+ sum ai)
        (helper (+ i 1) (* ai x) (+ sum ai))))
  (helper 0 1 0))
(calc-sum 2 3)
(calc-sum 5 7)
(+ 1 2 4 8)

(define (calc-sum-1 x n)
  (apply
   +
   (foldl (λ (_ res) (cons (* x (car res))
                           res))
          '(1)
          (range n))))
(calc-sum-1 2 3)
