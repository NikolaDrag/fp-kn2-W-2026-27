#lang racket

; Седмица 1 - решения на tasks.md
; Задачи 1–3: без рекурсия. Задача 4: два процеса.

; --- задача 1 ---

(define (mymin a b)
  (if (< a b)
      a
      b))

(mymin 3 5)
(mymin 7 1)

; --- задача 2 ---
; няма рекурсия.
;
; обръщението е (inside? x a b). редът x, a, b е на функцията:
;   x = точката, [a, b] = интервалът
;
; редът на <= е по числовата ос (наляво по-малко), не редът на параметрите.
; ( <= x a b ) би значило x <= a <= b. това НЕ е „x в [a, b]“.
;
; инфикс в C++:          a <= x && x <= b
; префикс с and:         (and (<= a x) (<= x b))
; същото с 3 аргумента:  (<= a x b)
;
; инфикс в Racket е само с точки:  (a . <= . x)  ≡  (<= a x)
; backtick ` в Racket е quasiquote (цитиране на шаблон), не инфикс.
; Haskell има x `elem` xs. тук такова няма.

(define (inside? x a b)
  (and (<= a x)
       (<= x b)))

(inside? 3 1 10)
(inside? 0 1 10)
(inside? 10 1 10)

; --- задача 3 ---

(define (myfunc x y)
  (/ (+ (* x x) (* y y)) 2))

(myfunc 3 4)

; --- задача 4 ---
; редица: 1, 1, 2, 3, 5, ...   индексирана от 0
;
; рекурсивен процес:
; (myfib 4)
; → (+ (myfib 2) (myfib 3))
;
; итеративен процес:
; (myfib-iter 4)
; → (helper 0 0 1)
; → (helper 1 1 1)
; → (helper 2 1 2)
; → (helper 3 2 3)
; → (helper 4 3 5)
; → 5

(define (myfib n)
  (if (<= n 1)
      1
      (+ (myfib (- n 2))
         (myfib (- n 1)))))

(define (myfib-iter n)
  (define (helper i prev cur)
    (if (= i n)
        cur
        (helper (+ i 1)
                cur
                (+ prev cur))))
  (helper 0 0 1))

(myfib 0)
(myfib 1)
(myfib 5)
(myfib-iter 0)
(myfib-iter 1)
(myfib-iter 5)
(myfib-iter 500)
