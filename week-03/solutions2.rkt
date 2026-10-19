#lang racket


;Задача 1. Да се дефинира функция (sum-prime-divisors n), която намира сумата на всички прости делители на едно число n.
(define (mymaxdivisor x)
  (define (helper d)
    (if (= (remainder x d) 0)
        d
        (helper (- d 1))))
  (if (<= x 1)
      (error "x <= 1")
      (helper (- x 1))))

(define (prime? n)
  (and (> n 1) (= 1 (mymaxdivisor n))))

(define (sum-prime-helper cur n)(cond [(= cur 0) 0]
                                      [(and (prime? cur) (= (remainder n cur) 0)) (+ cur (sum-prime-helper (- cur 1) n))]
                                      [else (sum-prime-helper (- cur 1) n)]))

(define (sum-prime-divisors n)
  (sum-prime-helper n n))

;Задача 2. Да се дефинира функция (pow x n), която генерира линейно рекурсивен процес и намира x на степен n, където x е реално, а n - естествено число.

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;Задача 3. Да се дефинира функция (count-оccurences d n), намираща броя на срещанията на дадена цифра d в записа на число n.

(define (cohelper d n)
  (cond [(= n 0) 0]
        [(= (remainder n 10) d) (+ 1 (cohelper  d (quotient n 10)))]
        [else (cohelper d (quotient n 10))]))

(define (count-оccurences d n)
  (cond [(and (= d 0) (= n 0)) 1]
        [else (cohelper d n)]))

;Задача 4. Да се дефинира предикат (ascending? n), който връща истина, ако цифрите на дадено естествено число n са в нарастващ ред от първата към последната.


(define (asc-helper cur lastdigit)
  (cond [(= cur 0) #t]
        [(> (remainder cur 10) lastdigit) #f]
        [else (asc-helper (quotient cur 10) (remainder cur 10))]))

(define (ascending? n)
  (asc-helper n 10))

;Задача 5. Да се дефинира функцията (perfect-number? n), която проверява дали числото n e съвършено, т.е. дали е равно на сбора на делителите си.

(define (sum-divisors cur n)
  (if (<= cur 0)
      0
      (if (= (remainder n cur) 0) (+ cur (sum-divisors (- cur 1) n))
          (sum-divisors (- cur 1) n))))

(define (perfect-number? n)
  (and (> n 0) (= n (sum-divisors (- n 1) n))))


;Задача 6. По зададени x и n, да се дефинира функция (calc-sum x n), която пресмята сумата: 1 + x + x^2 + x^3 + ... + x^n. Използвайте не повече от n на брой умножения.
(define (calc-sum x n)
  (define (calc-sum-helper cur times sum)
    (if (= 0 times) (+ sum cur)
        (calc-sum-helper (* cur x) (- times 1) (+ sum cur))))
  (calc-sum-helper 1 n 0))