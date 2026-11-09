#lang racket

(require math/number-theory)

(prime? 13)



(define (length lst)
  (if (empty? lst)
      0
      (+ 1 (length (rest lst)))))

(length '(1 2 3))

(length (fourth '(1 2 3 (5))))  ; -> 1
(length (fourth '(1 2 3 '(5)))) ; -> 2
; (fourth '(1 2 3 5))

(define (number->list n)
  (define (helper lst k)
    (if (< k 10)
        (cons k lst)
        (helper (cons (remainder k 10) lst) (quotient k 10))))
  (helper '() n))
(number->list 12345)
(reverse '(1 2 3))

(define (list->number lst)
  (define (helper res lst)
    (if (empty? lst)
        res
        (helper (+ (* res 10) (first lst)) (rest lst))))
  (helper 0 lst))
(list->number '(1 2 3))

(define (list->number-1 lst)
  (foldl (λ (d res)
           (+ (* res 10)
              d))
         0
         lst))
(list->number-1 '(1 2 3))


; eq?, eqv?, equal?

(define (member? v lst)
  (cond [(empty? lst) #f]
        [(equal? v (first lst)) #t]
        [else (member? v (rest lst))]))

(member? 5 '(1 2 3 5))
(member? '(5) '(1 2 3 (5)))
(member? '(5) '(1 2 3 '(5)))

(define (member-1? v lst)
  (and (not (empty? lst))
       (or (equal? v (first lst))
           (member-1? v (rest lst)))))

(member-1? 5 '(1 2 3 5))
(member-1? '(5) '(1 2 3 (5)))
(member-1? '(5) '(1 2 3 '(5)))

(define (insert-at v pos lst)
  (cond [(= pos 0) (cons v lst)]
        [(empty? lst) (cons #f (insert-at v (- pos 1) lst))]
        [else (cons (first lst)
                    (insert-at v (- pos 1) (rest lst)))]))
(insert-at 2 1 '(0 3 5))
(insert-at 2 10 '(0 3 5))


(define (find-min lst)
  (if (empty? (rest lst))
      (first lst)
      (min (first lst) (find-min (rest lst)))))
(find-min '(1 5 2 0 4))

(define (find-min-1 lst)
  (foldl (λ (a b) (if (< a b) a b))
         (first lst)
         (rest lst)))
(find-min-1 '(1 5 2 0 4))

(define (find-min-2 lst)
  (apply min lst))
(find-min-2 '(1 5 2 0 4))


(define (remove-first v lst)
  (cond [(empty? lst) '()]
        [(equal? v (first lst)) (rest lst)]
        [else (cons (first lst) (remove-first v (rest lst)))]))
(remove-first 5 '(1 2 5 6 5 2 5 7))

(define (remove-all v lst)
  (cond [(empty? lst) '()]
        [(equal? v (first lst)) (remove-all v (rest lst))]
        [else (cons (first lst) (remove-all v (rest lst)))]))
(remove-all 5 '(1 2 5 6 5 2 5 7))

(define (remove-all-1 v lst)
  (filter (λ (u) (not (equal? v u)))
          lst))
(remove-all-1 5 '(1 2 5 6 5 2 5 7))


(define (concat lst1 lst2)
  (if (empty? lst1)
      lst2
      (cons (first lst1)
            (concat (rest lst1) lst2))))

(concat '(1 2 3) '(4 5 6))

(define (concat-1 lst1 lst2)
  (foldr cons lst2 lst1))
(concat-1 '(1 2 3) '(4 5 6))


(define (reverse-1 lst)
  (define (helper res lst)
    (if (empty? lst)
        res
        (helper (cons (first lst) res) (rest lst))))
  (helper '() lst))
(reverse-1 '(1 2 3))

(define (reverse-2 lst)
  (foldl cons '() lst))
(reverse-2 '(1 2 3))

(drop '(0 1 2 3 4) 2)
(take '(0 1 2 3 4) 2)

(define (sublist-between start end xs)
  (take (drop xs start)
        (+ (- end start) 1)))
(sublist-between 2 5 '(0 1 2 3 4 5 6 7))


(define (count-occurences subxs xs)
  (length (filter (λ (x) (equal? subxs x)) xs)))
(count-occurences '(1 2) '(1 2 (1 2) 3 4)) ; -> 1
(count-occurences 1 '(1 2 (1 2) 3 1 1 4))  ; -> 3


(define (count-occurences-1 subxs xs)
  (define lsubxs (length subxs))
  (define (helper lxs xs)
    (cond [(> lsubxs lxs) 0]
          [(equal? subxs (take xs lsubxs))
           (+ 1 (helper (- lxs 1) (rest xs)))]
          [else (helper (- lxs 1) (rest xs))]))
  (helper (length xs) xs))
(count-occurences-1 '(1 2) '(1 2 1 2 3 4)) ; -> 2
