#lang racket

(require racket/trace)

(define (ordered? xs pred)
  (or (empty? xs)
      (empty? (rest xs))
      (and (pred (first xs) (second xs))
           (ordered? (rest xs) pred))))

(ordered? '(1 2 3 4) <=) ; -> #t
(ordered? '(4 3 2) >) ; -> #t

;  '(1 2 3)
;  '(2 3 4)
(define (ordered-2? xs pred)
  (andmap pred (drop-right xs 1) (rest xs)))

(ordered-2? '(1 2 3 4 5) >)
(ordered-2? '(1 2 3 4 5) <)
(ordered-2? '(4 3 2) >)


(define (get-prefix xs)
  (define (helper count prefix xs)
    (if (or (empty? xs)
            (< (first xs) (first prefix)))
        (list count (reverse prefix) xs)
        (helper (+ 1 count) (cons (first xs) prefix) (rest xs) )))
  ;(trace helper)
  (if (empty? xs)
      (list 0 '() '())
      (helper 1 (list (first xs)) (rest xs))))

(get-prefix '(1 5 2 4 6 8 3 4 1)) 
(get-prefix '(2 2 4 6 8 3 4 1)) 

(define (max-ordered-sublist xs)
  (define (helper max-sublist xs)
    (define new-prefix (get-prefix xs))
    (cond [(empty? xs) (second max-sublist)]
          [(< (first max-sublist) (first new-prefix))
           (helper new-prefix (third new-prefix))]
          [else (helper max-sublist (third new-prefix))]))
  ;(trace helper)
  (helper (list 0 '()) xs))
            
(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) ; → '(2 4 6 8)
(max-ordered-sublist '(2 2 4 6 8 3 4 1)) ; → '(2 4 6 8)


(define (flatten xss)
  (cond [(empty? xss) '()]
        [(list? (first xss)) (append (flatten (first xss))
                                     (flatten (rest xss)))]
        [else (cons (first xss)
                    (flatten (rest xss)))]))
(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))
; -> '(1 2 3 4 5 6 7 8 9 10 11 12)

(define (flatten-2 xss)
  (if (list? xss)
      (apply append (map flatten-2 xss))
      (list xss)))
; (trace flatten-2)
(flatten-2 '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))
; -> '(1 2 3 4 5 6 7 8 9 10 11 12)


(define (assoc key a-list)
  (cond [(empty? a-list) #f]
        [(equal? key (caar a-list)) (cdar a-list)]
        [else (assoc key (rest a-list))]))
(assoc 2 '((1 . 2) (2 . b)))
(assoc 5 '((1 . 2) (2 . b)))


(define (replace lst dict)
  (map (λ (key) (assoc key dict)) lst))

(replace '(1 2 3 4 5) '((1 . a) (2 . b) (3 . c) (4 . d)))

(define (replace-2 lst dict)
  (define (find key)
    (define value (assoc key dict))
    (if value value key))
  (map find lst))

(replace-2 '(1 2 3 4 5) '((1 . a) (2 . b) (3 . c) (4 . d)))

(define (deep-delete xs)
  (define (helper k xs)
    (cond [(empty? xs) '()]
          [(list? (first xs)) (cons (helper (+ k 1) (first xs))
                                    (helper k (rest xs)))]
          [(<= k (first xs)) (cons (first xs)
                                   (helper k (rest xs)))]
          [else (helper k (rest xs))]))
  (helper 1 xs))
(deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) ; -> (1 (2 (4)) (3 ())))


(define (diagonal mat)
  (if (empty? mat)
      '()
      (cons (caar mat)
            (diagonal (rest (map rest mat))))))

(define matrix '(( 1  2  3  4)
                 ( 5  6  7  8)
                 ( 9 10 11 12)
                 (13 14 15 16)))
(diagonal matrix) ; -> (1 6 11 16)


(define (tabulate f)
  (define (helper a b)
    (if (> a b)
        '()
        (cons (cons a (f a))
              (helper (+ a 1) b))))
  helper)
((tabulate sqr) 1 5) ; -> '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))

(define (tabulate-2 f)
  (λ (a b) (map (λ (x) (cons x (f x)))
                (range a (+ b 1)))))
((tabulate-2 sqr) 1 5) ; -> '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))


(define (where list-elements list-predicates)
  (define (check? x)
    (define (helper ps)
      (or (empty? ps)
          (and ((first ps) x)
               (helper (rest ps)))))
    (helper list-predicates))
  (filter check? list-elements))

(where '(3 4 5 6 7 8 9 10)
       (list even?
             (lambda (x) (> x 5))))
; → '(6 8 10)
(where '(3 4 5 7)
       (list even?
             (lambda (x) (> x 5))))
; → '() 


(define (where-2 list-elements list-predicates)
  (filter (λ (x) (andmap (λ (p?) (p? x))
                         list-predicates))
          list-elements))

(where-2 '(3 4 5 6 7 8 9 10)
         (list even?
               (lambda (x) (> x 5))))
; → '(6 8 10)
(where-2 '(3 4 5 7)
         (list even?
               (lambda (x) (> x 5))))
; → '() 
