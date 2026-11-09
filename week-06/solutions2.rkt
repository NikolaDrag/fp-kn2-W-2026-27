#lang racket

;Задача 1. Дефинирайте функция (ordered? xs pred), която проверява дали списък е сортиран възходящо/низходящо според подадена функция за сравнение.

(define (ordered? xs pred) 
  (cond [(empty? xs) #t] ;тук може да рефакторираме и да оставим само условието без #t
        [(= (length xs) 1) #t];тук също, клауза на cond само с условие връща стойността на самото условие, ако тя не е #f; също това е O(n^2), с (empty? (rest xs)) става O(n) цялата функция а този ред - O(1)
        [(pred (first xs) (second xs)) (ordered? (rest xs) pred)]
        [else #f]))

(ordered? '(1 2 3) <)   ; → #t
(ordered? '(3 2 1) >)   ; → #t
(ordered? '(1 1 2) <)   ; → #f
(ordered? '(1 1 2) <=)  ; → #t
;Задача 2. Дефинирайте функция (max-ordered-sublist xs), която връща най-дългия възходящо сортиран подсписък от списъка xs.
(define (max-ordered-sublist xs)
  (define (helper xss curlen maxlen curidx curstart maxstart)
    (cond [(empty? xss) (take (drop xs maxstart) maxlen)] ;null? и empty? се държат еднакво. И двете връщат #t само за '() и #f за всичко останало, включително за неща, които не са списъци, като 5, без да дават грешка.
          [(empty? (rest xss)) (helper '() curlen (max (+ curlen 1) maxlen) (+ 1 curidx) curstart (if (> (+ curlen 1) maxlen) curstart maxstart))]
          [(< (first xss) (second xss)) (helper (rest xss) (+ 1 curlen) maxlen (+ 1 curidx) curstart maxstart)]
          [else (helper (rest xss) 0 (max (+ curlen 1) maxlen) (+ 1 curidx) (+ 1 curidx) (if (> (+ curlen 1) maxlen) curstart maxstart))]))
  (helper xs 0 0 0 0 0))

#| Втори начин (по-удобен): Функция, която връща най-дългия растящ префикс на списък. Много прилича на твоята ordered?.
Функция, която взема този префикс, маха го с drop, повтаря върху остатъка и помни по-дългия.|#
(define (ordered-prefix xs)
  (cond [(empty? xs) '()]
        [(empty? (rest xs)) xs]
        [(< (first xs) (second xs)) (cons (first xs) (ordered-prefix (rest xs)))]
        [else (list (first xs))]))
(ordered-prefix '(1 2 3 1 2)) ; -> '(1 2 3)

(define (max-ordered-sublist-1 xs)
  (define (helper xs best bestlen)
    (define prefix (ordered-prefix xs))
    (define len (length prefix))
    (cond [(empty? xs) best]
          [(> len bestlen) (helper (drop xs len) prefix len)]
          [else (helper (drop xs len) best bestlen)]))
  (helper xs '() 0))

(max-ordered-sublist-1 '(1 5 2 3 4 0)) ; -> '(2 3 4)
(max-ordered-sublist '(1 5 2 3 4 0)) ; -> '(2 3 4)
;Задача 3. Дефинирайте функция (flatten xss), която приема списък от списъци (които също могат да са от списъци, т.е. имаме произволно ниво на вложение) и връща списък само от елементи, т.е. списък без вложени списъци

(define (flatten xss)
  (cond [(empty? xss) '()]
        [(not (list? (first xss))) (append (list (first xss)) (flatten (rest xss)))]
        [else (append (flatten (first xss)) (flatten (rest xss)))]))

(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) ;-> '(1 2 3 4 5 6 7 8 9 10 11 12)

;Задача 4. Дефинирайте функция (assoc key a-list), която търси key в асоциативния списък a-list и връща първия му елемент с ключ равен на key.

(define (assoc key a-list)
  (cond [(empty? a-list) (error "not found")]
        [(equal? (car (car a-list)) key) (car a-list)]
        [else (assoc key (rest a-list))]))


;Задача 5. Дефинирайте функция (replace lst dict), която връща като резултат списък, получен от lst, в който елементите са заменени с асоциацията им в dict.
 
(define (replace lst dict)
  (cond [(empty? lst) '()]
        [else (append (list(cdr (assoc (first lst) dict))) (replace (rest lst) dict))]))

;Задача 6. "Дълбок списък" ще наричаме списък, който може да съдържа други списъци, а "атом" - елемент на дълбок списък, който не е списък
;(примерно число или символ). "Ниво на влагане" на атом в дълбок списък наричаме броя пъти, който трябва да се приложи операцията car за достигане до атома.
;а се реализира функция deep-delete, която в даден дълбок списък (в който всеки атом е число) изтрива всички атоми, които са по-малки от нивото им на влагане.

(define (deep-delete xs)
  (define (helper xs level)
    (cond [(empty? xs) '()]
          [(list? (first xs)) (cons (helper (first xs) (+ 1 level)) (helper (rest xs) level))]
          [(< (first xs) level) (helper (rest xs) level)]
          [else (cons (first xs) (helper (rest xs) level))]))
  (helper xs 1))
;това, което трябва да се сетим тук е - kато стигнем до атом, без значение дали го махнем или не оставаме на същото ниво
(deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) ; -> '(1 (2 (4)) (3 ())))


;Задача 7. Да се дефинира функция (diagonal mat), която получава квадратна числова матрица mat, представена като списък от списъци, и връща списък от елементите на главния диагонал на матрицата.

(define (diagonal mat)
  (define (helper_el_at_idx lst cur idx)
    (if (= cur idx) (first lst) (helper_el_at_idx (rest lst) (+ 1 cur) idx)))
  (define (diagonal-one mat row)
    (cond [(empty? mat) '()]
          [else (cons (helper_el_at_idx (first mat) 1 row) (diagonal-one (rest mat) (+ 1 row)))]))
  (diagonal-one mat 1))
  


(define matrix '((1 2 3 4)
                 (5 6 7 8)
                 (9 10 11 12)
                 (13 14 15 16)))
(diagonal matrix) ; -> '(1 6 11 16)

;Задача 8. Да се дефинира функция от по-висок ред (tabulate f), която връща като резултат функция. Върнатата функция приема като аргументи целите числа a и b и връща
;като резултат списък от точкови двойки от вида '(x . fx), първите елементи на които са поредните точки от интервала [a, b], а вторите – стойностите на f в тези точки.

(define (tabulate f)
  (λ (a b) (define (helper x)
              (if (> x b) '() (cons (cons x (f x)) (helper (+ x 1)))))
            (helper a)))
            
((tabulate sqr) 1 5) ; -> '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25))

