;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Exercise 221

; Los -> Boolean
; searches for "atom" in the los
(define (contains-atom? los)
  (contains? "atom" los))

; Los -> Boolean
; searches for "basic" in the los
(define (contains-basic? los)
  (contains? "basic" los))

; Los -> Boolean
; searches for "zoo" in the los
(define (contains-zoo? los)
  (contains? "zoo" los))

; Exercise 222
; Lon -> Lon
; add 1 to each number on l
(define (add1* l)
  (map-add 1 l))

; Lon -> Lon
; adds 5 to each number on l
(define (plus5 l)
  (map-add 5 l))

; Number Lon -> Lon
; adds n to each number on l
(define (map-add n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n)
                (map-add n (rest l)))]))

(check-expect (add1* (list 1 2 3 4))
              (list 2 3 4 5))
(check-expect (add1* '()) '())

(check-expect (plus5 '()) '())
(check-expect (plus5 (list 3 5 6)) '(8 10 11))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

; Exercise 223
; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

; Exercise 224
; R Nelon -> Number
; determins the Rth number on l
(define (most R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (cond
            [(R (first l) (most R (rest l)))
             (first l)]
            [else (most R (rest l))])]))

(define (inf-1 l)
  (most < l))

(define (sup-1 l)
  (most > l))

; The reason it's slow is probably because it needs to calculate (extract R (rest l) t)
; over and over and over again.. once per comparison, so like O(2n)? not sure..

(define (extract-2 R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (R (first l) (extract-2 R (rest l)))]))

(define (inf-2 l)
  (extract-2 max l))

(define (sup-2 l)
  (extract-2 min l))

; Faster because each min only runs once.. so it's like O(n) with the number of list items

; A [List-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])
; parametric data definitions

; A [CP H V] is a structure:
;   (make-point H V)
(define-struct point [hori veri])

; Exercise 225
; A [List X Y] is a structure:
;   (cons X (cons Y '()))

; [List Number Number]
(cons 10 (cons 4 '()))

; [List Number 1String]
(cons 5 (cons "s" '()))

; [List String Boolean]
(cons "hello" (cons #f '()))