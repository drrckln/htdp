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