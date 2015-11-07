;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; by the first clause
'()
; by the second clause and the preceding example
(cons "a" '())
; again by the second clause and the preceding example
(cons "b" (cons "a" '()))

; List-of-strings -> Number
; count how many strings alos contains
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else (+ (how-many (rest alos)) 1)]))

(check-expect (how-many '()) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "b" (cons "a" '()))) 2)

; Exercise 138
; this is because they are using the same single data definition

; Exercise 139
; A List-of-amounts is one of:
; - '()  ^----------------------------------|
; - (cons PositiveNumber List-of-amounts) <-|
; interpretation a List-of-amounts represents some amounts of money
'()
(cons 10 '())
(cons 13 (cons 10 '()))

; List-of-amounts -> PositiveNumber
; computes the sum of the amounts in alom
(define (sum alom)
  (cond
    [(empty? alom) 0]
    [else (+ (first alom) (sum (rest alom)))]))

(check-expect (sum '()) 0)
(check-expect (sum (cons 10 '())) 10)
(check-expect (sum (cons 13 (cons 10 '()))) 23)