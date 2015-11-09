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

; Exercise 140
; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; determines whether all numbers in alon are positive numbers
(define (pos? alon)
  (cond
    [(empty? alon) #true]
    [else (and (> (first alon) 0)
               (pos? (rest alon)))]))

(check-expect (pos? '()) #true)
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons -3 '())) #false)
(check-expect (pos? (cons -3 (cons 5 '()))) #false)
(check-expect (pos? (cons 3 (cons 5 '()))) #true)

; List-of-numbers -> PositiveNumber or Error
; produces sum if alon is also a List-of-amounts, otherwise error
(define (checked-sum alon)
  (cond
    [(empty? alon) 0]
    [else (cond ; could just use if statement here
            [(pos? alon) (sum alon)]
            [else (error "there are nonpositive numbers")])]))

(check-expect (checked-sum '()) 0)
(check-error (checked-sum (cons -4 '())))
(check-expect (checked-sum (cons 4 '())) 4)
(check-expect (checked-sum (cons 5 (cons 4 '()))) 9)
(check-error (checked-sum (cons -5 (cons 4 '()))))

; sum computes, for an element of List-of-numbers, the sum of the numbers..

; Exercise 141
; List-of-boolean is one of:
; - '()
; - (cons Boolean List-of-boolean)

; List-of-boolean -> Boolean
; determines whether alob is all true
(define (all-true alob)
  (cond
    [(empty? alob) #true]
    [else (and (first alob)
               (all-true (rest alob)))]))

(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)

; List-of-boolean -> Boolean
; determines whether alob has single #true value

(define (one-true alob)
  (cond
    [(empty? alob) #false]
    [else (or (first alob)
              (one-true (rest alob)))]))

(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)