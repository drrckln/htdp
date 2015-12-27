;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> List-of-numbers
; converts a list of Celsius
; temperatures to Fahrenheit
(define (cf* l)
  (cond
    [(empty? l) '()]
    [else (cons (C2F (first l))
                (cf* (rest l)))]))

(check-expect (cf* (list 100 0 -40))
              (list 212 32 -40))

; Number -> Number
; converts one Celsius
; temperature to Fahrenheit
(define (C2F c)
  (+ (* 9/5 c) 32))

; Inventory -> List-of-strings
; extracts the names of toys
; from an inventory
(define (names i)
  (cond
    [(empty? i) '()]
    [else (cons (IR-name (first i))
                (names (rest i)))]))

(check-expect (names (list (make-IR "doll" 21.0)
                           (make-IR "bear" 13.0)))
              (list "doll" "bear"))

(define-struct IR [name price])
; An IR is (make-IR String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)

#|
(define (cf* l g)
  (cond
    [(empty? l) '()]
    [else (cons (g (first l))
                (cf* (rest l) g))]))

(define (names i g)
  (cond
    [(empty? i) '()]
    [else (cons (g (first i))
                (names (rest i) g))]))
|#

; [List-of Number] [Number -> Number] -> [List-of Number]
; [List-of IR] [IR -> String] -> [List-of String]
(define (map1 k g)
  (cond
    [(empty? k) '()]
    [else (cons (g (first k))
                (map1 (rest k) g))]))

; List-of-numbers -> List-of-numbers
(define (cf*-from-map1 l)
  (map1 l C2F))

(check-expect (cf*-from-map1 (list 100 0 -40))
              (list 212 32 -40))

; Inventory -> List-of-strings
(define (names-from-map1 i)
  (map1 i IR-name))

(check-expect (names-from-map1 (list (make-IR "doll" 21.0)
                                     (make-IR "bear" 13.0)))
              (list "doll" "bear"))

; [List-of Number] -> [List-of Number]
(define (add1-to-each l)
  (map1 l add1))

; Exercise 238
; Number -> [List-of Number]
; tabulates sin between n
; and 0 (inclusive) In a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else (cons (sin n)
                (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n
; and 0 (inclusive) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else (cons (sqrt n)
                (tab-sqrt (sub1 n)))]))

; Number [Number -> Number] -> [List-of Number]
; tabulates f between n
; and 0 (inclusive) in a list
(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else (cons (f n)
                (tabulate (sub1 n) f))]))

; Number -> [List-of Number]
(define (tab-sqr n)
  (tabulate n sqr))

; Number -> [List-of Number]
(define (tab-tan n)
  (tabulate n tan))

; Exercise 239
; [List-of Number] -> Number
; computes the sum of the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else (* (first l) (product (rest l)))]))

; [List-of Number] [Number -> Number] Number -> Number
(define (fold1 l op b)
  (cond
    [(empty? l) b]
    [else (op (first l)
              (fold1 (rest l) op b))]))

(define (fold1-sum l)
  (fold1 l + 0))

(define (fold1-product l)
  (fold1 l * 1))

(check-expect (sum '(1 2 3 4))
              (fold1-sum '(1 2 3 4)))
(check-expect (product '(1 2 3 4 5))
              (fold1-product '(1 2 3 4 5)))