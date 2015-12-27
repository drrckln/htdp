;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

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

; Exercise 240
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else (place-dot (first l)
                     (image* (rest l)))]))

; Posn Image -> Image
(define (place-dot p img)
  (place-image dot
               (posn-x p) (posn-y p)
               img))

; graphical constants:
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

; [List-of A] [A B -> B] B -> B
(define (fold2 l op b)
  (cond
    [(empty? l) b]
    [else (op (first l)
              (fold2 (rest l) op b))]))

(define (fold2-product l)
  (fold2 l * 1))

(define (fold2-image* l)
  (fold2 l place-dot emt))

(check-expect (image* (list (make-posn 3 4) (make-posn 4 5)))
              (fold2-image* (list (make-posn 3 4) (make-posn 4 5))))

; Exercise 241
; [Number -> Boolean]
(define (5? n) (= n 5))
; [Boolean String -> Boolean]
(define (to-not? bool s)
  (if (string=? s "not")
      (not bool)
      bool))
; [Number Number Number -> Number]
(define (multiply x y z) (* x y z))
; [Number -> [List-of Number]]
(define (list-initialize n)
  (cond
    [(= n 0) '()]
    [else (cons n (list-initialise (sub1 n)))]))
; [[List-of Number] -> Boolean]
(define (sum-greater-than-5? l)
  (> (fold1 l + 0) 5))

; Exercise 242
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; [List-of String] [String String -> Boolean] -> [List-of String]
; Abstracted
; [List-of X] [X X -> Boolean] -> [List-of X]
; Instantiation using IR
; [List-of IR] [IR IR -> Boolean] -> [List-of IR]
