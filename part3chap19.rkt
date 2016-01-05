;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3chap19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 265
;(lambda (x y) (x y y)) ; legal if x is a function
;(lambda () 10) ; not legal, no variables
;(lambda (x) x) ; legal, just returns. identity.
;(lambda (x y) x) ; legal, like first
;(lambda x 10) ; not legal, syntax error

; Exercise 266
; 1. 3
; 2. 14
; 3. 13.25

; Exercise 267
(lambda (n) (< n 10))
(lambda (x y) (number->string (* x y)))
(lambda (ir1 ir2) (< (ir-price ir1)
                     (ir-price ir2)))
(lambda (nat) (if (even? nat) 0 1))
(lambda (p im) (place-image (circle 3 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            im))