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
#|
(lambda (n) (< n 10))
(lambda (x y) (number->string (* x y)))
(lambda (ir1 ir2) (< (ir-price ir1)
                     (ir-price ir2)))
(lambda (nat) (if (even? nat) 0 1))
(lambda (p im) (place-image (circle 3 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            im))
|#

; Exercise 268
(define (f-plain x) (* 10 x))

(define f-lambda (lambda (x)
                   (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

; Exercise 269
; yes it can

; Exercise 270

(map (lambda (x) (* 10 x))
     '(1 2 3))

(foldl (lambda (name rst) (string-append name ", " rst)) "etc."
       '("Matthew" "Robby"))

(define threshold 100)
(define-struct ir [name price])
(filter (lambda (ir) (<= (ir-price ir) threshold))
        (list (make-ir "bear" 10) (make-ir "doll" 33)))

; Exercise 271
((lambda (x) x) (lambda (x) x))
; so.. you just get the original function
((lambda (x) (x x)) (lambda (x) x))
; identity applied twice
;((lambda (x) (x x)) (lambda (x) (x x)))
; infinite identities

; Exercise 272
; [List-of USD] -> [List-of Euro]
(define (convert-euro lod)
  (map (lambda (dollar) (* 1.22 dollar))
       lod))
; [List-of Fahrenheit] -> [List-of Celsius]
(define (convertFC lof)
  (map (lambda (f) (* 5/9 (- f 32))) lof))

; [List-of Posn] -> [List-of [List Number Number]]
(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p)))
       lop))