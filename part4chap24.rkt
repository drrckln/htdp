;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])

; Exercise 331
; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

(make-add 10 -10)
(make-add (make-mul 20 3) 33)
(make-add (make-mul 3.14
                    (make-mul 2 3))
          (make-mul 3.14
                    (make-mul -1 -9)))

(+ -1 2)
(+ (* -2 -3) 33)
(* (+ 1
      (* 2 3))
   (* 3.14 12))

; Exercise 332
; A representation of a BSL-expression (BSL) can evaluate
; - Number
; - (+ BSL BSL)
; - (* BSL BSL)

; Exercise 333
; BSL-expr -> Number
(define (eval-expression bsl)
  (cond
    [(number? bsl) bsl]
    [(add? bsl) (+ (eval-expression (add-left bsl))
                   (eval-expression (add-right bsl)))]
    [(mul? bsl) (* (eval-expression (mul-left bsl))
                   (eval-expression (mul-right bsl)))]))

(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)