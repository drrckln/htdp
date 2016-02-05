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

; Exercise 334
; A BooleanBSL is one of:
; - #true
; - #false
; - (make-and BooleanBSL BooleanBSL)
; - (make-or BooleanBSL BooleanBSL)
; - (make-not BooleanBSL)
(define-struct and_ [left right])
(define-struct or_ [left right])
(define-struct not_ [operand])

; BooleanBSL -> Boolean
(define (eval-bool-expression bbsl)
  (cond
    [(boolean? bbsl) bbsl]
    [(and_? bbsl) (and (eval-bool-expression (and_-left bbsl))
                       (eval-bool-expression (and_-right bbsl)))]
    [(or_? bbsl) (or (eval-bool-expression (or_-left bbsl))
                     (eval-bool-expression (or_-right bbsl)))]
    [(not_? bbsl) (not (eval-bool-expression (not_-operand bbsl)))]))

(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression (make-and_ #true #false)) #false)
(check-expect (eval-bool-expression (make-not_ #true)) #false)
(check-expect (eval-bool-expression (make-or_ #true #false)) #true)

; Exercise 335
(require htdp/docs)
(define WRONG "wrong kind of S-expression")
 
; S-expr -> BSL-expr
; creates representation of a BSL expression for s (if possible)
(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
 
          ; SL -> BSL-expr 
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(< L 3)
                 (error WRONG)]
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [else
                 (error WRONG)])))
 
          ; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "strings not allowed")]
              [(symbol? s) (error "symbols not allowed")])))
    (parse s)))

(check-expect (parse 3) 3)
(check-error (parse "hello"))
(check-error (parse 'hey))
(check-error (parse '(3 2)))
(check-error (parse '(3 4 5)))
(check-error (parse '(+ 3 4 6)))
(check-expect (parse '(+ 3 4))
              (make-add 3 4))
(check-expect (parse '(* 3 4))
              (make-mul 3 4))
(check-expect (parse '(+ (* 3 4) (* -2 3)))
              (make-add (make-mul 3 4)
                        (make-mul -2 3)))

; Weird stuff: it doesn't use cond to take advantage of the
; different forms of BSL-expr. it uses error a lot. it uses
; the (< L 3) (= L 3) else expressions, which aren't based
; on the form of the BSL expr so much as the properties of
; the form of BSL-expr.

; Programming languages should be designed for the convenience
; of the programmer who uses it! Otherwise there's no point.

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; Exercise 336
; BSL-var-expr Symbol Number -> BSL-var-expr
; replaces symbol x with value v
(define (subst bslve x v)
  (cond
    [(number? bslve) bslve]
    [(symbol? bslve) (if (symbol=? bslve x) v bslve)]
    [(add? bslve) (make-add (subst (add-left bslve) x v)
                            (subst (add-right bslve) x v))]
    [(mul? bslve) (make-mul (subst (mul-left bslve) x v)
                            (subst (mul-right bslve) x v))]))

(check-expect (subst (make-mul 3 (make-add 'y 7)) 'y 2)
              (make-mul 3 (make-add 2 7)))

(check-expect (subst (make-mul 3 (make-add 'z 7)) 'y 2)
              (make-mul 3 (make-add 'z 7)))

; Exercise 337
; BSL-var-expr -> Boolean
; determines whether it is also a BSL-expr
; eg, no symbols :)
(define (numeric? bslve)
  (cond
    [(number? bslve) #true]
    [(symbol? bslve) #false]
    [(add? bslve) (and (numeric? (add-left bslve))
                       (numeric? (add-right bslve)))]
    [(mul? bslve) (and (numeric? (mul-left bslve))
                       (numeric? (mul-right bslve)))]))

(check-expect (numeric? (make-mul 3 (make-add 'y 7))) #false)
(check-expect (numeric? (make-mul 3 (make-add 3 7))) #true)
    