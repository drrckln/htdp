;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define-struct element [name content])

; <machine><action /><action /><action /></machine> expanded is:
; <machine><action></action><action></action><action></action></machine>

(make-element "machine" (list (make-element "action" '())))

(define-struct element [name attributes content])
(define-struct attribute [name value])

(make-element "machine" (list (make-attribute 'initial "red")) '())
'(machine (action))

(make-element "machine" (list (make-attribute 'initial "red"))
              (list
               (make-element "action"
                             (list (make-attribute 'state "red")
                                   (make-attribute 'next "green"))
                             '())
               (make-element "action"
                             (list (make-attribute 'state "green")
                                   (make-attribute 'next "yellow"))
                             '())
               (make-element "action"
                             (list (make-attribute 'state "yellow")
                                   (make-attribute 'next "green"))
                             '())))

'(machine ((initial "red"))
          ((action ((state "red") (next "green")))
           (action ((state "green") (next "yellow")))
           (action ((state "yellow") (next "green")))))

; An Xexpr.v0 (short for X-expression) is
; (cons Symbol '())

; An Xexpr.v1 is
; (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is
; (cons Symbol [List-of Xexpr.v2])
; (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; An Attribute is
; (cons Symbol (cons String '())

; Exercise 351
; An Xexpr.v2 is
; (cons Symbol '())
; (cons Symbol (cons Xexpr.v2 Xexpr.v2*))
; (cons Symbol (cons Attribute* Xexpr.v2*))

; Exercise 352

(cons 'transition (list (list 'from "seen-e")
                        (list 'to "seen-f"))
      '())

(cons 'ul (list (list 'li
                      (list (list 'word)
                            (list 'word)))
                (list 'li
                      (list (list 'word)))))

(cons 'end '()) ; THIS ONE