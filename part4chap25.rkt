;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct element [name content])

; <machine><action /><action /><action /></machine> expanded is:
; <machine><action></action><action></action><action></action></machine>

(make-element "machine" (list (make-element "action" '())))

(define-struct element [name attributes content])
(define-struct attribute [name value])