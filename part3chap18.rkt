;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 244

; [X -> Number] [List-of X] -> X
; finds the (first) item in alox that maximizes f, that is:
; if (argmax f (list x-1 ... x-n)) == x-i,
; then (>= (f x-i (f x-1)), (>= (f x-i) (f x-2)), and so on
; Finds the X in the list, that when, f is applied to it,
; results in the largest number (in all of the List of X).

; [X -> Number] [List-of X] -> X
; finds the (first) item in alox that minimizes f, that is:
; blah blah
; Finds the X in the list... smallest number. Basically, it's
; the number where if you map f onto the List-of-X, which X
; gave you the largest numerical value? (if tied, give the 1st)