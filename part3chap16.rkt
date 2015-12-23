;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Exercise 221

; Los -> Boolean
; searches for "atom" in the los
(define (contains-atom? los)
  (contains? "atom" los))

; Los -> Boolean
; searches for "basic" in the los
(define (contains-basic? los)
  (contains? "basic" los))

; Los -> Boolean
; searches for "zoo" in the los
(define (contains-zoo? los)
  (contains? "zoo" los))