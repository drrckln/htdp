;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; by the first clause
'()
; by the second clause and the preceding example
(cons "a" '())
; again by the second clause and the preceding example
(cons "b" (cons "a" '()))

; List-of-strings -> Number
; count how many strings alos contains
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else (... (first alos) ... (how-many (rest alos)) ...)])

(check-expect (how-many '()) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "b" (cons "a" '()))) 2)