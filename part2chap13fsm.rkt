;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is
;   (make-transition FSM-State FSM-State)

; FSM-State is a String that specifies a color.

; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to key strokes

; Exercise 212

; FSM-State FSM-State -> Boolean
; an equality predicate for states
(define (state=? s1 s2)
  (string=? s1 s2))

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; Exercise 213
(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))