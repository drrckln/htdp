;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

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

; FSM -> SimulationState.v1
; match the keys pressed by a player with the given FSM
(define (simulate.v1 fsm0)
  (big-bang initial-state
   [to-draw render-state.v1]
   [on-key find-next-state.v1]))

(define initial-state fsm-traffic)

; A SimulationState.v1 is a FSM-State

; SimulationState.v1 -> Image
; renders a world state as an image
(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 -> SimulationState.v1
; finds the next state from a key stroke ke and current state cs
(define (find-next-state.v1 cs ke)
  cs)

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure:
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image
(define (render-state.v2 s)
  empty-image)

; SimulationState.v2 -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs
;(define (find-next-state.v2 cs ke)
;  cs)

; FSM FSM-State -> SimulationState.v2
; match the keys pressed by a player with the given FSM
(define (simulate.v2 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))

; I assume "red" is a good initial state for traffic lights due to safety reasons
; for the BW machine it doesn't seem to matter.

; SimulationState.v2 -> Image
; renders the current world state as a colored square
(define (state-as-colored-square a-fs)
  (square 100 "solid" (fs-current a-fs)))

(check-expect (state-as-colored-square (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs
(define (find-next-state a-fs ke)
  (make-fs (fs-fsm a-fs)
           (find (fs-fsm a-fs) (fs-current a-fs))))

(check-expect (find-next-state (make-fs fsm-traffic "red") "n")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "red") "a")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "green") "q")
              (make-fs fsm-traffic "yellow"))
(check-expect (find-next-state (make-fs fsm-traffic "yellow") "n")
              (make-fs fsm-traffic "red"))

; Exercise 214
; FSM FSM-State -> FSM-State
; finds the state matching current in the transition table

(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black") "not found: black")
(check-expect (find bw-machine "black") "white")
(check-expect (find bw-machine "white") "black")
(check-error (find bw-machine "yellow") "not found: yellow")

(define (find transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [(string=? (transition-current (first transitions)) current)
     (transition-next (first transitions))]
    [else (find (rest transitions) current)]))

; Exercise 215
(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

(define fsm111
  (list (make-ktransition "A" "b" "B")
        (make-ktransition "A" "c" "C")
        (make-ktransition "B" "b" "B")
        (make-ktransition "B" "c" "C")
        (make-ktransition "C" "b" "B")
        (make-ktransition "C" "c" "C")
        (make-ktransition "B" "d" "D")
        (make-ktransition "C" "d" "D")))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed by a player with the given FSM
(define (simulate.v3 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
            [to-draw state-as-colored-square.v2]
            [on-key find-next-state.v2]))

; SimulationState.v2 -> Image
; renders the current world state as a colored square
(define (state-as-colored-square.v2 a-fs)
  (cond
    [(string=? (fs-current a-fs) "A") (square 100 "solid" "white")]
    [(string=? (fs-current a-fs) "B") (square 100 "solid" "yellow")]
    [(string=? (fs-current a-fs) "C") (square 100 "solid" "yellow")]
    [(string=? (fs-current a-fs) "D") (square 100 "solid" "green")]))

(check-expect (state-as-colored-square.v2 (make-fs fsm111 "B"))
              (square 100 "solid" "yellow"))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from a key stroke ke and current state cs
(define (find-next-state.v2 a-fs ke)
  (make-fs (fs-fsm a-fs)
           (find.v2 (fs-fsm a-fs) (fs-current a-fs) ke)))

; FSM FSM-State -> FSM-State
; finds the state matching current in the transition table
(define (find.v2 transitions current ke)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [(and (string=? (ktransition-current (first transitions)) current)
          (string=? (ktransition-key (first transitions)) ke))
     (ktransition-next (first transitions))]
    [else (find.v2 (rest transitions) current ke)]))
