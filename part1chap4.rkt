;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part1chap4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;part1chap4.rkt
(require 2htdp/image)
(require 2htdp/universe)

(define (reward s)
  (cond
    [(<= 0 s 10) "bronze"]
    [(and (< 10 s) (<= s 20)) "silver"]
    [else "gold"]))


(define (next traffic-light-state)
  (cond
    [(string=? "red" traffic-light-state) "green"]
    [(string=? "green" traffic-light-state) "yellow"]
    [(string=? "yellow" traffic-light-state) "red"]))

(reward 18)

(define y 210)

(- 200 (cond
         [(> y 200) 0]
         [else y]))

;(define (create-rocket-scene.v5 h)
;  (cond
;    [(<= h ROCKET-CENTER-TO-BOTTOM)
;     (place-image ROCKET 50 h MTSCN)]
;    [(> h ROCKET-CENTER-TO-BOTTOM)
;     (place-image ROCKET 50 ROCKET-CENTER-TO-BOTTOM MTSCN)]))

;(define (create-rocket-scene.v6 h)
;  (place-image ROCKET
;               50
;               (cond
;                 [(<= h ROCKET-CENTER-TO-BOTTOM) h]
;                 [(> h ROCKET-CENTER-TO-BOTTOM) ROCKET-CENTER-TO-BOTTOM])
;               MTSCN))

; A MouseEvt is one of these strings:
; - "button-down"
; - "button-up"
; - "drag"
; - "move"
; - "enter"
; - "leave"

; A TrafficLight shows one of three colors:
; - "red"
; - "green"
; - "yellow"
; interpretation each element of TrafficLight represents which colored
; bulb is currently turned on

; TrafficLight -> TrafficLight
; determines the next state of the traffic light from the given s

(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; LightState is a String
; interpretation the current state of the traffic light

(define (main ls)
  (big-bang ls
    [on-tick tock]
    [to-draw render]))

; LightState -> LightState
; updates the state of the light
(define (tock ls)
  (traffic-light-next ls))

; LightState -> Image
(define (render ls)
  (place-image (circle 10 "solid" ls) 50 50 BACKGROUND))

(define BACKGROUND (empty-scene 100 100))

; A 1String is a string of length 1,
; including
; - "\\" (the backslash),
; - " " (the space bar),
; - "\t" (tab),
; - "\r" (return), and
; - "\b" (backspace).
; interpretation represents a single letter or keyboard character.

