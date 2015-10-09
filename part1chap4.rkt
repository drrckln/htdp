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

; String -> NorF
; converts teh given string into a number;
; produces #false if impossible

; a NorF is one of:
; - #false
; - a Number

; NorF -> Number
; adds 3 to the given number; 3 otherwise

(define (add3 x)
  (cond
    [(false? x) 3]
    [else (+ x 3)]))

; A LR (short for: launching rocket) is one of:
; - "resting"
; - non-negative number
; intepretation "resting" represents a rocket on the ground
; a number denotes the height of a rocket in flight

; physical constants
(define HEIGHT 300)
(define WIDTH 100)
(define YDELTA 3)
(define XCOORD 10)

; graphical constants
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
(define ROCKET-CENTER (/ (image-height ROCKET) 2))

; A LCRD (short for: launching rocket count down) is one of:
; - "resting"
; - a number in [-3, -1]
; - a non-negative number
; interpretation a rocket resting on the ground, in count-down mode,
; or the number of pixels from the top i.e. its height

; LRCD -> Image
; renders the state as a resting or flying rocket
(define (show x)
  (cond
    [(string? x) (place-rocket HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  XCOORD (* 3/4 WIDTH)
                  (place-rocket HEIGHT))]
    [(>= x 0)
     (place-rocket x)]))

; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed,
; if the rocket is still resting
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA,
; if it is moving already
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x 1) HEIGHT (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

(check-expect
 (show "resting")
 (place-image ROCKET
              10 (- HEIGHT ROCKET-CENTER)
              BACKG))

(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET
                           10 (- HEIGHT ROCKET-CENTER)
                           BACKG)))

(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 ROCKET-CENTER) BACKG))

(check-expect
 (show HEIGHT)
 (place-image ROCKET 10 (- HEIGHT ROCKET-CENTER) BACKG))

(check-expect
 (show 0)
 (place-image ROCKET 10 (- 0 ROCKET-CENTER) BACKG))

; Exercise 56
; (string=? "resting" x) is incorrect because "resting" stands for any KeyEvent,
; and "space" starts the countdown
; a Boolean expression that evaluates to #true when x belongs to the first
; subclass of LRCD would be:
; (or (= 1 (string-length? x))
;     (and (string? x)
;          (elem x ("left", "right", "up, "down"..etc))))
; I guess that last part would be like
; (cond [(string=? x "left") #true] etc..)

; Exercise 57
; LRCD -> Image
; Takes in a height and displays the rocket, bottom centered
(define (place-rocket h)
  (place-image ROCKET XCOORD (- h ROCKET-CENTER) BACKG))

(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
      [to-draw show]
      [on-key launch]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))