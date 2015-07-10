;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part1chap3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
; data definitions
; Temperature is a Number
; interpretation degrees Celsius

; We use plain numbers to represent temperatures.

; String -> Number
; (define (f a-string) 0)
; Temperature -> String
; (define (g n) "a")
; Number String Image -> Image
; (define (h num str img) (empty-scene 100 100))

; Number String Image -> Image
; (define (add-image y s img)
;   (empty-scene 100 100))

; Number -> Number
; computes the area of a square whose side is len
; given: 2, expect: 4
; given: 7, expect: 49
; (define (area-of-square len) 0)

; template
; (define (area-of-square len)
;   (... len ...))

; time to code
; Number -> Number
; computes the area of a square whose side is len
; given: 2, expect: 4
; given: 7, expect: 49
(define (area-of-square len)
  (sqr len))

; Number String Image -> Image
; add s to img, y pixels from the top, 10 pixels to the left
; given:
; 5 for y,
; "hello" for s, and
; (empty-scene 100 100) for img
; expected:
; (place-image (text "hello" 10 "red") 10 5 (empty-scene 100 100))
(define (add-image y s img)
  (place-image (text s 10 "red") 10 y img))

; String -> 1-String
; Take a nonempty string and return the first character (as a string)
; given: "hello", expect: "h"
; given: "cat", expect: "c"
; given: " oog", expect: " "
(define (string-first str)
  (substring str 0 1))

; Image -> Number
; Take an image and return the number of pixels in it
; given: (rectangle 20 33 "solid" "red"), expect: 660
; given: (circle 5 "solid" "blue"), expect: 100
; given: (square 13 "outline" "purple"), expect: 169
(define (image-area img)
  (* (image-height img) (image-width img)))

; String -> String
; Takes a string and returns the same string without the last character
; given: "oog", expect: "oo"
; given: "falafel", expect: "falafe"
; given: "Mew2King", expect: "Mew2Kin"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

; Number -> Number 
; converts Fahrenheit temperatures to Celsius temperatures

(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)

(define (f2c f)
  (* 5/9 (- f 32)))

(define WIDTH-OF-WORLD 200)

; Single Point of Control :D
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE WHEEL-RADIUS 0 "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))
(define CAR (underlay/offset (rectangle (* 2 WHEEL-DISTANCE) (* 2 WHEEL-RADIUS) "solid" "red")
                             0
                             WHEEL-RADIUS
                             BOTH-WHEELS))
(define tree
  (underlay/xy (circle 10 'solid 'green)
               9 15
               (rectangle 2 20 'solid 'brown)))
(define BACKGROUND (place-image tree 150 30 (empty-scene 500 40)))
(define Y-CAR (- (image-height BACKGROUND) (/ (image-height CAR) 2)))


; WorldState is a Number
; interpretation the number of pixels between the left border and the car's right border

; render
; clock-tick-handler
; key-stroke-handler
; mouse-event-handler
; end?

; WorldState -> Image
; places the image of the car x pixels from the left margin of
; the BACKGROUND image
; (define (render x)
;   BACKGROUND)

; WorldState -> WorldState
; adds 3 to x to move the car right
; moves the car by 3 pixels every time the clock ticks
; given: 20, expect: 23
; given: 78, expect: 81
;(check-expect (tock 20) 23)
;(check-expect (tock 78) 81)
;(define (tock ws)
;  (+ ws 3))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [stop-when ender]))

; WorldState -> Image
; places the car onto a scene according to the given world state
;(define (render ws)
;  (place-image CAR ws Y-CAR BACKGROUND))

; WorldState -> Boolean
; determines when to end the program, at that point returning 't
(define (ender ws)
  (cond
    [(>= (- (* ws 3) (/ (image-width CAR) 2)) (image-width BACKGROUND)) #t]
    [else #f]))

; AnimationState is a Number
; interpretation the number of clock ticks since the animation started

; AnimationState -> AnimationState
; determines number of clock ticks since beginning
(define (tock ws)
  (add1 ws))

; AnimationState -> Image
; places the car onto a scene dependent on number of clock ticks so far
(define (render ws)
  (place-image CAR (* 3 ws) (+ Y-CAR (sin ws)) BACKGROUND))

