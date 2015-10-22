;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part1chap6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Exercise 94
; Constants
(define MISSILE (triangle 8 "solid" "black"))
(define TANK (rectangle 20 15 "solid" "green"))
(define TANK-HEIGHT (image-height TANK))
(define UFO (ellipse 30 15 125 "blue"))
(define WIDTH 150)
(define HEIGHT 300)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


; initial scene
(place-image TANK
             (/ (image-width BACKGROUND) 2)
             (- (image-height BACKGROUND) (/ (image-height TANK) 2))
             (place-image UFO
                          (/ (image-width BACKGROUND) 2)
                          (+ 0 (image-height UFO))
                          BACKGROUND))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is Posn
; interpretation (make-posn x y) is the UFO's current location

(define-struct tank [loc vel])
; a Tank is (make-tank Number Number)
; interpretation (make-tank x dx) means the tank is at position
; (x, HEIGHT) and that it moves dx pixels per clock tick

; a Missile is Posn
; interpretation (make-posn x y) is the missile's current location

; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; interpretation represents the state of the space invader game

; not fired
(make-aim (make-posn 20 10) (make-tank 28 -3))

; just fired
(make-fired (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT)))

; close enough for collision
(make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103))

; Exercise 95
; instance one uses the first because there is no missile
; the last two uses the second as there is now a missile in play

; Exercise 96
(place-image UFO
             20 10
             (place-image TANK
                          28 (- 200 (/ (image-height TANK) 2))
                          (empty-scene 200 200)))

(place-image UFO
             20 10
             (place-image MISSILE
                          28 (- 200 TANK-HEIGHT)
                          (place-image TANK
                                       28 (- 200 (/ (image-height TANK) 2))
                                       (empty-scene 200 200))))

(place-image MISSILE
             22 103
             (place-image UFO
                          20 100
                          (place-image TANK
                                       100
                                       (- 200 (/ TANK-HEIGHT 2))
                                       (empty-scene 200 200))))