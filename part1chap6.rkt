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
(define UFO-SPEED 5)
(define MISSILE-SPEED (* -2 UFO-SPEED))
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

; Exercise 97
; Location is one of:
; - Posn
; - Number
; interpretation Posn are positions on the Cartesian plane,
; Numbers are positions on either the x- or the y-axis.

(define R 10)
; Location -> Boolean
; (in-reach? l) determines whether l's distance is less than R
(define (in-reach? l)
  (cond
    [(posn? l) (cond
                 [(< (dist l) R) #true]
                 [else #false])]
    [(number? l) (cond
                   [(< (abs l) R) #true]
                   [else #false])]))

; Posn -> Number
; determines the distance of a Posn to origin
(define (dist p)
  (sqrt (+ (sqr (posn-x p))
           (sqr (posn-y p)))))


(check-expect (in-reach? (make-posn 3 4)) #true)
(check-expect (in-reach? (make-posn 10 1)) #false)
(check-expect (in-reach? (make-posn -3 4)) #true)
(check-expect (in-reach? 10) #false)
(check-expect (in-reach? -11) #false)
(check-expect (in-reach? -4) #true)

; SIGS -> Image
; adds TANK, UFO, and possibly the MISSILE to the BACKGROUND
(define (si-render s)
  (cond
    [(aim? s) (tank-render (aim-tank s) (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s) (tank-render (fired-tank s)
                             (ufo-render (fired-ufo s)
                                         (missile-render (fired-missile s) BACKGROUND)))]))

; Exercise 98
; they are the same result when the Tank and the UFO don't overlap

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (tank-loc t) (- HEIGHT (/ TANK-HEIGHT 2))
               im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u) (posn-y u)
               im))

; Missile Image -> Image
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE
               (posn-x m) (posn-y m)
               im))

; Exercise 99
; SIGS -> Boolean
; #true when UFO lands or missile hits UFO
(define (si-game-over? sigs)
  (cond
    [(and (aim? sigs)
          (landed? (aim-ufo sigs)))
          #true]
    [(and (fired? sigs)
          (hit? (fired-ufo sigs) (fired-missile sigs)))
     #true]
    [else #false]))

; UFO -> Boolean
; determines whether the UFO has landed
(define (landed? u)
  (>= (posn-y u)
      (- HEIGHT (/ (image-height UFO) 2))))

; UFO Missile -> Boolean
; determines wheter the missile is close enough to the UFO to hit
(define (hit? u m)
  (< (near u m) 5))

; Posn Posn -> Number
; determines the distance between two Posns
(define (near a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

; SIGS -> Image
; places GAME OVER on the final image
(define (si-render-final sigs)
  (overlay/align "middle" "middle"
                 (text "GAME OVER" 25 "solid" "indigo")
                 (si-render sigs)))

; Exercise 100
; SIGS Number -> SIGS
; move all objects according to their velocity
(define (si-move-proper s r)
  (cond
    [(aim? s) (make-aim (move-ufo (aim-ufo s) r)
                        (move-tank (aim-tank s)))]
    [(fired? s) (make-fired (move-ufo (fired-ufo s) r)
                            (move-tank (fired-tank s))
                            (move-missile (fired-missile s)))]))

; UFO Number-> UFO
; moves the UFO down and to the x position horizontal
(define (move-ufo u x)
  (make-posn (modulo (+ x (posn-x u)) WIDTH)
             (+ (posn-y u) UFO-SPEED)))

; Tank -> Tank
; moves the tank x coord, leaves vel the same
(define (move-tank t)
  (make-tank (+ (tank-loc t) (tank-vel t))
             (tank-vel t)))

; Missile -> Missile
; moves the Missile upward, leaving x coord the same
(define (move-missile m)
  (make-posn (posn-x m)
             (+ (posn-y m) MISSILE-SPEED)))

; SIGS -> SIGS
; move all objects according to their velocities
(define (si-move w)
  (si-move-proper w (create-random-number w)))

; SIGS -> Number
; create a random number in case a UFO should perform a horizontal jump
(check-random (create-random-number 0)
              (cond
                [(>= (random 2) 1) (random WIDTH)]
                [else (* -1 (random WIDTH)))
(define (create-random-number w)
  (cond
    [(>= (random 2) 1) (random WIDTH)]
    [else (* -1 (random WIDTH))]))

; testing random function!
(check-expect (si-move-proper (make-aim (make-posn 20 10) (make-tank 28 -3)) 10)
              (make-aim (make-posn 10 15) (make-tank 25 -3)))

; Exercise 101
; SIGS KeyEvent -> SIGS
; "left" makes the tank move left, "right" to the right
; pressing space fires the missile if it hasn't been launched yet
(define (si-control s ke)
  (cond
    [(aim? s) (cond
                [(string=? "left"  ke) (make-aim (aim-ufo s) (tank-left  (aim-tank s)))]
                [(string=? "right" ke) (make-aim (aim-ufo s) (tank-right (aim-tank s)))]
                [(string=? " "     ke) (make-fired (aim-ufo s) (aim-tank s) (fire (aim-tank s)))]
                [else s])]
    [(fired? s) (cond
                  [(string=? "left"  ke) (make-fired (fired-ufo s) (tank-left  (fired-tank s)) (fired-missile s))]
                  [(string=? "right" ke) (make-fired (fired-ufo s) (tank-right (fired-tank s)) (fired-missile s))]
                  [else s]]))

(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 -3))
                          "a")
              (make-aim (make-posn 20 10) (make-tank 30 -3)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25))
                          "a")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25)))
(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 -3))
                          "left")
              (make-aim (make-posn 20 10) (make-tank 30 -3)))
(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 3))
                          "left")
              (make-aim (make-posn 20 10) (make-tank 30 -3)))
(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 -3))
                          "right")
              (make-aim (make-posn 20 10) (make-tank 30 3)))
(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 3))
                          "right")
              (make-aim (make-posn 20 10) (make-tank 30 3)))
(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 -3))
                          " ")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25))
                          "left")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25))
                          "left")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-missile 30 25))
                          "right")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25))
                          "right")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25))
                          " ")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-missile 30 25)))

; Tank -> Tank
; changes velocity left
(define (tank-left t)
  (cond
    [(> (tank-vel t) 0) (make-tank (tank-loc t) (* -1 (tank-vel t)))]
    [else t]))

; Tank -> Tank
; changes velocity right
(define (tank-right t)
  (cond
    [(< (tank-vel t) 0) (make-tank (tank-loc t) (* -1 (tank-vel t)))]
    [else t]))

; Tank -> Missile
; creates instance of missile beginning to fire
(define (fire t)
  (make-missile (tank-loc t) (- HEIGHT TANK-HEIGHT)))