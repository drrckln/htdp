;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3chap18ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Exercise 263. Full Space War spells out a game of space war. In the
basic version, a UFO descends and a player defends with a tank. One
additional suggestion is to equip the UFO with charges that it can
drop at the tank; the tank is destroyed if a charge comes close enough.

Inspect the code of your project for places where it can benefit from
existing abstraction, e.g., processing lists of shots or charges.

Once you have simplified the code with the use of existing abstractions
look for opportunities to create abstractions. Consider moving lists
of objects as one example where abstraction may pay off.
|#

(require 2htdp/image)
(require 2htdp/universe)

; Constants
(define MISSILE (triangle 8 "solid" "black"))
(define TANK (rectangle 20 15 "solid" "green"))
(define TANK-HEIGHT (image-height TANK))
(define TANK-SPEED 6)
(define UFO (ellipse 30 15 125 "blue"))
(define UFO-SPEED 3)
(define UFO-WARP 15)
(define MISSILE-SPEED (* -3 UFO-SPEED))
(define WIDTH 300)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


(define-struct sigs [ufo tank missiles])
; SIGS is (make-sigs UFO Tank List-of-missile)
; interpretation represents the state of the space invader game

; A UFO is Posn
; interpretation (make-posn x y) is the UFO's current location

(define-struct tank [loc vel])
; a Tank is (make-tank Number Number)
; interpretation (make-tank x dx) means the tank is at position
; (x, HEIGHT) and that it moves dx pixels per clock tick

; a Missile is Posn
; interpretation (make-posn x y) is the missile's current location


; SIGS -> SIGS
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))

; SIGS -> Image
; renders the given game state and added it to BACKGROUND
(define (si-render s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missiles-render (sigs-missiles s)
                                            BACKGROUND))))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK
               (modulo (tank-loc t) WIDTH) (- HEIGHT (/ TANK-HEIGHT 2))
               im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO
               (posn-x u) (posn-y u)
               im))

; List-of-missile Image -> Image
; adds all missiles to the given image
(define (missiles-render lom im)
  (local (; Missile Image -> Image
          ; adds m to the given image im
          (define (missile-render m img)
            (place-image MISSILE
                         (posn-x m) (posn-y m)
                         img)))
  (foldr missile-render im lom)))

; SIGS -> SIGS
; moves all objects by their designated velocities
(define (si-move s)
  (si-move-proper s (create-random-number s)))

; SIGS -> Number
; create a random number in case a UFO should perform a horizontal jump
; decides whether to jump left or right, jumping by UFO-WARP distance
(define (create-random-number w)
  (cond
    [(>= (random 20) 19) UFO-WARP]
    [(>= (random 20) 19) (* -1 UFO-WARP)]
    [else 0]))

(check-random (create-random-number 0)
              (cond
                [(>= (random 20) 19) UFO-WARP]
                [(>= (random 20) 19) (* -1 UFO-WARP)]
                [else 0]))

; SIGS Number -> SIGS
; referentially transparent version of si-move.v2
; moves all objects, including the UFO horizontally by random number r
(define (si-move-proper s r)
  (make-sigs (move-ufo (sigs-ufo s) r)
             (move-tank (sigs-tank s))
             (move-missiles (sigs-missiles s))))

(check-expect (si-move-proper (make-sigs (make-posn 20 10)
                                         (make-tank 20 -3)
                                         '())
                              10)
              (make-sigs (make-posn (modulo 30 WIDTH) (+ 10 UFO-SPEED))
                         (make-tank 17 -3)
                         '()))

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

; List-of-missile -> List-of-missile
; moves all the existing missiles
(define (move-missiles lom)
  (local (; Missile -> Missile
          ; updates the missile
          (define (update-m m)
            (make-posn (posn-x m)
                       (+ (posn-y m) MISSILE-SPEED))))
    (map update-m lom)))

; SIGS KeyEvent -> SIGS
; "left" makes the tank move left, "right" to the right
; pressing space fires a missile
(define (si-control s ke)
  (cond
    [(string=? "left"  ke) (make-sigs (sigs-ufo s) (tank-left  (sigs-tank s)) (sigs-missiles s))]
    [(string=? "right" ke) (make-sigs (sigs-ufo s) (tank-right (sigs-tank s)) (sigs-missiles s))]
    [(string=? " "     ke) (make-sigs (sigs-ufo s) (sigs-tank s) (cons (fire (sigs-tank s)) (sigs-missiles s)))]
    [else s]))

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
  (make-posn (tank-loc t) (- HEIGHT TANK-HEIGHT)))

; SIGS -> Boolean
; #true when UFO lands or a missile hits UFO
(define (si-game-over? sigs)
  (or (landed? (sigs-ufo sigs))
      (any-hit? (sigs-ufo sigs) (sigs-missiles sigs))))

; UFO -> Boolean
; determines whether the UFO has landed
(define (landed? u)
  (>= (posn-y u)
      (- HEIGHT (/ (image-height UFO) 2))))

; UFO List-of-missile -> Boolean
; determines whether any missile hit the UFO
(define (any-hit? u lom)
  (local (; Missile -> Boolean
          ; determines whether the missile is close enough to the UFO to hit
          (define (hit? m)
            (<= (near u m) (image-width UFO))))
    (ormap hit? lom)))

; Posn Posn -> Number
; determines the distance between two Posns
(define (near a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

; SIGS -> Image
; places GAME OVER on the final image
(define (si-render-final sigs)
  (overlay/align "middle" "middle"
                 (text "GAME OVER" 25 "indigo")
                 (si-render sigs)))



; initial scene
(place-image TANK
             (/ (image-width BACKGROUND) 2)
             (- (image-height BACKGROUND) (/ (image-height TANK) 2))
             (place-image UFO
                          (/ (image-width BACKGROUND) 2)
                          (+ 0 (image-height UFO))
                          BACKGROUND))

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

(si-main (make-sigs (make-posn 10 5)
                      (make-tank 20 TANK-SPEED)
                      '()))