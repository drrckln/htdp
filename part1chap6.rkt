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
(define TANK-SPEED 6)
(define UFO (ellipse 30 15 125 "blue"))
(define UFO-SPEED 3)
(define UFO-WARP 15)
(define MISSILE-SPEED (* -3 UFO-SPEED))
(define WIDTH 300)
(define HEIGHT 600)
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
               (modulo (tank-loc t) WIDTH) (- HEIGHT (/ TANK-HEIGHT 2))
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
    [(and (fired? sigs)
          (landed? (fired-ufo sigs)))
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
  (<= (near u m) (image-width UFO)))

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
; decides whether to jump left or right, jumping by UFO-WARP distance
(check-random (create-random-number 0)
              (cond
                [(>= (random 20) 19) UFO-WARP]
                [(>= (random 20) 19) (* -1 UFO-WARP)]
                [else 0]))
(define (create-random-number w)
  (cond
    [(>= (random 20) 19) UFO-WARP]
    [(>= (random 20) 19) (* -1 UFO-WARP)]
    [else 0]))

; testing random function!
(check-expect (si-move-proper (make-aim (make-posn 20 10) (make-tank 28 -3)) 10)
              (make-aim (make-posn 30 (+ 10 UFO-SPEED)) (make-tank 25 -3)))

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
                  [else s])]))

(check-expect (si-control (make-aim (make-posn 20 10) (make-tank 30 -3))
                          "a")
              (make-aim (make-posn 20 10) (make-tank 30 -3)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25))
                          "a")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25)))
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
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25))
                          "left")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25))
                          "left")
              (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 -3) (make-posn 30 25))
                          "right")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25))
                          "right")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25)))
(check-expect (si-control (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25))
                          " ")
              (make-fired (make-posn 20 10) (make-tank 30 3) (make-posn 30 25)))

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

; SIGS -> SIGS
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when si-game-over? si-render-final]))

; SIGS.v2 -> Image
; renders the given game state and added it to BACKGROUND
(define (si-render.v2 s)
  (tank-render (sigs-tank s)
               (ufo-render (sigs-ufo s)
                           (missile-render.v2 (sigs-missile s)
                                              BACKGROUND))))

(define-struct sigs [ufo tank missile])
; SIGS.v2 (short for version 2)
; is (make-sigs UFO Tank MissileOrNot)
; interpretation represents the state of the space invader game

; A MissileOrNot is one of:
; - #false
; - Posn
; interpretation #false means the missile hasn't been fired yet;
; Posn says the missile has been fired and is at the specified location.

; Exercise 102
; MissileOrNot Image -> Image
; adds the missile iamge to sc for m
(define (missile-render.v2 m scene)
  (cond
    [(boolean? m) scene]
    [(posn? m) (place-image MISSILE (modulo (posn-x m) WIDTH) (posn-y m) scene)]))

(check-expect (missile-render.v2 #false BACKGROUND) BACKGROUND)
(check-expect (missile-render.v2 (make-posn 32 (- HEIGHT TANK-HEIGHT 10)) BACKGROUND)
              (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10) BACKGROUND))

; Exercise 103

; SIGS.v2 -> SIGS.v2
; moves all objects by their designated velocities
(define (si-move.v2 s)
  (si-move-proper.v2 s (create-random-number s)))

; SIGS.v2 Number -> SIGS.v2
; referentially transparent version of si-move.v2
; moves all objects, including the UFO horizontally by random number r
(define (si-move-proper.v2 s r)
  (make-sigs (move-ufo (sigs-ufo s) r)
             (move-tank (sigs-tank s))
             (move-missile.v2 (sigs-missile s))))

(check-expect (si-move-proper.v2 (make-sigs (make-posn 20 10)
                                     (make-tank 20 -3)
                                     #false)
              10)
              (make-sigs (make-posn (modulo 30 WIDTH) (+ 10 UFO-SPEED))
                         (make-tank 17 -3)
                         #false))

; Missile -> Missile
; moves the missile if it exists
(define (move-missile.v2 m)
  (cond
    [(boolean? m) #false]
    [(posn? m) (make-posn (posn-x m)
                          (+ (posn-y m) MISSILE-SPEED))]))

; SIGS.v2 -> Boolean
; #true when UFO lands or missile hits UFO
(define (si-game-over.v2? sigs)
  (cond
    [(landed? (sigs-ufo sigs)) #true]
    [(boolean? (sigs-missile sigs)) #false]
    [(hit? (sigs-ufo sigs) (sigs-missile sigs)) #true]
    [(landed? (sigs-ufo sigs)) #true]
    [else #false]))

; SIGS.v2 KeyEvent -> SIGS.v2
; "left" makes the tank move left, "right" to the right
; pressing space fires the missile if it hasn't been launched yet
(define (si-control.v2 s ke)
  (cond
    [(string=? "left"  ke) (make-sigs (sigs-ufo s) (tank-left  (sigs-tank s)) (sigs-missile s))]
    [(string=? "right" ke) (make-sigs (sigs-ufo s) (tank-right (sigs-tank s)) (sigs-missile s))]
    [(string=? " "     ke) (make-sigs (sigs-ufo s) (sigs-tank s) (fire (sigs-tank s)))]
    [else s]))

; SIGS.v2 -> Image
; places GAME OVER on the final image
(define (si-render-final.v2 sigs)
  (overlay/align "middle" "middle"
                 (text "GAME OVER" 25 "indigo")
                 (si-render.v2 sigs)))

(define (si-main.v2 s)
  (big-bang s
            [on-tick si-move.v2]
            [to-draw si-render.v2]
            [on-key si-control.v2]
            [stop-when si-game-over.v2? si-render-final.v2]))

; Exercise 104
; a Person is one of:
; Student (make-student String String Number)
; - (make-student [first last gpa])
; Professor (make-professor String String Boolean)
; - (make-professor [first last tenure?])
; Staff (make-staff String String String)
; - (make-staff [first last salary-group])

; Person -> ?
; (define (function p)
;   (cond
;     [(student? p) (... (student-first p) ... (student-last p) ... (student-gpa p) ...)]
;     [(professor? p) (... (professor-first p) ... (professor-last p) ... (professor-tenure? p) ...)]
;     [(staff? p) (... (staff-first p) ... (staff-last p) ... (staff-salary-group p)...)]))

; Exercise 105
(define-struct spider [rem-legs space])
; A Spider is (make-spider Number Number)
; - (make-spider [rem-legs space])
(define-struct elephant [space])
; An Elephant is (make-elephant Number)
; - (make-elephant [space])
(define-struct boa [length girth])
; A Boa is (make-boa Number Number)
; - (make-boa [length girth])
(define-struct armadillo [tail? space])
; A Armadillo is (make-armadillo Boolean Number)
; - (make-armadillo [tail? space])
; A ZooAnimal is one of:
; - Spider
; - Elephant
; - Boa
; - Armadillo

; ZooAnimal -> ?
(define (fzanimal zanimal)
  (cond
    [(spider? zanimal) (... (spider-rem-legs zanimal) ... (spider-space zanimal) ...)]
    [(elephant? zanimal) (... (elephant-space zanimal) ...)]
    [(boa? zanimal) (... (boa-length zanimal) ... (boa-girth zanimal) ...)]
    [(armadillo? zanimal) (... (armadillo-tail? zanimal) ... (armadillo-space zanimal) ...)]))

; ZooAnimal Number -> Boolean
; (fits animal volume) determines whether the animal can fit in the volume given
(define (fits a v)
  (cond
    [(spider? a) (>= (spider-space a) v)]
    [(elephant? a) (>= (elephant-space a) v)]
    [(boa? a) (>= (* (boa-length a) (boa-girth a)) v)]
    [(armadillo? a) (>= (armadillo-space a) v)]))

; Exercise 106
(define-struct vehicle [capacity license mpg])
; A Vehicle is (make-vehicle Number String Number)
; interpretation (make-vehicle [capacity license mpg])

; Vehicle -> ?
(define (fvehicle v)
  (... (vehicle-capacity v) ... (vehicle-license v) ... (vehicle-mpg v) ...))

; Exercise 107
; -3, -5 .. 3 and 5 units distance from the top, on the Y axis
; 4, 6, 4 or 6 on the x-axis, distance from the left
; (make-posn 3 5) 3 left and 5 down, (make-posn 7 4) 7 from left and 4 down