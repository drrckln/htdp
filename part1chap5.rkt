;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part1chap5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

; computes the distance of a-posn to the origin
(check-expect (distance-to-0 (make-posn 0 5)) 5)
(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)
(define (distance-to-0 a-posn)
  (sqrt
   (+ (sqr (posn-x a-posn))
      (sqr (posn-y a-posn)))))

; Exercise 66
; computes the Manhattan distance of a-posn to the origin
(check-expect (manhattan-distance (make-posn 0 5)) 5)
(check-expect (manhattan-distance (make-posn 7 5)) 12)
(check-expect (manhattan-distance (make-posn 6 3)) 9)
(check-expect (manhattan-distance (make-posn 5 5)) 10)
(check-expect (manhattan-distance (make-posn 13 4)) 17)
(define (manhattan-distance a-posn)
  (+ (posn-x a-posn)
     (posn-y a-posn)))

(define-struct entry [name phone email])

(define pl
  (make-entry "Sarah Lee" "666-7771" "lee@classy-university.edu"))

(define bh
  (make-entry "Tara Harper" "666-7770" "harper@small-college.edu"))

; Exercise 67
; (define-struct movie [title producer year]) produces
; make-movie, movie-title, movie-producer, movie-year, and movie?
; (define-struct person [name hair eyes phone]) produces
; make-person, person-name, person-hair, person-eyes, person-phone, and person?
; (define-struct pet [name number]) produces
; make-pet, pet-name, pet-number, and pet?
; (define-struct CD [artist title price]) produces
; make-CD, CD-artist, CD-title, CD-price, and CD?
; (define-struct sweater [material size producer]) produces
; make-sweater, sweater-material, sweater-size, sweater-producer, and sweater?

(define-struct ball [location velocity])

; Exercise 68
(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")
(make-balld 4 "down")

(define-struct vel [deltax deltay])
(define ball1 (make-ball (make-posn 30 40) (make-vel -10 5)))

; Exercise 69
; this is known as a flat representation, rather than nested
(define-struct ballf [x y deltax deltay])
(make-ballf 30 40 -10 5)

(define-struct centry [name home office cell])

(define-struct phone [area number])

(make-centry "Shriram Fisler"
             (make-phone 207 "363-2421")
             (make-phone 101 "776-1099")
             (make-phone 208 "112-9981"))

; Exercise 70
; (centry-name (make-centry n h o c)) == n
; (centry-home (make-centry n h o c)) == h
; (centry-office (make-centry n h o c)) == o
; (centry-cell (make-centry n h o c)) == c
; (phone-area (make-phone a n)) == a
; (phone-number (make-phone a n)) == n
(phone-area
 (centry-office
  (make-centry
   "Shriram Fisler"
   (make-phone 207 "363-2421")
   (make-phone 101 "776-1099")
   (make-phone 208 "112-9981"))))
; (centry-office (make-centry n h o c)) == o
;(phone-area
; (make-phone 101 "776-1099"))
; (phone-area (make-phone a n)) == a
;101

; Exercise 71
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH 400)
(define CENTER (quotient WIDTH 2))

(define-struct game [left-player right-player ball])

(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))

; Exercise 72
; (define-struct centry [name home office cell])
; A centry is a structure: (make-centry String String String String)
; interpretation
; the name, home phone number, office phone number, and cell phone number as Strings

; (define-struct phone# [area switch num])
; A phone# is a structure: (make-phone# [Area Switch Num])
; Area is a Positive Number 001 to 999
; Switch is a Positive Number 001 to 999
; Num is a Positive Number 0001 to 9999
; interpretation Area is area code, Switch is phone switch exchange
; Num is phone with respect to neighborhood

; visual constants
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; The state of the world is represented by a Posn

; Posn -> Posn
(define (main p0)
  (big-bang p0
            [on-tick x+]
            [on-mouse reset-dot]
            [to-draw scene+dot]))

; Posn -> Image
; adds a red spot to MTS at p
(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 88 73))
              (place-image DOT 88 73 MTS))

; Posn -> Posn
; increases the x-coordinate of p by 3
(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

(check-expect (x+ (make-posn 10 10))
              (make-posn 13 10))
(check-expect (x+ (make-posn 43 0))
              (make-posn 46 0))

; Exercise 73
; Posn Number -> Posn
; takes a Posn p and Number n, and produces a Posn like p with n in the x-field
(define (posn-up-x p n)
  (make-posn n (posn-y p)))

(check-expect (posn-up-x (make-posn 10 14) 7)
              (make-posn 7 14))
(check-expect (posn-up-x (make-posn 34 10) 3)
              (make-posn 3 10))

; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(define (reset-dot p x y me)
  (cond
    [(mouse=? "button-down" me) (make-posn x y)]
    [else p]))



(check-expect (reset-dot (make-posn 10 20) 29 31 "button-down")
              (make-posn 29 31))
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-up")
              (make-posn 10 20))


(define-struct ufo [loc vel])
; A UFO is a structure: (make-ufo Posn Vel)
; interpretation (make-ufo p v) is at location p moving at velocity v
; for Vel, see above.

; task: develop ufo-move-1, which computes the location of a given UFO after one clock tick
(define v1 (make-vel 8 -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))

(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))

; UFO -> UFO
; determines where u moves in one clock tick;
; leaves the velocity as is

(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2) (make-ufo (make-posn 17 77) v2))

(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u)) (ufo-vel u)))

; If a function deals with nested structures, develop one function per level

; Posn Vel -> Posn
; adds v to p
(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))

(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))

; A BS is one of:
; - "hello",
; - "world", or
; - pi.

; Exercise 76
; A movie is a structure: (make-movie String String String)
; where the first string is the title of the movie,
; the second the producer's name, and
; the third the year the movie was produced.

; A person is a structure: (make-person String String String String)
; 1st is name of the person
; 2nd is the hair color of the person
; 3rd is eye color
; 4th is phone number

; A pet is a structure: (make-pet String PosNumber)
; 1st is the name of the pet
; 2nd is the age of the pet, a positive number

; a CD is a structure: (make-CD String String Number)
; 1st being the artist's name
; 2nd the title of the album
; 3rd being the price

; A structure is a sweater: (make-sweater String String String)
; 1st being the material
; 2nd the size of the sweater
; 3rd the producer

; Exercise 77
; A TimePoint is a structure: (make-TimePoint Hour Minute Second)
; interpretation time since midnight
; an Hour is an interval; a PositiveInteger from 0 to 23
; a Minute is an interval; PositiveInteger from 0 to 59
; a Second is an interval; PositiveInteger from 0 to 59

; Exercise 78
; a 3Word is one of:
; - a structure: (make-3Word Letter Letter Letter)
; - #false
; where Letter is one of the following 1Strings:
; - a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z

; Exercise 79
; "white", "orange", "red", "blue"
; 74
; (make-person "Jim" "Bow" #true), and no it is not
; (make-dog "Jim Bow" "Spot" 7 89)
; interpretation is a dog with properties owner's name, dog name, age, and happiness level
; #false, (make-posn 40 59)

