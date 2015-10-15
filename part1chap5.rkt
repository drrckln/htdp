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
