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