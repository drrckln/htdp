;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13firefight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define HEIGHT 500)
(define WIDTH 500)
(define TREE-SIZE 20)

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define TREE (triangle TREE-SIZE "solid" "green"))
(define FIRE (overlay/align "middle" "top"
                            (circle 5 "solid" "red")
                            TREE))
(define WATER (circle 7 "solid" "blue"))

; data definition
(define-struct forest [trees fires])
; Forest is a (make-forest List-of-tree List-of-fire), where these are mutually exclusive sets
; Tree is a Posn
; Fire is a Posn

; Forest -> Forest
; displays the forest, sets them on fire randomly
(define (fire-main f)
  (big-bang f
            [to-draw fire-render]
            [on-tick create-fire (/ 1 10)]))

; Forest -> Image
; displays the forest and the fires
(define (fire-render f)
  (render FIRE (forest-trees f)
          (render TREE (forest-fires f) MT)))

; Image List-of-Posn Scene -> Image
; populates the scene with the image i at every position
(define (render i lop scene)
  (cond
    [(empty? lop) scene]
    [else (place-image i
                       (posn-x (first lop))
                       (posn-y (first lop))
                       (render i (rest lop) scene))]))

; Forest - Forest
; randomly sets a tree on fire
(define (create-fire f)
  (make-forest (random-pick (forest-trees f))
               (cons (random-pick (forest-trees f)) (forest-fires f))))