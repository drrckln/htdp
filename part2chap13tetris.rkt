;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 10) ; the maximal number of blocks horizontally

; graphical constants
(define SIZE 10) ; blocks are square
(define BLOCK ; they are rendered as red squares with black rims
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))

(define SCENE-SIZE (* WIDTH SIZE)) ; as width is in block units

; A Tetris is (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
;- (cons Block Landscape)
; Block is (make-block N N)

; interpretation given (make-tetris (make-block x y) (list b1 b2 ...))
; (x,y) is the logical position of the dropping block, while
; b1, b2, etc are the positions of the resting blocks
; a logical position (x,y) determines how many SIZEs the block is
;   from the left-x and from the top-y.