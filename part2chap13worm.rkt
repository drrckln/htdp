;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; Exercise 201
; Physical constants
(define SEGMENT-DIAMETER 10)
(define HEIGHT 400)
(define WIDTH 400)
(define WORM-COLOR "red")

; Graphical constants
(define MT (empty-scene HEIGHT WIDTH))
(define SEGMENT (circle SEGMENT-DIAMETER "solid" WORM-COLOR))

(define-struct worm [dir left top])
; trying the logical definition
; A WormState is (make-worm Direction Number Number)
; interpretation "left" and "top" denote segment-widths from the left and top
; Direction is one of:
; - "up", "down", "left", or "right"
; indicates the worm's bearing/heading

; Rate -> WormState
; takes in a clock rate
(define (main r)
  (big-bang (make-worm ""
                       (/ WIDTH SEGMENT-DIAMETER 2)
                       (/ HEIGHT SEGMENT-DIAMETER 2))
            [on-tick tock (/ 1 r)]
            [to-draw render]
            [on-key heading]
            [stop-when hit-wall? end-screen]))

; WormState -> Image
; places the worm on the MT
(define (render ws)
  (place-image SEGMENT
               (- (* (worm-left ws) SEGMENT-DIAMETER)
                  (/ SEGMENT-DIAMETER 2))
               (- (* (worm-top ws) SEGMENT-DIAMETER)
                  (/ SEGMENT-DIAMETER 2))
               MT))

; WormState KeyEvent -> WormState
; changes the heading of the worm
(define (heading ws ke)
  (cond
    [(string=? "up" ke) (make-worm "up" (worm-left ws) (worm-top ws))]
    [(string=? "down" ke) (make-worm "down" (worm-left ws) (worm-top ws))]
    [(string=? "left" ke) (make-worm "left" (worm-left ws) (worm-top ws))]
    [(string=? "right" ke) (make-worm "right" (worm-left ws) (worm-top ws))]
    [else ws]))

; WormState -> WormState
; movement over time, a diameter per clock tick
(define (tock ws)
  (cond
    [(string=? "up" (worm-dir ws)) (make-worm "up" (worm-left ws) (- (worm-top ws) 2))]
    [(string=? "down" (worm-dir ws)) (make-worm "down" (worm-left ws) (+ (worm-top ws) 2))]
    [(string=? "left" (worm-dir ws)) (make-worm "left" (- (worm-left ws) 2) (worm-top ws))]
    [(string=? "right" (worm-dir ws)) (make-worm "right" (+ (worm-left ws) 2) (worm-top ws))]
    [else ws]))

; Exercise 202
; WormState -> Boolean
; determines if the snake hit a wall
(define (hit-wall? ws)
  (cond
    [(or (= (worm-left ws) 0)
         (= (worm-left ws) (/ WIDTH SEGMENT-DIAMETER)))
     #true]
    [(or (= (worm-top ws) 0)
         (= (worm-top ws) (/ HEIGHT SEGMENT-DIAMETER)))
     #true]
    [else #false]))

; WormState -> Image
(define (end-screen ws)
  (overlay/xy (text "worm hit border" 20 "black")
              (* -1 25)
              (* -1 (- HEIGHT 25))
              (render ws)))