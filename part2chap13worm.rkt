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

;(define-struct worm [dir left top])
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
                       (list (make-segment (/ WIDTH SEGMENT-DIAMETER 2)
                                           (/ HEIGHT SEGMENT-DIAMETER 2))))
            [on-tick tock (/ 1 r)]
            [to-draw render]
            [on-key heading]
            [stop-when any-hit-wall? end-screen]))

; WormState -> Image
; places the worm on the MT
(define (render ws)
  (cond
    [(empty? (worm-segments ws)) MT]
    [else (place-image SEGMENT
                       (- (* (segment-left (first (worm-segments ws))) SEGMENT-DIAMETER)
                          (/ SEGMENT-DIAMETER 2))
                       (- (* (segment-top (first (worm-segments ws))) SEGMENT-DIAMETER)
                          (/ SEGMENT-DIAMETER 2))
                       (render (make-worm (worm-dir ws) (rest (worm-segments ws)))))]))

; WormState KeyEvent -> WormState
; changes the heading of the worm
(define (heading ws ke)
  (cond
    [(string=? "up" ke) (make-worm "up" (worm-segments ws))]
    [(string=? "down" ke) (make-worm "down" (worm-segments ws))]
    [(string=? "left" ke) (make-worm "left" (worm-segments ws))]
    [(string=? "right" ke) (make-worm "right" (worm-segments ws))]
    [else ws]))

; WormState -> WormState
; movement over time, a diameter per clock tick
(define (tock ws)
  (cond
    [(string=? "up" (worm-dir ws)) (make-worm "up"
                                              (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                  (- (segment-top (first (worm-segments ws))) 2))
                                                    (reverse (rest (reverse (worm-segments ws))))))]
    [(string=? "down" (worm-dir ws)) (make-worm "down"
                                                (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                    (+ (segment-top (first (worm-segments ws))) 2))
                                                      (reverse (rest (reverse (worm-segments ws))))))]
    [(string=? "left" (worm-dir ws)) (make-worm "left"
                                                (cons (make-segment (- (segment-left (first (worm-segments ws))) 2)
                                                                    (segment-top (first (worm-segments ws))))
                                                      (reverse (rest (reverse (worm-segments ws))))))]
    [(string=? "right" (worm-dir ws)) (make-worm "right"
                                                 (cons (make-segment (+ (segment-left (first (worm-segments ws))) 2)
                                                                     (segment-top (first (worm-segments ws))))
                                                       (reverse (rest (reverse (worm-segments ws))))))]
    [else ws]))

; Exercise 202
; Segment -> Boolean
; determines if the snake segment hit a wall
(define (hit-wall? ws)
  (cond
    [(or (= (segment-left ws) 0)
         (= (segment-left ws) (/ WIDTH SEGMENT-DIAMETER)))
     #true]
    [(or (= (segment-top ws) 0)
         (= (segment-top ws) (/ HEIGHT SEGMENT-DIAMETER)))
     #true]
    [else #false]))

; Worm -> Boolean
; determines if the snake hit a wall
(define (any-hit-wall? ws)
  (cond
    [(empty? (worm-segments ws)) #false]
    [else (or (hit-wall? (first (worm-segments ws)))
              (any-hit-wall? (make-worm (worm-dir ws)
                                        (rest (worm-segments ws)))))]))

; WormState -> Image
(define (end-screen ws)
  (overlay/xy (text "worm hit border" 20 "black")
              (* -1 25)
              (* -1 (- HEIGHT 25))
              (render ws)))

; Exercise 203
(define-struct worm [dir segments])
; a Worm is (make-worm Direction List-of-Segments)
; Direction is one of: "up", "down", "left", "right"
; A List-of-Segments is one of:
; - '()
; - (cons Segment List-of-Segments)
(define-struct segment [left top])
; A Segment is (make-segment Number Number)
; where "left" is the number of segments from the left, top etc

