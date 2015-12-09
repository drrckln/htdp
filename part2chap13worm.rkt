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
(define MAX (/ WIDTH SEGMENT-DIAMETER))
(define WORM-COLOR "red")
(define FOOD-COLOR "green")

; Graphical constants
(define MT (empty-scene HEIGHT WIDTH))
(define SEGMENT (circle SEGMENT-DIAMETER "solid" WORM-COLOR))
(define FOOD (circle SEGMENT-DIAMETER "solid" FOOD-COLOR))

;(define-struct worm [dir left top])
; trying the logical definition
; A WormState is (make-worm Direction Number Number)
; interpretation "left" and "top" denote sercgment-widths from the left and top
; Direction is one of:
; - "up", "down", "left", or "right"
; indicates the worm's bearing/heading

; Rate -> Number
; takes in a clock rate, outputs a length score
(define (main r)
  (length
   (worm-segments
    (big-bang (make-worm (food-create (make-posn (/ WIDTH SEGMENT-DIAMETER 2)
                                                 (/ HEIGHT SEGMENT-DIAMETER 2)))
                         ""
                         (list (make-segment (/ WIDTH SEGMENT-DIAMETER 2)
                                             (/ HEIGHT SEGMENT-DIAMETER 2))))
              [on-tick tock (/ 1 r)]
              [to-draw render]
              [on-key heading]
              [stop-when any-hit? end-screen]))))

; WormState -> Image
; places the worm and food on the MT
(define (render ws)
  (cond
    [(empty? (worm-segments ws)) (place-image FOOD
                                              (- (* (posn-x (worm-food ws)) SEGMENT-DIAMETER)
                                                 (/ SEGMENT-DIAMETER 2))
                                              (- (* (posn-y (worm-food ws)) SEGMENT-DIAMETER)
                                                 (/ SEGMENT-DIAMETER 2))
                                              MT)]
    [else (place-image SEGMENT
                       (- (* (segment-left (first (worm-segments ws))) SEGMENT-DIAMETER)
                          (/ SEGMENT-DIAMETER 2))
                       (- (* (segment-top (first (worm-segments ws))) SEGMENT-DIAMETER)
                          (/ SEGMENT-DIAMETER 2))
                       (render (make-worm (worm-food ws) (worm-dir ws) (rest (worm-segments ws)))))]))

; WormState KeyEvent -> WormState
; changes the heading of the worm
(define (heading ws ke)
  (cond
    [(string=? "up" ke) (make-worm (worm-food ws) "up" (worm-segments ws))]
    [(string=? "down" ke) (make-worm (worm-food ws) "down" (worm-segments ws))]
    [(string=? "left" ke) (make-worm (worm-food ws) "left" (worm-segments ws))]
    [(string=? "right" ke) (make-worm (worm-food ws) "right" (worm-segments ws))]
    [else ws]))

; WormState -> WormState
; movement over time, a diameter per clock tick
(define (tock ws)
  (cond
    [(string=? "up" (worm-dir ws)) (cond
                                     [(and (within-one? (posn-x (worm-food ws))
                                                        (segment-left (first (worm-segments ws))))
                                           (within-one? (posn-y (worm-food ws))
                                                        (- (segment-top (first (worm-segments ws))) 2)))
                                      (make-worm (food-create (worm-food ws))
                                                 "up"
                                                 (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                     (- (segment-top (first (worm-segments ws))) 2))
                                                       (worm-segments ws)))]
                                     [else (make-worm (worm-food ws)
                                                      "up"
                                                      (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                          (- (segment-top (first (worm-segments ws))) 2))
                                                            (reverse (rest (reverse (worm-segments ws))))))])]
    [(string=? "down" (worm-dir ws)) (cond
                                       [(and (within-one? (posn-x (worm-food ws))
                                                          (segment-left (first (worm-segments ws))))
                                             (within-one? (posn-y (worm-food ws))
                                                          (+ (segment-top (first (worm-segments ws))) 2)))
                                        (make-worm (food-create (worm-food ws))
                                                   "down"
                                                   (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                       (+ (segment-top (first (worm-segments ws))) 2))
                                                         (worm-segments ws)))]
                                       [else (make-worm (worm-food ws)
                                                        "down"
                                                        (cons (make-segment (segment-left (first (worm-segments ws)))
                                                                            (+ (segment-top (first (worm-segments ws))) 2))
                                                              (reverse (rest (reverse (worm-segments ws))))))])]
    [(string=? "left" (worm-dir ws)) (cond
                                       [(and (within-one? (posn-x (worm-food ws))
                                                          (- (segment-left (first (worm-segments ws))) 2))
                                             (within-one? (posn-y (worm-food ws))
                                                          (segment-top (first (worm-segments ws)))))
                                        (make-worm (food-create (worm-food ws))
                                                   "left"
                                                   (cons (make-segment (- (segment-left (first (worm-segments ws))) 2)
                                                                       (segment-top (first (worm-segments ws))))
                                                         (worm-segments ws)))]
                                       [else (make-worm (worm-food ws)
                                                        "left"
                                                        (cons (make-segment (- (segment-left (first (worm-segments ws))) 2)
                                                                            (segment-top (first (worm-segments ws))))
                                                              (reverse (rest (reverse (worm-segments ws))))))])]
    [(string=? "right" (worm-dir ws)) (cond
                                        [(and (within-one? (posn-x (worm-food ws))
                                                           (+ (segment-left (first (worm-segments ws))) 2))
                                              (within-one? (posn-y (worm-food ws))
                                                           (segment-top (first (worm-segments ws)))))
                                         (make-worm (food-create (worm-food ws))
                                                    "right"
                                                    (cons (make-segment (+ (segment-left (first (worm-segments ws))) 2)
                                                                        (segment-top (first (worm-segments ws))))
                                                          (worm-segments ws)))]
                                        [else (make-worm (worm-food ws)
                                                         "right"
                                                         (cons (make-segment (+ (segment-left (first (worm-segments ws))) 2)
                                                                             (segment-top (first (worm-segments ws))))
                                                               (reverse (rest (reverse (worm-segments ws))))))])]
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
              (any-hit-wall? (make-worm (worm-food ws)
                                        (worm-dir ws)
                                        (rest (worm-segments ws)))))]))

; WormState -> Image
(define (end-screen ws)
  (cond
    [(hit-self? ws) (overlay/xy (text "worm hit self" 20 "black")
                                    (* -1 25)
                                    (* -1 (- HEIGHT 25))
                                    (render ws))]
    [(any-hit-wall? ws) (overlay/xy (text "worm hit border" 20 "black")
                                    (* -1 25)
                                    (* -1 (- HEIGHT 25))
                                    (render ws))]))

; Exercise 203
;(define-struct worm [dir segments])
; a Worm is (make-worm Direction List-of-Segments)
; Direction is one of: "up", "down", "left", "right"
; A List-of-Segments is one of:
; - '()
; - (cons Segment List-of-Segments)
(define-struct segment [left top])
; A Segment is (make-segment Number Number)
; where "left" is the number of segments from the left, top etc

; Exercise 204
; Worm -> Boolean
; determines if the snake hit a wall or itself
(define (any-hit? ws)
  (or (any-hit-wall? ws)
      (hit-self? ws)))
      
; Worm -> Boolean
; determines if the snake will hit itself
(define (hit-self? ws)
  (member? (first (worm-segments (tock ws)))
           (rest (worm-segments ws))))

; Exercise 205
(define-struct worm [food dir segments])
; ...
; Food is a Posn, though in the same units as segment-left and segment-top

; Posn -> Posn
; takes a Posn, and generates a new Posn, passing off both to
; food-check-create to make sure it isn't the same area

(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (food-check-create p (make-posn (random MAX) (random MAX))))

; Posn Posn -> Posn
; generative recursion
; appears to check if p is the same as candidate. if not, use it.
; otherwise generate a new random p
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1)
            (= (posn-y p) 1))))

; Number Number -> Boolean
; checks whether two numbers are within 1 of each other
(define (within-one? x y)
  (<= (abs (- x y)) 1))