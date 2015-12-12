;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; physical constants
(define WIDTH 10) ; the maximal number of blocks horizontally
(define HEIGHT 15) ; ... vertically

; graphical constants
(define SIZE 10) ; blocks are square
(define BLOCK ; they are rendered as red squares with black rims
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))

(define SCENE-SIZE (* WIDTH SIZE)) ; as width is in block units
(define MT (empty-scene SCENE-SIZE (* HEIGHT SIZE)))

(define-struct tetris [block ls])
; A Tetris is (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
;- (cons Block Landscape)
; Block is (make-block N N)
(define-struct block [x y])

; interpretation given (make-tetris (make-block x y) (list b1 b2 ...))
; (x,y) is the logical position of the dropping block, while
; b1, b2, etc are the positions of the resting blocks
; a logical position (x,y) determines how many SIZEs the block is
;   from the left-x and from the top-y.

; Exercise 206
(define landscape0 '())
(define block-dropping (make-block 0 1))
(define tetris0 (make-tetris (make-block 0 0) landscape0))
(define tetris0-drop (make-tetris block-dropping landscape0))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define tetris1 (make-tetris block-on-block (list block-landed)))

; Tetris -> Image
; renders the tetris instance as an image
(define (tetris-render t)
  (place-image/align BLOCK
                      (* SIZE (block-x (tetris-block t)))
                      (* SIZE (block-y (tetris-block t)))
                      "left" "top"
                      (landscape-render (tetris-ls t))))

; List-of-blocks -> Image
; renders the current landscape
(define (landscape-render ls)
  (cond
    [(empty? ls) MT]
    [else (place-image/align BLOCK
                             (* SIZE (block-x (first ls)))
                             (* SIZE (block-y (first ls)))
                             "left" "top"
                             (landscape-render (rest ls)))]))

(check-expect (tetris-render tetris0)
              (place-image/align BLOCK 0 0 "left" "top" MT))
(check-expect (tetris-render tetris0-drop)
              (place-image/align BLOCK 0 (* 1 SIZE) "left" "top" MT))
(check-expect (tetris-render tetris1)
              (place-image/align BLOCK 0 (* SIZE (- HEIGHT 2)) "left" "top"
                                 (place-image/align BLOCK 0 (* SIZE (- HEIGHT 1)) "left" "top" MT)))

; Exercise 207
; Number -> Tetris
(define (tetris-main r)
  (big-bang (make-tetris (make-block 0 0) '())
            [on-tick tetris-tock (/ 1 r)]
            [to-draw tetris-render]
            [on-key tetris-key]
            [stop-when tetris-stop?]))

; Block Landscape -> Boolean
; determines if the block has landed
(define (landed? b ls)
  (cond
    [(= (block-y b) HEIGHT) #true]
    [else (member? b ls)]))

; Tetris -> Tetris
; drops blocks, lands blocks, and creates new blocks
(define (tetris-tock t)
  (cond
    [(landed? (make-block (block-x (tetris-block t))
                          (+ (block-y (tetris-block t)) 1))
              (tetris-ls t))
     (make-tetris (block-generate (block-x (tetris-block t)))
                  (cons (tetris-block t)
                        (tetris-ls t)))]
    [else (make-tetris (make-block (block-x (tetris-block t))
                                   (+ (block-y (tetris-block t)) 1))
                       (tetris-ls t))]))

; Number -> Block
; takes in a block x column and creates a block at a new column
(define (block-generate x)
  (block-check-generate x (make-block (random WIDTH) 0)))

; Number Block -> Block
(define (block-check-generate x candidate)
  (if (equal? x (block-x candidate))
      (block-generate x)
      candidate))

; Exercise 208
; modifying..
; Tetris KeyEvent -> Tetris
; tetris-key handles moving left or right
(define (tetris-key t ke)
  (cond
    [(and (string=? "left" ke)
          (left-ok? t))
     (make-tetris (make-block (- (block-x (tetris-block t)) 1)
                              (block-y (tetris-block t)))
                  (tetris-ls t))]
    [(and (string=? "right" ke)
          (right-ok? t))
     (make-tetris (make-block (+ (block-x (tetris-block t)) 1)
                              (block-y (tetris-block t)))
                  (tetris-ls t))]
    [else t]))

; Tetris -> Boolean
; determines if moving the block left is ok
(define (left-ok? t)
  (cond
    [(= (block-x (tetris-block t)) 0) #false]
    [(member? (make-block (- (block-x (tetris-block t)) 1)
                          (block-y (tetris-block t)))
              (tetris-ls t))
     #false]
    [else #true]))

; Tetris -> Boolean
; determines if moving the block right is ok
(define (right-ok? t)
  (cond
    [(= (block-x (tetris-block t)) (sub1 WIDTH)) #false]
    [(member? (make-block (+ (block-x (tetris-block t)) 1)
                          (block-y (tetris-block t)))
              (tetris-ls t))
     #false]
    [else #true]))

; Exercise 209
; Tetris -> Boolean
; determines when to stop the game, passing #true you want to stop
(define (tetris-stop? t)
  (check-columns WIDTH HEIGHT (tetris-ls t)))

; Nat Landscape -> List-of-blocks
; finds all blocks in the landscape with x-coordinate x0
(define (find-col x0 ls)
  (cond
    [(empty? ls) '()]
    [(= x0 (block-x (first ls)))
     (cons (first ls) (find-col x0 (rest ls)))]
    [else (find-col x0 (rest ls))]))

; Nat Nat Landscape -> Boolean
; checks all the columns, #true if any is HEIGHT high
(define (check-columns w h ls)
  (cond
    [(< w 0) #false]
    [(= h (length (find-col w ls)))
     #true]
    [else (check-columns (sub1 w) h ls)]))