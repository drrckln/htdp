;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo_scope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; Exercise 287
(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))

(define (p2 x)
  (+ (* 55 x) (+ x 11)))

(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

; Exercise 288
; No, these are the same function (diagrams drawn in notebook)

; Exercise 289
; (define x (cons 1 >x<) >x< is bound at the first x
; I guess it's an infinite loop of (cons 1 (cons 1 (cons 1 ...

;(define x (cons 1 x))
#|
; Exercise 290
; > < highlighted
; { } binding
(lambda ({x} y)
  (+ >x< (* x y))) 

(lambda ({x}0 y)
  (+ >x<0
     (local ((define {x}1 (* y y)))
       (+ (* 3 >x<1)
          (/ 1 x)))))

(lambda ({x}0 y)
  (+ >x<0
     ((lambda ({x}1)
        (+ (* 3 >x<1)
           (/ 1 x)))
      (* y y))))
|#

; [List-of X] -> [List-of (Number, X)]
(define (enumerate2 lox)
  (local ((define index (build-list (length lox) identity)))
    (map list index lox)))

; [List-of X] -> [List-of [List N X]]
; pair each item in l with its index
(check-expect (enumerate '(a b c)) '((1 a) (2 b) (3 c)))
(define (enumerate l)
  (for/list ((item l) (ith (length l)))
    (list (+ ith 1) item)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
(define (cross2 l1 l2)
  (foldl append '()
         (map (lambda (y) (map (lambda (x) (list x y)) l1))
              l2)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generate all possible pairs of items from l1 and l2
(check-satisfied (cross '(a b c) '(1 2)) (lambda (c) (= (length c) 6)))
(define (cross l1 l2)
  (for*/list ([item1 l1][item2 l2])
    (list item1 item2)))

(define width 2)
(for*/list ([width 3][height width])
  (list width height))

; and-map gives the last generated value or #false
; [X -> Maybe Y] [List-of X] -> Maybe X
(define (and-map f ls)
  (cond
    [(empty? (rest ls)) (if (equal? (f (first ls)) #false)
                            #false
                            (first ls))]
    [(equal? (f (first ls)) #false) #false]
    [else (and-map f (rest ls))]))

; or-map gives the first value that is not #false, else #false (all #false)
; [X -> Maybe Y] [List-of X] -> Maybe X
(define (or-map f ls)
  (cond
    [(empty? ls) #false]
    [(not (equal? (f (first ls)) #false))
     (first ls)]
    [else (or-map f (rest ls))]))

; N -> sequence?
; construct an infinite sequence of natural numbers starting at n
(define (in-naturals n) ...)