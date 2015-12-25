;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Exercise 221

; Los -> Boolean
; searches for "atom" in the los
(define (contains-atom? los)
  (contains? "atom" los))

; Los -> Boolean
; searches for "basic" in the los
(define (contains-basic? los)
  (contains? "basic" los))

; Los -> Boolean
; searches for "zoo" in the los
(define (contains-zoo? los)
  (contains? "zoo" los))

; Exercise 222
; Lon -> Lon
; add 1 to each number on l
(define (add1* l)
  (map-add 1 l))

; Lon -> Lon
; adds 5 to each number on l
(define (plus5 l)
  (map-add 5 l))

; Number Lon -> Lon
; adds n to each number on l
(define (map-add n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n)
                (map-add n (rest l)))]))

(check-expect (add1* (list 1 2 3 4))
              (list 2 3 4 5))
(check-expect (add1* '()) '())

(check-expect (plus5 '()) '())
(check-expect (plus5 (list 3 5 6)) '(8 10 11))

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l) (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))

; Exercise 223
; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

; Exercise 224
; R Nelon -> Number
; determins the Rth number on l
(define (most R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (cond
            [(R (first l) (most R (rest l)))
             (first l)]
            [else (most R (rest l))])]))

(define (inf-1 l)
  (most < l))

(define (sup-1 l)
  (most > l))

; The reason it's slow is probably because it needs to calculate (extract R (rest l) t)
; over and over and over again.. once per comparison, so like O(2n)? not sure..

(define (extract-2 R l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (R (first l) (extract-2 R (rest l)))]))

(define (inf-2 l)
  (extract-2 max l))

(define (sup-2 l)
  (extract-2 min l))

; Faster because each min only runs once.. so it's like O(n) with the number of list items

; A [List-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])
; parametric data definitions

; A [CP H V] is a structure:
;   (make-point H V)
(define-struct point [hori veri])

; Exercise 225
; A [List X Y] is a structure:
;   (cons X (cons Y '()))

; [List Number Number]
(cons 10 (cons 4 '()))

; [List Number 1String]
(cons 5 (cons "s" '()))

; [List String Boolean]
(cons "hello" (cons #f '()))

; Exercise 226
; A Nested-string is one of:
; - String
; - (make-layer Nested-string)

; A Nested-number is one of:
; - Number
; - (make-layer Nested-number)

(define-struct layer [stuff])
(make-layer (make-layer (make-layer 4)))
(make-layer (make-layer "whee"))

; A [Nested ITEM] is one of:
; - ITEM
; - (make-layer [Nested ITEM])

; instantiate with Number and String..

; Exercise 227
; A [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])

; Exercise 228
; A [Bucket ITEM] is (make-bucket N [List-of ITEM])
; interpretation the n in (make-bucket n l) is the length of l

; [Bucket String]
; List of Strings, along with the length of the list
; (make-bucket 3 '("a" "b" "x"))
; [Bucket IR]
; List of IR, along with the total number of IR in the list
; (make-bucket 2 '((make-ir "a" 7) (make-ir "whoo" 3)))
; [Bucket Posn]
; List of Posns, along with the number of Posns
; (make-bucket 1 '((make-posn 10 3)))

; [Bucket [List-of [List-of String]]]
;(make-bucket 2 '('("a" "b" "c") '("x" "d")))
;(make-bucket 1 '('("c" "d")))
;(make-bucket 3 '('("a") '("f") '("jeez")))

; Exercise 229
; A [Maybe X] is one of:
; - #false
; - X

; [Maybe String] is either a String or #false
; [Maybe [List-of String]] is either a List-of Strings or #false
; [List-of [Maybe String]] is a List-of either Strings or #falses

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of the list los if it contains s
; #false otherwise.
; This means it takes a String and a List-of String, and then gives
; back either a List-of String or #false. According to the header,
; #false is if the List-of String does not contain the s String.

(check-expect (occurs "a" (list "b" "a" "d")) (list "d"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    [(empty? los) #f]
    [(string=? s (first los)) (rest los)]
    [else (occurs s (rest los))]))

; Exercise 230
; cons a primitive, f and '() are values
; f and f are both values
; f 10 and '() are values, cons a primitive

; Exercise 231
; you can apply x in the first position now
; same deal, and you can use a function as an argument
; both

; Exercise 232
; (Number -> Number) (Number -> Number) -> Boolean
(define (function=at1.2-3-and-5.775? f g)
  (and (= (f 1) (g 1))
       (= (f 2) (g 2))
       (= (f 3) (g 3))
       (= (f -5.775) (g -5.775))))

; Maybe if you do could a forall. But I don't think you can, as it
; is impractical to list all the possible inputs. I guess you can
; try by ensuring the the set of input types is the same, then
; randomizing.. The general implication would be that there are
; ideas that aren't easily definable as a function.

(extract squared>? (list 3 4 5) 10)

; Exercise 233
; I can see how the cons is retaining the 4 and 5..

; Exercise 234
(extract < (cons 6 (cons 4 '())) 5)

; Exercise 235
(extract < (cons 8 (cons 6 (cons 4 '()))) 5)