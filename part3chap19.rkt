;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3chap19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 265
;(lambda (x y) (x y y)) ; legal if x is a function
;(lambda () 10) ; not legal, no variables
;(lambda (x) x) ; legal, just returns. identity.
;(lambda (x y) x) ; legal, like first
;(lambda x 10) ; not legal, syntax error

; Exercise 266
; 1. 3
; 2. 14
; 3. 13.25

; Exercise 267
#|
(lambda (n) (< n 10))
(lambda (x y) (number->string (* x y)))
(lambda (ir1 ir2) (< (ir-price ir1)
                     (ir-price ir2)))
(lambda (nat) (if (even? nat) 0 1))
(lambda (p im) (place-image (circle 3 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            im))
|#

; Exercise 268
(define (f-plain x) (* 10 x))

(define f-lambda (lambda (x)
                   (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

; Exercise 269
; yes it can

; Exercise 270

(map (lambda (x) (* 10 x))
     '(1 2 3))

(foldl (lambda (name rst) (string-append name ", " rst)) "etc."
       '("Matthew" "Robby"))

(define threshold 100)
;(define-struct ir [name price])
;(filter (lambda (ir) (<= (ir-price ir) threshold))
;        (list (make-ir "bear" 10) (make-ir "doll" 33)))

; Exercise 271
((lambda (x) x) (lambda (x) x))
; so.. you just get the original function
((lambda (x) (x x)) (lambda (x) x))
; identity applied twice
;((lambda (x) (x x)) (lambda (x) (x x)))
; infinite identities

; Exercise 272
; [List-of USD] -> [List-of Euro]
(define (convert-euro lod)
  (map (lambda (dollar) (* 1.22 dollar))
       lod))
; [List-of Fahrenheit] -> [List-of Celsius]
(define (convertFC lof)
  (map (lambda (f) (* 5/9 (- f 32))) lof))

; [List-of Posn] -> [List-of [List Number Number]]
(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p)))
       lop))

; Exercise 273
(define-struct ir [name desc acq rec])
; [List-of IR] -> [List-of IR]
; sorts by diff between the two prices
(define (ir-sorter loir)
  (cond
    [(empty? loir) '()]
    [else (insert (first loir) (ir-sorter (rest loir)))]))

; IR [List-of IR] -> [List-of IR]
; inserts IR into the sorted list
(define (insert ir loir)
  (cond
    [(empty? loir) (list ir)]
    [((lambda (x) (<= (abs (- (ir-acq ir) (ir-rec ir)))
                      (abs (- (ir-acq x) (ir-rec x)))))
      (first loir))
     (cons ir (loir))]
    [else (cons (first loir) (insert ir (rest loir)))]))

; Exercise 274
; Number [List-of IR] -> [List-of IR]
; produces list of all with acq price below ua
(define (eliminate-exp n loir)
  (filter (lambda (ir) (< (ir-acq ir) n)) loir))

; String [List-of IR] -> [List-of IR]
; removes all IR that have ty as the name
(define (recall ty loir)
  (filter (lambda (ir) (not (string=? ty (ir-name)))) loir))

; [List-of String] [List-of String] -> [List-of String]
; consumes two lists of names and selects all those from the
; second one that are also on the first
(define (selection l1 l2)
  (filter (lambda (name) (member? name l1)) l2))

; Exercise 275
(define n 5)
(build-list n (lambda (x) x))
(build-list n (lambda (x) (+ x 1)))
(build-list n (lambda (x) (/ 1 (expt 10 x))))
(build-list n (lambda (x) (* (+ x 1) 2)))
(define (identmatrix n)
  (build-list n (lambda (x) (build-list n (lambda (y)
                                            (cond
                                              [(= x y) 1]
                                              [else 0]))))))

; Exercise 276
; Name [List-of Name] -> [List-of Name]
; determines whether any of the names on the list are equal
; to or an extension of the name
(define (find-name name lon)
  (ormap (lambda (x) (cond
                       [(string=? name
                                  (substring 0 (string-length name)))
                        #true]
                       [else false]))
         lon))

; [List-of Name] -> Boolean
; checks that all names start with "a"
(define (check-start-a lon)
  (andmap (lambda (x) (if (string=? "a" (substring 0 1 x)) #true #false))
          lon))

; You should use andmap ("and" for conditions that hold for all)

; Exercise 277
; [List-of X] [List-of X] -> [List-of X]
; concatenates the two lists
(define (append-from-fold l1 l2)
  (foldr (lambda (x y) (cons x y)) l2 l1))

; Exercise 278
; (X -> Y) [List-of X] -> [List-of Y]
(define (map-via-fold f ls)
  (foldr (lambda (x y) (cons (f x) y)) '() ls))

#|
; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; sort alon0 according to cmp

(check-expect (sort-cmp '("a" "b" "c") string<?) '("a" "b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))

(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces a variant of alon sorted by cmp
          (define (isort alon) ...)

          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon) ...))
    (isort alon0)))

(check-satisfied (sort-cmp '("a" "b" "c") string<?) (sorted string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <) (sorted <))

; [X X -> Boolean] -> [ [List-of X] -> Boolean]
; produces a function that determines whether
; some list is sorted according to cmp
(define (sorted cmp)
  (lambda (l0)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
              (cond
                [(empty? (rest l)) #true]
                [else (and (cmp (first l) (second l))
                           (sorted/l (rest l)))])))
      (if (empty? l0) #true (sorted/l l0)))))

(check-expect [(sorted string<?) '("a" "b" "c")] #true)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #true)

; Exercise 279
; [X X -> Boolean] [NEList-of X] -> Boolean 
; determine whether l is sorted according to cmp
 
(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)
 
(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [(cmp (first l) (second l))
     (sorted? cmp (rest l))]
    [else #false]))

; yes, you could redefine sorted to use sorted?
; it doesn't consume cmp because the local expression
; has access to the variables in the scope of the function
; containing it.

; List-of-Numbers -> List-of-Numbers
; produces a sorted version of l
(define (sort-cmp/bad l)
  '(9 8 6 5 4 3 2 1 0))

; [List-of X] [X X -> Boolean] -> [ [List-of X] -> Boolean]
; is l0 sorted according to cmp
; are all items in list k members of l0
(define (sorted-variant-of k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k))))

(check-expect [(sorted-variant-of '(1 3 2) <) '(1 2 3)] #true)
(check-expect [(sorted-variant-of '(1 3 2) <) '(1 3)] #false)

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(2 1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(2 1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (item-in-k) (member? item-in-k l)) k))

(define (sorted-variant-of.v2 k cmp)
  (lambda (l0)
    (and (sorted? cmp l0)
         (contains? l0 k)
         (contains? k l0))))

|#
; Exercise 280
; X [List-of X] -> [Maybe [List-of X]]
; produces the first sublist of l that starts with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x) l (find x (rest l)))]))

; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
(define (found? x k)
  (lambda (l0)
    (cond
      [(boolean? l0) (not (ormap (lambda (y) (equal? (first y) x)) k))]
      [else (member? l0 k)])))

(check-satisfied (find 10 (list (list 3 4)
                                (list 7 8 3)
                                (list 2 7 1 0)))
                 (found? 10 (list (list 3 4)
                                  (list 7 8 3)
                                  (list 2 7 1 0))))

; Exercise 281
; X [List-of X] -> [Maybe N]
; determine the (0-based) index of the first occurrence of x in l, 
; #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> [[Maybe N] -> Boolean]
(define (is-index? x k)
  (lambda (maybe-n)
    (cond
      [(boolean? maybe-n) (not (member? x k))]
      [else (and (equal? x (list-select maybe-n k))
                 (first-occurrence? x maybe-n k))])))

; N [List-of X] -> Maybe X
(define (list-select n l)
  (cond
    [(empty? l) #false]
    [(= n 0) (first l)]
    [else (list-select (sub1 n) (rest l))]))

; X N [List-of X] -> Boolean
; checks that N is indeed where the first occurrence of x is
(define (first-occurrence? x i l)
  (local (; N [List-of X]
          ; cuts off starting from i
          (define (cut-off n l0)
            (cond
              [(= n 0) '()]
              [else (cons (first l0) (cut-off (sub1 n) (rest l0)))]))
          ; the list before i index
          (define before-list (cut-off i l)))
    (not (member? x before-list))))

(check-satisfied (index 3 (list 5 8 0 4 2 3 9 8 2))
                 (is-index? 3 (list 5 8 0 4 2 3 9 8 2)))

; Exercise 282
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generate n random Posns in a WIDTH by HEIGHT rectangle
(check-satisfied (random-posns 3) (n-inside-playground? 3))
 
(define (random-posns n)
  (build-list n (lambda (i) (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [[List-of Posn] -> Boolean]
(define (n-inside-playground? k)
  (lambda (lop)
    (and (= (length lop) k)
         (andmap (lambda (p) (and (<= 0 (posn-x p) WIDTH)
                                  (<= 0 (posn-y p) HEIGHT)))
                 lop))))

; This is an incomplete specification because it doesn't check
; that the Posns are random

; N -> [List-of Posn]
(define (random-posns/bad n)
  (build-list n (lambda (i) (make-posn (/ WIDTH 2) (/ HEIGHT 2)))))

(check-satisfied (random-posns/bad 3) (n-inside-playground? 3))

; Shape is a function:
; [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p) produces
; #true if the given Posn is inside of s, #false otherwise

; Shape Posn -> Boolean
; this takes the Shape s and the Posn p to be tested, and applies
; (s p). This works because the Shape type IS a function w/ the
; set of Posns as the domain (input type)
(define (inside? s p)
  (s p))

; Posn -> Boolean
; this example shape is simply the point (3, 4)
(lambda (p) (and (= (posn-x p) 3)
                 (= (posn-y p) 4)))

; Number Number -> Shape
; represents a point at (x,y)
(define (make-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))

(define a-sample-shape (make-point 3 4))

(check-expect (inside? (make-point 3 4) (make-posn 3 4)) #true)
(check-expect (inside? (make-point 3 4) (make-posn 3 -4)) #false)

; Number Number Number -> Shape
; creates a data representation for a circle of radius r
; located at (center-x, center-y)
(define (make-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

(check-expect (inside? (make-circle 3 4 5) (make-posn 0 0)) #true) ; exactly 5 away
(check-expect (inside? (make-circle 3 4 5) (make-posn 0 -1)) #false) ; too far
(check-expect (inside? (make-circle 3 4 5) (make-posn -1 3)) #true) ; close enough

; Exercise 283
; drawn

; Exercise 284
; Number Number Posn -> Number
; takes an x and y, then finds distance to the Posn p given
(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))