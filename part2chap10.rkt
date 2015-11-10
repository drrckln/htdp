;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; by the first clause
'()
; by the second clause and the preceding example
(cons "a" '())
; again by the second clause and the preceding example
(cons "b" (cons "a" '()))

; List-of-strings -> Number
; count how many strings alos contains
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else (+ (how-many (rest alos)) 1)]))

(check-expect (how-many '()) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "b" (cons "a" '()))) 2)

; Exercise 138
; this is because they are using the same single data definition

; Exercise 139
; A List-of-amounts is one of:
; - '()  ^----------------------------------|
; - (cons PositiveNumber List-of-amounts) <-|
; interpretation a List-of-amounts represents some amounts of money
'()
(cons 10 '())
(cons 13 (cons 10 '()))

; List-of-amounts -> PositiveNumber
; computes the sum of the amounts in alom
(define (sum alom)
  (cond
    [(empty? alom) 0]
    [else (+ (first alom) (sum (rest alom)))]))

(check-expect (sum '()) 0)
(check-expect (sum (cons 10 '())) 10)
(check-expect (sum (cons 13 (cons 10 '()))) 23)

; Exercise 140
; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; determines whether all numbers in alon are positive numbers
(define (pos? alon)
  (cond
    [(empty? alon) #true]
    [else (and (> (first alon) 0)
               (pos? (rest alon)))]))

(check-expect (pos? '()) #true)
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons -3 '())) #false)
(check-expect (pos? (cons -3 (cons 5 '()))) #false)
(check-expect (pos? (cons 3 (cons 5 '()))) #true)

; List-of-numbers -> PositiveNumber or Error
; produces sum if alon is also a List-of-amounts, otherwise error
(define (checked-sum alon)
  (cond
    [(empty? alon) 0]
    [else (cond ; could just use if statement here
            [(pos? alon) (sum alon)]
            [else (error "there are nonpositive numbers")])]))

(check-expect (checked-sum '()) 0)
(check-error (checked-sum (cons -4 '())))
(check-expect (checked-sum (cons 4 '())) 4)
(check-expect (checked-sum (cons 5 (cons 4 '()))) 9)
(check-error (checked-sum (cons -5 (cons 4 '()))))

; sum computes, for an element of List-of-numbers, the sum of the numbers..

; Exercise 141
; List-of-boolean is one of:
; - '()
; - (cons Boolean List-of-boolean)

; List-of-boolean -> Boolean
; determines whether alob is all true
(define (all-true alob)
  (cond
    [(empty? alob) #true]
    [else (and (first alob)
               (all-true (rest alob)))]))

(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false (cons #true '()))) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)

; List-of-boolean -> Boolean
; determines whether alob has single #true value

(define (one-true alob)
  (cond
    [(empty? alob) #false]
    [else (or (first alob)
              (one-true (rest alob)))]))

(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)

; Exercise 142
(define (cat l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (cat (rest l)))]))

; Exercise 143
; ImageOrFalse is one of:
; - Image
; - #false

; List-of-images PositiveNumber -> ImageOrFalse
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (cond
            [(and (= (image-height (first loi)) n)
                  (= (image-width  (first loi)) n))
             (first loi)]
            [else (ill-sized? (rest loi) n)])]))

; A List-of-temperatures is one of:
; - '()
; - (cons CTemperature List-of-temperatures)

; A CTemperature is a Number greater or equal to -273.

; List-of-temperatures -> Number
; computes the average temperature
;(define (average alot)
;  (cond
;    [(empty? alot) ...]
;    [(cons? alot)
;     (... (first alot) ... (average (rest alot)) ...)]))
; this doesn't work though.. break into 3 functions!


; List-of-temperatures -> Number
; computes the average temperature
;(define (average alot)
;  (/ (sum alot)
;     (how-many alot)))

(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)

; List-of-temperatures -> Number
; adds up the temperatures on the given list
(define (nsum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

(check-expect (nsum (cons 1 (cons 2 (cons 3 '())))) 6)

; List-of-temperatures -> Number
; counts the temperatures on the given list
;(define (how-many alot) 0)

; Exercise 144
; List-of-temperatures -> Number
; computes the average temperature
(define (checked-average alot)
  (cond
    [(empty? alot) (error "list of temperatures is empty")]
    [else (/ (sum alot)
             (how-many alot))]))

(check-error (checked-average '()))
(check-expect (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)

; A NEList-of-temperatures is one of:
; - (cons CTemperature '())
; - (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of measured temperatures

(cons -273 '()) ; clause that does NOT use a self-reference

; NEList-of-temperatures -> Number
; computes the averages temperature

;(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)

; now this version just works
(define (average anelot)
  (/ (ne-sum anelot)
     (ne-how-many anelot)))

; Exercise 145
; sum would not work because NEList-of-temperatures would never reach the base-case of the empty list
; you'd have to rewrite what the base case iss. It becomes a partial function. Same problem w/ how-many.
; (sum (cons 3 (cons 2 '()))) => (+ 3 (+ 2 (sum '()))) => sum '() breaks

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures
(check-expect (ne-sum (cons 1 (cons 2 (cons 3 '())))) 6)
(define (ne-sum anelot)
  (cond
    [(empty? (rest anelot)) (first anelot)] ; doesn't include (rest anelot) because we know it's '()
    [(cons? (rest anelot))
     (+ (first anelot) (ne-sum (rest anelot)))]))

; Exercise 146
; NEList-of-temperatures -> Boolean
; produces #true if the temperatures are sorted in descending order
; eg second is smaller than first, third smaller than second, so on.
(define (sorted>? anelot)
  (cond
    [(empty? (rest anelot)) #true]
    [(cons? (rest anelot))
     (and (> (first anelot) (first (rest anelot)))
          (sorted>? (rest anelot)))]))

(check-expect (sorted>? (cons 2 '())) #true)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 2 (cons 3 '()))) #false)
(check-expect (sorted>? (cons 5 (cons 3 (cons 4 '())))) #false)
(check-expect (sorted>? (cons 5 (cons 4 (cons 3 '())))) #true)

; Exercise 147
; NEList-of-temperatures -> Number
; determines how many temperatures are in anelot
(define (ne-how-many anelot)
  (cond
    [(empty? (rest anelot)) 1]
    [(cons? (rest anelot))
     (+ 1 (ne-how-many (rest anelot)))]))

(check-expect (ne-how-many (cons 2 '())) 1)
(check-expect (ne-how-many (cons 3 (cons 2 '()))) 2)

; Exercise 148
; A NEList-of-Booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-Booleans)

; NEList-of-Booleans -> Boolean
; determines whether all are true
(define (ne-all-true anelob)
  (cond
    [(empty? (rest anelob)) (first anelob)]
    [(cons? (rest anelob))
     (and (first anelob)
          (ne-all-true (rest anelob)))]))

(check-expect (ne-all-true (cons #true '())) #true)
(check-expect (ne-all-true (cons #false '())) #false)
(check-expect (ne-all-true (cons #true (cons #true (cons #true '())))) #true)
(check-expect (ne-all-true (cons #false (cons #true (cons #true '())))) #false)
(check-expect (ne-all-true (cons #true (cons #false (cons #true '())))) #false)
(check-expect (ne-all-true (cons #true (cons #true (cons #false '())))) #false)

; NEList-of-Booleans -> Boolean
; determines whether one value is true
(define (ne-one-true anelob)
  (cond
    [(empty? (rest anelob)) (first anelob)]
    [(cons? (rest anelob))
     (or (first anelob)
         (ne-one-true (rest anelob)))]))

(check-expect (ne-one-true (cons #true '())) #true)
(check-expect (ne-one-true (cons #false '())) #false)
(check-expect (ne-one-true (cons #true (cons #true (cons #true '())))) #true)
(check-expect (ne-one-true (cons #false (cons #true (cons #true '())))) #true)
(check-expect (ne-one-true (cons #true (cons #false (cons #true '())))) #true)
(check-expect (ne-one-true (cons #true (cons #true (cons #false '())))) #true)
(check-expect (ne-one-true (cons #false (cons #false (cons #false '())))) #false)

; Exercise 149
; I'm not sure.. the NE versions of functions seem more complex, but you get the checking.
; Actually they're not really more complex, just more verbose due to different base case.
; It's probably better to use data definitions that have non-empty lists, depending on the
; type of functions you will be building on it.

; A N is one of:
; - 0
; - (add1 N)
; interpretation represents the natural numbers or counting numbers
; church numerals?
; add1 is like a constructor, and sub1 the "selector"
; then for each clause -- zero? positive?

; N String -> List-of-strings
; creates a list of n strings s

(check-expect (copier 2 "hello") (cons "hello" (cons "hello" '())))
(check-expect (copier 0 "hello") '())

(define (copier n s)
  (cond
    [(zero? n) '()]
    [(positive? n) (cons s (copier (sub1 n) s))]))

; Exercise 150
; IT DOES WORK

(define (copier.v2 n s)
  (cond
    [(zero? n) '()]
    [else (cons s (copier.v2 (sub1 n) s))]))

; copier says NOPE, all clauses exhausted
; copier.v2 goes on infinitely

; Exercise 151
; N -> Number
; computes (+ n pi) without using +

(check-within (add-to-pi 3) (+ 3 pi) 0.001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; the skeleton uses check-within since pi is irrational and this can cause imprecisions

; N Number -> Number
; adds a natural n to arbitrary number x, without +
(define (add n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add (sub1 n) x))]))

(check-within (add 10 pi) (+ 10 pi) 0.001)