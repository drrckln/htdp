;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct no-parent [])
(define MTFT (make-no-parent))
(define-struct child [father mother name date eyes])
; A FT (family tree) is one of:
; - MTFT
; - (make-child FT FT String N String)

(make-child MTFT MTFT "Carl" 1926 "green")
(make-child (make-child MTFT MTFT "Carl" 1926 "green")
            (make-child MTFT MTFT "Bettina" 1926 "green")
            "Adam"
            1950
            "hazel")

; Oldest Generation:
(define Carl (make-child MTFT MTFT "Carl" 1926 "green"))
(define Bettina (make-child MTFT MTFT "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child MTFT MTFT "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

#| Template
; FT -> ???
; ...
(define (fun-for-ft a-ftree)
  (cond
    [(no-parent? a-ftree) ...]
    [else ; (child? a-ftree)
     (... (fun-for-ft (child-father a-ftree)) ...
      ... (fun-for-ft (child-mother a-ftree)) ...
      ... (child-name a-ftree) ...
      ... (child-date a-ftree) ...
      ... (child-eyes a-ftree) ...)]))
|#

; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else
      (or (string=? (child-eyes a-ftree) "blue")
          (blue-eyed-child? (child-father a-ftree))
          (blue-eyed-child? (child-mother a-ftree)))]))

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)

; Exercise 296
; FT -> Number
; counts the child structures in a-ft
(define (count-persons a-ft)
  (cond
    [(no-parent? a-ft) 0]
    [else (+ 1
             (count-persons (child-father a-ft))
             (count-persons (child-mother a-ft)))]))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Dave) 3)
(check-expect (count-persons Gustav) 5)

; Exercise 297
; FT -> Number
; produces the average age of all child structures in the family tree
(define (average-age ft)
  (/ (sum-ages ft 2016)
     (count-persons ft)))

(check-expect (average-age Carl) (- 2016 1926))
(check-expect (average-age Eva) (/ (+ (- 2016 1965)
                                      (- 2016 1926)
                                      (- 2016 1926))
                                   3))

; FT Number -> Number
; sums the ages of all child structures in the family tree
(define (sum-ages ft current-year)
  (cond
    [(no-parent? ft) 0]
    [else ; (child? a-ftree)
     (+ (sum-ages (child-father ft) current-year)
        (sum-ages (child-mother ft) current-year)
        (- current-year (child-date ft)))]))

(check-expect (sum-ages Carl 2013) (- 2013 1926))
(check-expect (sum-ages Eva 2016) (+ (- 2016 1965)
                                     (- 2016 1926)
                                     (- 2016 1926)))

; Exercise 298
; FT -> [List-of Color]
; produces a list of all eye colors in the tree
(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [else ; (child? a-ftree)
     (append (list (child-eyes ft))
             (eye-colors (child-father ft))
             (eye-colors (child-mother ft)))]))