;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 130

(cons "Mercury" (cons "Venus" (cons "Earth" (cons "Mars" (cons "Jupiter" (cons "Saturn" (cons "Uranus" (cons "Neptune" '()))))))))

(cons "steak"
      (cons "french fries"
            (cons "beans"
                  (cons "bread"
                        (cons "water"
                              (cons "brie cheese"
                                    (cons "ice cream" '())))))))

(cons "red"
      (cons "orange"
            (cons "yellow"
                  (cons "green"
                        (cons "blue"
                              (cons "indigo"
                                    (cons "violet" '())))))))

; A 3LON is (cons Number (cons Number (cons Number '())))
; interpretation (cons 1 (cons -1 (cons -2 '()))) represents a point
; in a three-dimensional space

; A List-of-names is one of:
; - '()
; - (cons String List-of-Names)
; interpretation a List-of-names represents a list of invitees by last name

; Exercise 131
(cons "Flatt"
      (cons "Felleisen"
            (cons "Findler"
                  (cons "Krishnamurthi"
                        (cons "Lin"
                              '())))))
; (cons 2 '()) is not, because the element being cons-ed onto '() isn't a String but a Number

; Exercise 132
; A List-of-Boolean is one of:
; - '()
; - (cons Boolean List-of-Boolean)

(define-struct pair [left right])
; A ConsPair is (make-pair Any Any).

; Any Any -> ConsPair
;(define (our-cons a-value a-list)
;  (make-pair a-value a-list))

; A ConsOrEmpty is one of:
; - '()
; - (cons Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all BSL lists

; Any ConsOrEmpty -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: list as second argument expected")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the right part of the given pair
(define (our-rest a-list)
  (cond
    [(empty? a-list) (error 'our-rest "...")]
    [else (pair-right a-list)]))

; List-of-names -> Boolean
; determines whether "Flatt" occurs on a-list-of-names
(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) #false]
    [(cons? a-list-of-names)
     (or (string=? (first a-list-of-names) "Flatt")
         (contains-flatt? (rest a-list-of-names)))]))

(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Findler" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect
 (contains-flatt? (cons "Mur" (cons "Fish" (cons "Find" '()))))
 #false)
(check-expect
 (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)

; Exercise 133
; I expect #true, and it is indeed true.

; Exercise 134
; This is the version I expected before. I am not sure which is better..
; this one may be easier to read as a tree of possibilities. Efficiency-wise
; it should be the same, but only because "or" shortcuts. If it didn't the
; (cond version is superior as it exits at the first opportunity.

; Exercise 135
; String List-of-names -> Boolean
(define (contains? s a-list-of-names)
  (cond
    [(empty? a-list-of-names) #false]
    [else (cond
            [(string=? (first a-list-of-names) s) #true]
            [else (contains? s (rest a-list-of-names))])]))

; Exercise 136
(contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
; it goes to the end, you get to an empty list and you or a bunch of #fs, for #f

; Exercise 137
(our-first (our-cons "a" '())) ; == "a", yes
(our-rest (our-cons "a" '())) ; == '(), not "a"!

