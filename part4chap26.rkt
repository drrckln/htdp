;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap26) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; constructs a new list by replacing '() in front with end
#|
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
|#

; Exercise 372
; [List-of Symbol] [List-of Number] -> [List-of (Symbol, Number)]
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [(empty? lon) '()]
    [(empty? (rest los)) (cons (list (first los) (first lon))
                               (cross (list (first los)) (rest lon)))]
    [else (replace-eol-with (cross (list (first los)) lon)
                            (cross (rest los) lon))]))
    
(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(check-expect (cross '() '(1 2))
              '())
(check-expect (cross '(a b c) '())
              '())
(check-expect (cross '(a) '(1 2))
              '((a 1) (a 2)))

; Exercise 373
(define-struct employee [name ssn pay-rate])
(define-struct wr [name hours]) ; work record
(define-struct output [name pay]) ; output record

; [List-of Employee] [List-of WR] -> [List-of Output]
; computes weekly wages by multiplying the corresponding
; items on hours and hourly-wages
; assume the two lists are of equal length
(define (wages*.v2 ems wrs)
  (cond
    [(empty? ems) '()]
    [else
     (cons (make-output (employee-name (first ems))
                    (weekly-wage (employee-pay-rate (first ems))
                                 (wr-hours (first wrs))))
           (wages*.v2 (rest ems) (rest wrs)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours-worked
(define (weekly-wage pay-rate hours-worked)
  (* pay-rate hours-worked))

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list (make-employee "jim" 3838383 5.65)) (list (make-wr "jim" 40)))
              (list (make-output "jim" 226.0)))
;(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0)) '(226.0 262.5))

; to compute the wage of one worker, use weekly-wage
; to deal with income taxes, change weekly-wage

; Exercise 374
(define-struct phone-record [name number])
; A PhoneRecord is (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; takes list of names and list of phone numbers, creating phone records
; lists are equally long
; assumption: corresponding list items belong to the same person
(define (zip names phones)
  (cond
    [(empty? names) '()]
    [else (cons (make-phone-record (first names) (first phones))
                (zip (rest names) (rest phones)))]))

; N is one of:
; - 0
; - (add1 N)

; [List-of Symbol] N -> Maybe Symbol
; extracts the nth symbol from l
; signals an error if there is no such symbol
(define (list-pick l n)
  (cond
    [(empty? l) (error "list too short")]
    [(= n 0) (first l)]
    [(> n 0) (list-pick (rest l) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")
(check-error (list-pick (cons 'a '()) 3) "list too short")

; Exercise 375
(define-struct branch [left right])
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

; TOS [List-of Direction] -> Maybe Symbol
#|
(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod))
     tos]
    [(and (branch? tos) (empty? lod))
     (error "not a leaf")]
    [(and (symbol? tos) (cons? lod))
     (error "no more depth")]
    [(and (branch? tos) (cons? lod))
     (cond
       [(symbol=? 'left (first lod)) (tree-pick (branch-left tos) (rest lod))]
       [else (tree-pick (branch-right tos) (rest lod))])]))
|#

(check-expect (tree-pick 'a '()) 'a)
(check-expect (tree-pick (make-branch 'a 'b) '()) (make-branch 'a 'b))
(check-error (tree-pick 'a (cons 'left '())) "no more depth")
(check-expect (tree-pick (make-branch 'a 'b) (cons 'left '())) 'a)

; it is important to understand that we designed the original in a systematic
; manner and that we were able to transform the first into the second with
; well-established algebraic laws.
; If we try to find the simple versions of functions directly, we sooner or
; later fail to take care of a case in our analysis, and we are guaranteed
; to produce flawed programs.

; Exercise 376
; [List-of Number] [List-of Number] -> [List-of Number]
; constructs a new list by replacing '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front) (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

; Exercise 377
; TOS [List-of Direction] -> Maybe Symbol
(define (tree-pick tos lod)
  (cond
    [(empty? lod) tos]
    [(symbol? tos) (error "no more depth")]
    [(symbol=? 'left (first lod)) (tree-pick (branch-left tos) (rest lod))]
    [else (tree-pick (branch-right tos) (rest lod))]))

; Exercise 378
; [List-of Number] [List-of Number] -> [List-of Number]
; two input lists are sorted in ascending order
; produces single sorted list of all numbers in the input lists
(define (merge l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (cons? l1) (empty? l2)) l1]
    [(and (empty? l1) (cons? l2)) l2]
    [(<= (first l1) (first l2))
     (cons (first l1) (merge (rest l1) l2))]
    [else (cons (first l2) (merge l1 (rest l2)))]))

(check-expect (merge '() '()) '())
(check-expect (merge (list 3 4) '()) (list 3 4))
(check-expect (merge '() (list 3 4)) (list 3 4))
(check-expect (merge (list 3 5 6) (list 2 4 9)) (list 2 3 4 5 6 9))

; Exercise 379
; [List-of X] Number -> [List-of X]
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(= n 0) l]
    [(> n 0) (drop (rest l) (sub1 n))]))

(check-expect (drop (list 3 2 4) 0) (list 3 2 4))
(check-expect (drop (list 3 2 4) 1) (list 2 4))
(check-expect (drop (list 3 2 4) 4) '())
(check-expect (drop '() 0) '())
(check-expect (drop '() 3) '())

(define (take l n)
  (cond
    [(or (= n 0) (empty? l)) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

(check-expect (take (list 3 2 4) 0) '())
(check-expect (take (list 3 2 4) 1) (list 3))
(check-expect (take (list 3 2 4) 4) (list 3 2 4))
(check-expect (take '() 0) '())
(check-expect (take '() 3) '())

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; A HM-Word is [List-of [Maybe Letter]]
; interpretation #false represents a letter to be guessed 
; A Letter is member? of LETTERS.

; HM-Word N -> String
; run a simplistic Hangman game, produce the current state of the game
; assume the-pick does not contain #false
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) #false))
 
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
 
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
 
    ; the state of the game is a HM-Word
 
    (implode
     (big-bang the-guess
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
; render the word, using "_" for places that are #false
(define (render-word w)
  (local ((define l (map (lambda (lt) (if (boolean? lt) "_" lt)) w))
          (define s (implode l)))
    (text s 22 "black")))

; Exercise 380
; [List-of Letter] HM-Word KeyEvent -> HM-Word
(define (compare-word word status guess)
  (local (; HM-Word -> HM-Word
          (define (update status)
            (updating word status guess))
          ; [List-of Letter] HM-Word KeyEvent -> HM-Word
          (define (updating w s g)
            (cond
              [(empty? s) '()]
              [(not (boolean? (first s)))
               (cons (first s) (updating (rest w) (rest s) g))]
              [(string=? g (first w))
               (cons g (updating (rest w) (rest s) g))]
              [else (cons #false (updating (rest w) (rest s) g))])))
    ; -- IN --    
    (cond
      [(and (member? guess word)
            (not (member? guess status)))
       (update status)]
      [else status])))

(define DICTIONARY-LOCATION "/usr/share/dict/words") ; on Mac OS X
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define DICTIONARY-SIZE (length DICTIONARY-AS-LIST))
 
(play (list-ref DICTIONARY-AS-LIST (random DICTIONARY-SIZE)) 20)

; Exercise 381
; [List-of ER] [List-of PR] -> [List of WR]
; signals an error if it cannot find an employee record for a punch card
; or vice versa
; assumes at most one punch card record per employee number, vice versa
(define-struct PR [emp hours])
(define-struct ER [name num rate])
(define-struct WR [name weekwage])

(define (wages*.v3 loer lopr)
  (cond
    [(and (empty? lopr) (empty? loer)) '()]
    [(or (empty? lopr) (empty? loer)) (error "missing records")]
    [(= (ER-num loer) (PR-emp lopr))
     (cons (make-wr (ER-name loer) (* (ER-rate loer) (PR-hours lopr)))
           (wages*.v3 (rest loer) (rest lopr)))]
    [else (wages*.v3 loer (reverse (cons (first lopr) (reverse (rest lopr)))))]))

; Exercise 382
(list 5)
(list 5 17)
(list 5 17 3)

; LinearCombination [List-of Variable] -> ValueOfCombination
; input lists must be equally long
(define (value lincomb lovar)
  (cond
    [(empty? lincomb) 0]
    [else (+ (* (first lincomb)
                (first lovar))
             (value (rest lincomb) (rest lovar)))]))

(check-expect (value (list 5) (list 10)) 50)
(check-expect (value (list 5 17) (list 10 1)) 67)
(check-expect (value (list 5 17 3) (list 10 1 2)) 73)

; Exercise 383
; [List-of String] -> [List-of String] 
; picks a “random” non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of the given list of names
; see exercise 199
(define (arrangements names)
  ...)

; [List-of X] -> X 
; returns a random item from the list 
; assume the list is not empty 
(define (random-pick l)
  (local (; N [List-of X] -> X
          ; picks the nth item
          (define (pick n ls)
            (cond
              [(<= n 0) (first ls)]
              [else (pick (sub1 n) (rest ls))])))
    ; -- IN --
    (pick (random (length l)) l)))
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do not agree 
; with names at any place 
(define (non-same names ll)
  (local (; [List-of String] [List-of String] -> Boolean
          ; checks if any names are the same in the same place
          (define (check-names nm poss-nm)
            (cond
              [(empty? nm) #true]
              [(string=? (first nm) (first poss-nm))
               #false]
              [else (check-names (rest nm) (poss-nm))]))
          ; [List-of String] -> Boolean
          ; uses check-names, just partially applying names
          (define (checker candidate)
            (check-names names candidate)))
    ; -- IN --
    (filter checker ll)))

; Exercise 384
; [List-of ACGT] [List-of ACGT] -> Boolean
; if pattern is identical to initial part of search-string
; #true. otherwise #false.
(define (DNAprefix pattern ss)
  (cond
    [(empty? pattern) #true]
    [(string=? (first pattern) (first ss))
     (DNAprefix (rest pattern) (rest ss))]
    [else #false]))

; [List-of ACGT] [List-of ACGT] -> Maybe ACGT
; like DNAprefix, but returns first item in search-string
(define (DNAprefix pattern ss)
  (cond
    [(empty? pattern) (if (empty? ss)
                          (error "no DNA letter after")
                          (first ss))]
    [(string=? (first pattern) (first ss))
     (DNAprefix (rest pattern) (rest ss))]
    [else #false]))