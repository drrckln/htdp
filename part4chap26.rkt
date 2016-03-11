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