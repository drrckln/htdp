;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define HOURLY 12)

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* HOURLY h))

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* alon)
  (cond
    [(empty? alon) '()]
    [else (cons (wage (first alon)) (wage* (rest alon)))]))

; Exercise 161
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons 336 '()))
(check-expect (wage* (cons 40 (cons 28 '())))
              (cons 480 (cons 336 '())))

; Exercise 162
(define (wage-limit h)
  (cond
    [(> h 100) (error "worked more than 100 hours??")]
    [else (* HOURLY h)]))

; Exercise 163
; Number -> Number
; converts list of Fahrenheit, alof, to list of Celsius
(define (convertFC alof)
  (cond
    [(empty? alof) '()]
    [else (cons (f->c (first alof)) (convertFC (rest alof)))]))

(check-expect (convertFC '()) '())
(check-expect (convertFC (cons -40 '())) (cons -40 '()))

; Number -> Number
; converts Fahrenheit to Celsius
(define (f->c n)
  (* 5/9 (- n 32)))

; Exercise 164
; List-of-number -> List-of-number
; converts list of USD, alod, to list of euros
(define (convert-euro alod)
  (cond
    [(empty? alod) '()]
    [else (cons (usd->eur (first alod)) (convert-euro (rest alod)))]))

; Number -> Number
; converts USD to Euro
(define (usd->eur n)
  (* 0.93 n))

; List-of-number Rate -> List-of-number
; Rate is a Number indicating exchange rate
; converts list of USD, alod, to list of euros using the exchange rate
(define (convert-euro* alod r)
  (cond
    [(empty? alod) '()]
    [else (cons (* r (first alod)) (convert-euro* alod r))]))