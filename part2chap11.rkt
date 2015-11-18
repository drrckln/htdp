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

; Exercise 165
; List-of-strings -> List-of-strings
; replaces "robot" with "r2d2"
(define (subst-robot alos)
  (cond
    [(empty? alos) '()]
    [else (cond
            [(string=? "robot" (first alos))
             (cons "r2d2" (subst-robot (rest alos)))]
            [else (cons (first alos) (subst-robot (rest alos)))])]))

; String String List-of-string -> List-of-string
; replaces old with new in alos
(define (substitute new old alos)
  (cond
    [(empty? alos) '()]
    [(string=? old (first alos))
     (cons new (substitute new old (rest alos)))]
    [else (cons (first alos) (substitute new old (rest alos)))]))

(define-struct work [employee rate hours])
; Work is a structure: (make-work String Number Number)
; interpretation (make-work n r h) combines the name (n)
; with the pay rate (r) and the number of hours (h) worked.

; Low (list of works) is one of:
; - '()
; - (cons Work Low)
; interpretation an instance of Low represents the hours worked
; of a number of employees

; (cons (make-work "John" 15.00 34) '())
; (cons (make-work "Bill" 20.00 15) (cons (make-work "John" 15.00 34) '()))

; Low -> List-of-numbers
; computes the weekly wages for all given weekly work records
(define (wage*.v2 an-low)
  (cond
    [(empty? an-low) '()]
    [else (cons (wage.v2 (first an-low))
                (wage*.v2 (rest an-low)))]))

(check-expect (wage*.v2 (cons (make-work "Robby" 11.95 39) '()))
              (cons (* 11.95 39) '()))

; Work -> ???
; a template for functions that process work structures
(define (for-work w)
  (... (work-employee w) ... (work-rate w) ... (work-hours w) ...))

; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

; Exercise 166
(define-struct pay-check [employee amount])

; Low -> List-of-pay-check
; consumes list of work records and produces a list of pay checks
(define (wage*.v3 an-low)
  (cond
    [(empty? an-low) '()]
    [else (cons (create-paycheck (first an-low))
                (wage*.v3 (rest an-low)))]))

; Work -> Paycheck
; creates a pay-check instance from a work record
(define (create-paycheck w)
  (make-pay-check (work-employee w)
                  (* (work-rate w) (work-hours w))))

(define-struct employee [name id])
; Employee is a structure: (make-employee String Number)
; interpretation (make-employee n id) combines the name (n)
; with the ID number assigned to him/her.

; Work is now a structure: (make-work Employee Number Number)
; interpretation (make-work e r h) combines the Employee (e)
; with the pay rate (r) and the number of hours (h) worked.

; Low -> List-of-pay-check
; consumes list of (revised) work records and produces a list of (revised) pay checks
(define (wage*.v4 an-low)
  (cond
    [(empty? an-low) '()]
    [else (cons (create-paycheck (first an-low))
                (wage*.v4 (rest an-low)))]))

; Exercise 167
; List-of-Posn -> Number
; produces the sum of all the x-coordinates
(define (sum lop)
  (cond
    [(empty? lop) '()]
    [else (+ (posn-x (first lop)) (sum (rest lop)))]))