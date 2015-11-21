;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname part2chap11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
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

; Exercise 168
; List-of-Posn -> List-of-Posn
; translates each posn up by 1
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else (cons (move-up (first lop))
                (translate (rest lop)) ...)]))

; Posn -> Posn
; moves p up the y axis by 1
(define (move-up p)
  (make-posn (posn-x p) (+ (posn-y p) 1)))

; Exercise 169
; List-of-Posn -> List-of-Posn
; filters for only 0 <= x <= 100
; and 0 <= y <= 200
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (cond
                   [(legal? (first lop)) (cons (first lop) (legal (rest lop)))]
                   [else (legal (rest lop))])]))

(check-expect (legal '()) '())
(check-expect (legal (cons (make-posn 30 150) '()))
              (cons (make-posn 30 150) '()))
(check-expect (legal (cons (make-posn 30 150) (cons (make-posn 150 100) '())))
              (cons (make-posn 30 150) '()))

; Posn -> Boolean
; checks if posn p satisfies
; 0 <= x <= 100
; 0 <= y <= 200
(define (legal? p)
  (and (<= 0 (posn-x p) 100)
       (<= 0 (posn-y p) 200)))

; Exercise 170
(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is between 100 and 999
; A Four is between 1000 and 9999

; List-of-Phone -> List-of-Phone
; replaces all occurrence of area code 713 with 281
(define (replace loph)
  (cond
    [(empty? loph) '()]
    [(cons? loph) (cond
                    [(= 713 (phone-area (first loph)))
                     (cons (make-281-phone (first loph))
                           (replace (rest loph)))]
                    [else (cons (first loph) (replace (rest loph)))])]))

(check-expect (replace '()) '())
(check-expect (replace (cons (make-phone 248 355 9793) '()))
              (cons (make-phone 248 355 9793) '()))
(check-expect (replace (cons (make-phone 713 282 3445) (cons (make-phone 248 355 9793) '())))
              (cons (make-phone 281 282 3445) (cons (make-phone 248 355 9793) '())))

; Phone -> Phone
; changes area code to 281
(define (make-281-phone p)
  (make-phone 281 (phone-switch p) (phone-four p)))

; Exercise 171
; A List-of-String is one of:
; - '()
; - (cons String List-of-String)

(cons "TTT"
      (cons ""
            (cons "Put up in a place"
                  (cons "where it's easy to see"
                        (cons "the cryptic admonishment"
                              (cons "T.T.T."
                                    (cons ""
                                          (cons "When you feel how depressingly"
                                                (cons "slowly you climb"
                                                      (cons "it's well to remember that"
                                                            (cons "Things Take Time."
                                                                  (cons ""
                                                                        (cons "Piet Hein" '())))))))))))))

(read-lines "ttt.txt")

(cons "TTT" (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" (cons "where" (cons "it's" (cons "easy"
      (cons "to" (cons "see" (cons "the" (cons "cryptic" (cons "admonishment" (cons "T.T.T." (cons "When"
      (cons "you" (cons "feel" (cons "how" (cons "depressingly" (cons "slowly" (cons "you" (cons "climb"
      (cons "it's" (cons "well" (cons "to" (cons "remember" (cons "that" (cons "Things" (cons "Take" (cons "Time."
      (cons "Piet" (cons "Hein" '())))))))))))))))))))))))))))))))))

(read-words "ttt.txt")

; List-of-List-of-String is one of:
; - '()
; - (cons List-of-String List-of-List-of-String)

(cons (cons "TTT" '())
      (cons '()
            (cons (cons "Put" (cons "up" (cons "in" (cons "a" (cons "place" '())))))
                  (cons (cons "where" (cons "it's" (cons "easy" (cons "to" (cons "see" '())))))
                        (cons (cons "the" (cons "cryptic" (cons "admonishment" '())))
                              (cons (cons "T.T.T." '())
                                    (cons '()
                                          (cons (cons "When" (cons "you" (cons "feel" (cons "how" (cons "depressingly" '())))))
                                                (cons (cons "slowly" (cons "you" (cons "climb" '())))
                                                      (cons (cons "it's" (cons "well" (cons "to" (cons "remember" (cons "that" '())))))
                                                            (cons (cons "Things" (cons "Take" (cons "Time." '())))
                                                                  (cons '()
                                                                        (cons (cons "Piet" (cons "Hein" '())) '())))))))))))))

(read-words/line "ttt.txt")

; LLS -> List-of-Numbers
; determines the number of words on each line
(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else
     (cons (words# (first lls)) ; a list of strings
     (words-on-line (rest lls)))]))

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())

(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1) (cons 2 (cons 0 '())))

(define (words# ln)
  (cond
    [(empty? ln) 0]
    [else (+ 1 (words# (rest ln)))]))
; this is just "length"

; String -> List-of-numbers
; counts the number of words  on each line in the given file
(define (file-statistic file-name)
  (words-on-line
   (read-words/line file-name)))

; Exercise 172
; LLS -> String
; converts a list of lines to a string 
(define (lls->string lls)
  (cond
    [(empty? lls) ""]
    [(cons? lls) (cond
                   [(= (length (rest lls)) 0) (l->string (first lls))]
                   [else (string-append (l->string (first lls)) "\n" (lls->string (rest lls)))])]))

; List-of-string -> String
; converts a line, ln, to a String
(define (l->string ln)
  (cond
    [(empty? ln) ""]
    [(cons? ln) (cond
                  [(= (length (rest ln)) 0) (first ln)]
                  [else (string-append (first ln) " " (l->string (rest ln)))])]))

; Exercise 173
; File-Name -> File
(define (messwith n)
  (write-file (string-append "no-articles-" n) (lls->string (filter-articles (read-words/line n)))))

; LLS -> LLS
; filters articles from a list of list of strings
(define (filter-articles lls)
  (cond
    [(empty? lls) '()]
    [(cons? lls) (cons (rem-articles(first lls)) (filter-articles (rest lls)))]))

; List-of-String -> List-of-String
; removes articles from ln
(define (rem-articles ln)
  (cond
    [(empty? ln) '()]
    [(cons? ln) (cond
                  [(or (string=? (first ln) "a")
                       (string=? (first ln) "an")
                       (string=? (first ln) "the"))
                   (rem-articles (rest ln))]
                  [else (cons (first ln) (rem-articles (rest ln)))])]))

; Exercise 174

; 1String -> String
; converts the given 1String into a three-letter numeric string

; 1String -> String
; auxiliary for stating tests
(define (code1 c)
  (number->string (string->int c)))

(check-expect (encode-letter "\t") (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a") (string-append "0" (code1 "a")))
(check-expect (encode-letter "z") (code1 "z"))

; converts the letter into a three-letter numeric string, padding zeros as necessary
(define (encode-letter s)
  (cond
    [(< (string->int s) 10) (string-append "00" (code1 s))]
    [(< (string->int s) 100) (string-append "0" (code1 s))]
    [else (code1 s)]))


; LLS -> LLS
; maps encode-letter onto lls
(define (map-encode-letter-3 lls)
  (cond
    [(empty? lls) '()]
    [(cons? lls) (cons (map-encode-letter-2 (first lls)) (map-encode-letter-3 (rest lls)))]))

; List-of-String -> List-of-String
; maps encode-letter onto los
(define (map-encode-letter-2 los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (map-encode-letter (first los)) (map-encode-letter-2 (rest los)))]))

; String -> String
; maps encode-letter onto a string
(define (map-encode-letter s)
  (cond
    [(= (string-length s) 1) (encode-letter s)]
    [else (string-append (encode-letter (substring s 0 1)) (map-encode-letter (substring s 1)))]))

; File-Name -> LLS
; encodes a lls numerically
(define (encode-lls n)
  (map-encode-letter-3 (read-words/line n)))

; Exercise 175
(define-struct output [1strings words lines])

; File-Name -> Output
(define (wc n)
  (make-output (count-1strings (read-words/line n))
               (count-words (read-words/line n))
               (count-lines (read-words/line n))))

; LLS -> Number
; counts number of lines in lls
(define (count-lines lls)
  (length lls))

; LLS -> Number
; counts number of words in lls
(define (count-words lls)
  (cond
    [(empty? lls) 0]
    [else (+ (length (first lls))
             (count-words (rest lls)))]))

; LLS -> Number
; counts number of 1Strings in lls
(define (count-1strings lls)
  (cond
    [(empty? lls) 0]
    [else (+ (line-1-strings (first lls))
             (count-1strings (rest lls)))]))

; List-of-Strings -> Number
; counts number of 1strings in a line
(define (line-1-strings ln)
  (cond
    [(empty? ln) 0]
    [else (+ (string-length (first ln))
             (line-1-strings (rest ln)))]))

; Exercise 176
; A Matrix is one of:
; - (cons Row '())
; - (cons Row Matrix)
; constraint all rows in matrix are of the same length

; An Row is one of:
; - '()
; - (cons Number Row)

; 11, 12, 21, 22 should be..
; (cons (cons 11 (cons 12 '()))
;       (cons (cons 21 (cons 22 '())) '()))

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

; Matrix -> Matrix
; transpose the items on the given matrix along the diagonal

(check-expect (transpose mat1) tam1)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()] ; if first row is empty then rest of rows must be empty, due to the constraint on matrices 
    [(cons? lln) (cons (first* lln) (transpose (rest* lln)))]))

; I can't do this because.. I guess.. rest* would throw away info that is required for
; generating the other lines of the transposed matrix
; ^- well this is wrong.. but how would I have written first* and rest*???
; I guess examples would help
; I couldn't have designed transpose because it says nothing about how to choose (empty? (first lln))
; which comes down to reading the constraint and selecting a base case

; Matrix -> List-of-numbers
; produces the first column?
(define (first* lon)
  (cond
    [(empty? lon) '()]
    [else (cons (first (first lon))
                (first* (rest lon)))]))

(check-expect (first* mat1) wor1)

; Matrix -> Matrix
(define (rest* lon)
  (cond
    [(empty? lon) '()]
    [else (cons (rest (first lon))
                (rest* (rest lon)))]))