;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap26) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; constructs a new list by replacing '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else (cons (first front)
                (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

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