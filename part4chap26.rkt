;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap26) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; constructs a new list by replacing '() in front with end
(define (replace-eol-with front end)
  (cond
    [(empty? front) ..]
    [else (... (first front) ...
           ... (replace-eol-with (rest front) end) ...)]))

(check-expect (replace-eol-with '() '(a b c)) '(a b c))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))
