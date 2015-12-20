;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname intermezzo_quote) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 217
(list 1 "a" 2 #false 3 "c")
(list)

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl" 1500)
      (list "dawn" 2300))

(define x 42)


; String String -> ... deeply nested list ...
; produces a (representation of) a web page with given author and title
(define (my-first-web-page author title)
  `(html
    (head
     (title ,title)
     (meta ((http-equiv "content-type")
            (content "text-html"))))
    (body
     (h1 ,title)
     (p "I, " ,author ", made this page."))))


; Exercise 218

(list 1 "a" 2 #false 3 "c")

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl , the great" 1500)
      (list "dawn" 2300))

(list 'html
      (list 'head (list 'title "ratings"))
      (list 'body (list 'h1 "ratings")
            (list 'p "A second web page")))


; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from a list of numbers
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  (cond
    [(string? n) `(td ,n)]
    [(number? n) `(td ,(number->string n))]))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

; Exercise 219
`(0 ,@'(1 2 3) 4)
(list 0 1 2 3 4)

`(("alan" ,(* 2 500))
  ("barb" 2000)
  (,@'(list "carl" " , the great")   1500)
  ("dawn" 2300))

(list (list "alan" 1000)
      (list "barb" 2000)
      (list 'list "carl" " , the great" 1500)
      (list "dawn" 2300))

`(html
  (body
   (table ((border "1"))
          (tr ((width "200")) ,@(make-row '( 1 2)))
          (tr ((width "200")) ,@(make-row '(99 65))))))

(list `html
      (list `body
            (list `table (list (list `border "1"))
                  (list `tr (list (list `width "200")) (list 'td "1") (list 'td "2"))
                  (list `tr (list (list `width "200")) (list 'td "99") (list 'td "65")))))

; Exercise 220
; List-of-String -> ... list representation of an HTML table
(define (make-ranking lost)
  `(table ((border "1"))
          ,@(make-rows (ranking lost))))

; List-of-String -> List-of-Rows
(define (make-rows los)
  (cond
    [(empty? los) '()]
    [else (cons `(tr ,@(make-row (first los))) (make-rows (rest los)))]))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-String -> List-of-String
; adds the ranking in
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-String -> List-of-List-of-Num-String
; turns each string into a list with a number as the first the string as the second
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))