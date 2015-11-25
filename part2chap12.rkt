;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 181
(check-expect (list "a" "b" "c" "d" "e")
              (cons "a" (cons "b" (cons "c" (cons "d" (cons "e" '()))))))
(check-expect (list (list 1 2))
              (cons (cons 1 (cons 2 '())) '()))
(check-expect (list "a" (list 1) #false)
              (cons "a" (cons (cons 1 '()) (cons #false '()))))
(check-expect (list (list 1 2) (list 2))
              (cons (cons 1 (cons 2 '())) (cons (cons 2 '()) '())))
(check-expect (list (list "a" 2) "hello")
              (cons (cons "a" (cons 2 '())) (cons "hello" '())))

; Exercise 182
(check-expect (list 0 1 2 3 4 5)
              (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(check-expect (list (list "adam" 0) (list "eve" 1) (list "louisXIV" 2))
              (cons (cons "adam" (cons 0 '()))
                    (cons (cons "eve" (cons 1 '()))
                          (cons (cons "louisXIV" (cons 2 '()))
                                '()))))
(check-expect (list 1 (list 1 2) (list 1 2 3))
              (cons 1 (cons (cons 1 (cons 2 '()))
                            (cons (cons 1 (cons 2 (cons 3 '())))
                                  '()))))

; Exercise 183
(check-expect (cons "a" (list 0 #false))
              (cons "a"
                    (cons 0 (cons #false '()))))
(check-expect (list (cons 1 (cons 13 '())))
              (cons (cons 1 (cons 13 '())) '()))
(check-expect (cons (list 1 (list 13 '())) '())
              (cons (cons 1
                          (cons (cons 13 (cons '() '()))
                                '()))
                    '()))
(check-expect (list '() '() (cons 1 '()))
              (cons '() (cons '() (cons (cons 1 '()) '()))))
(check-expect (cons "a" (cons (list 1) (list #false '())))
              (cons "a"
                    (cons (cons 1 '())
                          (cons #false (cons '() '())))))

(check-expect (cons "a" (list 0 #false))
              (list "a" 0 #false))
(check-expect (list (cons 1 (cons 13 '())))
              (list (list 1 13)))
(check-expect (cons (list 1 (list 13 '())) '())
              (list (list 1 (list 13 '()))))
(check-expect (list '() '() (cons 1 '()))
              (list '() '() (list 1)))
(check-expect (cons "a" (cons (list 1) (list #false '())))
              (list "a" (list 1) #false '()))

; Exercise 184
(check-expect (list (string=? "a" "b") (string=? "c" "c") #false)
              (list #false #true #false))
(check-expect (list (+ 10 20) (* 10 20) (/ 10 20))
              (list 30 200 1/2))
(check-expect (list "dana" "jane" "mary" "laura")
              (cons "dana" (cons "jane" (cons "mary" (cons "laura" '())))))

; Exercise 185
(check-expect (first (list 1 2 3))
              1)
(check-expect (rest (list 1 2 3))
              (list 2 3))
(check-expect (second (list 1 2 3))
              2)

; List-of-numbers -> List-of-numbers
; produces a sorted version of alon, descending order
(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [else (insert (first alon) (sort> (rest alon)))]))

(check-expect (sort> '()) '())
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))
(check-expect (sort> (list 3 2 1))
              (list 3 2 1))
(check-expect (sort> (list 1 2 3))
              (list 3 2 1))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [else (if (>= n (first alon))
              (cons n alon)
              (cons (first alon) (insert n (rest alon))))]))

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))
(check-expect (insert 2 (list 3 1)) (list 3 2 1))

; Exercise 186
(check-satisfied (sort> (list 12 20 -5)) sorted>?)
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)

; NEList-of-temperatures -> Boolean
; produces #true if the temperatures are sorted in descending order
; eg second is smaller than first, third smaller than second, so on.
(define (sorted>? anelot)
  (cond
    [(empty? (rest anelot)) #true]
    [(cons? (rest anelot))
     (and (> (first anelot) (first (rest anelot)))
          (sorted>? (rest anelot)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  '(9 8 7 6 5 4 3 2 1 0))

; well, you can do this
(check-expect (sort>/bad '()) '())
; with check-satisfied, though should really write predicate for length
(check-satisfied (sort>/bad (list 1 2 3)) length3?)

(define (length3? l)
  (= (length l) 3))
