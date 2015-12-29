;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname part3chap18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 244

; [X -> Number] [List-of X] -> X
; finds the (first) item in alox that maximizes f, that is:
; if (argmax f (list x-1 ... x-n)) == x-i,
; then (>= (f x-i (f x-1)), (>= (f x-i) (f x-2)), and so on
; Finds the X in the list, that when, f is applied to it,
; results in the largest number (in all of the List of X).

; [X -> Number] [List-of X] -> X
; finds the (first) item in alox that minimizes f, that is:
; blah blah
; Finds the X in the list... smallest number. Basically, it's
; the number where if you map f onto the List-of-X, which X
; gave you the largest numerical value? (if tied, give the 1st)

; Exercise 245
; [X Y -> Y] Y [List-of X] -> Y
; my-foldl works just like foldl
(check-expect (my-foldl cons '() '(a b c)) (foldl cons '() '(a b c)))
(check-expect (my-foldl / 1 '(6 3 2)) (foldl / 1 '(6 3 2)))
(define (my-foldl f e l)
  (foldr f e (reverse l)))

; N [N -> X] -> [List-of X]
; works just like build-list
(define (my-build-list n f)
  (build 0 n f))

; N N [N -> X] -> [List-of X]
(define (build count n f)
  (cond
    [(= count n) '()]
    [else (cons (f count)
                (build (add1 count) n f))]))

(check-expect (my-build-list 10 add1) (build-list 10 add1))
(check-expect (my-build-list 3 sub1) (build-list 3 sub1))

; alternately, add-at-end 0, then map f and reverse the results

; [List-of Addr] -> String
; creates a string of first names, sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing.v2 l)
  (local (; String String -> String
          ; concatenates two strings and prefixes with space
          (define (string-append-with-space s t)
            (string-append " " s t)))
    ; - IN -
    (foldr string-append-with-space
           " "
           (sort (map address-first-name l)
                 string<?))))

(define (listing.v3 l)
  (local (; String String -> String
          ; concatenates two strings and prefixes with space
          (define (string-append-with-space s t)
            (string-append " " s t))
          (define first-names (map address-first-name l))
          (define sorted-names (sort first-names string<?)))
    ; - IN -
    (foldr string-append-with-space " " sorted-names)))

; Exercise 246
; Polygon -> Image
(define (render-poly p)
  (local (; NELoP Posn -> Image
          ; connects the dots in p by rendering lines in MT, adding Posn last
          (define (connect-dots p last)
            (cond
              [(empty? (rest p)) (render-line MT (first p) last)]
              [else (render-line (connect-dots (rest p) last)
                                 (first p)
                                 (second p))]))
          (define last (first p))
          ; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line
             im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red")))
    ; - IN -
    (connect-dots p last)))

; Exercise 247
; Word -> List-of-words
; find all re-arrangements of word
(define (arrangements word)
  (local (; 1String List-of-words -> List-of-words
          ; inserts the 1string at every position of every word
          (define (insert-everywhere/in-all-words letter low)
            (cond
              [(empty? low) '()]
              [else (append (insert-everywhere/word letter (first low))
                            (insert-everywhere/in-all-words letter (rest low)))]))
          ; 1String List-of-words -> List-of-words
          ; inserts the 1string at every position of every word
          (define (insert-everywhere/in-all-words letter low)
            (cond
              [(empty? low) '()]
              [else (append (insert-everywhere/word letter (first low))
                            (insert-everywhere/in-all-words letter (rest low)))]))
          ; 1String NEWord -> NEList-of-words
          ; inserts the 1string at every position of the word
          (define (insert-everywhere/word letter word)
            (cond
              [(empty? (rest word)) (list (append (list letter) word)
                                          (append word (list letter)))]
              [else (append (list (cons letter word))
                            (fmap (first word) (insert-everywhere/word letter (rest word))))]))
          ; w List-of-a -> List
          ; maps w onto ls
          (define (fmap w ls)
            (cond
              [(empty? ls) '()]
              [else (cons (cons w (first ls)) (fmap w (rest ls)))])))
    ; - IN -
    (cond
      [(empty? word) '()]
      [(empty? (rest word)) (list (list (first word)))]
      [else (insert-everywhere/in-all-words (first word)
                                            (arrangements (rest word)))])))




