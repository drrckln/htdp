;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3chap18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
#|
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
#|
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
|#
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

; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf (rest l))))
       (cond
         [(< (first l) smallest-in-rest) (first l)]
         [else smallest-in-rest]))]))

; Exercise 248
(define-struct ir [name price])
; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items taht cost less than $1
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (cond
            [(<= (ir-price (first an-inv)) 1.0)
             (cons (first an-inv) (extract1 (rest an-inv)))]
            [else (extract1 (rest an-inv))])]))

(define (extract1.v2 an-inv)
  (local ((define extracted (extract1 (rest an-inv)))
          (define first-item (first an-inv)))
    (cond
      [(empty? an-inv) '()]
      [else (cond
              [(<= (ir-price first-item) 1.0)
               (cons first-item extracted)]
              [else extracted])])))

(define test-inv (list (make-ir "john" 3)
                       (make-ir "boo" 0.5)
                       (make-ir "ga" 1.0)
                       (make-ir "phleb" 1.4)
                       (make-ir "chu" 1.3)
                       (make-ir "oo" 0.9)))

; Well.. extract1.v2 CHOKES, so it doesn't help..
; modified it seems to make no difference. maybe a bit faster.
(extract1.v2 test-inv)

; Exercise 249
; Lon -> Lon
; constructs a list from the items in l in descending order
(define (sort> l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(> an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))

(check-expect (sort> (list 0 4 3 5 6 8 9 7 2 1))
              (list 9 8 7 6 5 4 3 2 1 0))

; Lon -> Lon
; constructs a list from the items in l in ascending order
(define (sort< l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(< an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))

(check-expect (sort< (list 0 4 3 5 6 8 9 7 2 1))
              (list 0 1 2 3 4 5 6 7 8 9))

; Lon -> Lon
; constructs a list from the items in l in cmp order
(define (sort-a cmp l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) '()]
              [else (insert (first l) (sort (rest l)))]))
          ; Number Lon -> Lon
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(cmp an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))

(define (sort-a< l0)
  (sort-a < l0))
(check-expect (sort< (list 0 4 3 5 6 8 9 7 2 1))
              (sort-a< (list 0 4 3 5 6 8 9 7 2 1)))

(define (sort-a> l0)
  (sort-a > l0))
(check-expect (sort> (list 0 4 3 5 6 8 9 7 2 1))
              (sort-a> (list 0 4 3 5 6 8 9 7 2 1)))

; Los -> Los
; ascending
(define (sort-a-s< los)
  (local ((define (string-sort< s1 s2) (< (string-length s1)
                                          (string-length s2))))
    (sort-a string-sort< los)))

; Exercise 250
; Number -> [List-of [List-of Number]]
; creates identity (diagonal 1) matrices
(define (diagonal n)
  (local (; Number Number -> [List-of Number]
          ; cnt starts at 0
          (define (make-row i cnt)
            (cond
              [(= cnt n) '()]
              [else (cond
                      [(= i cnt) (cons 1 (make-row i (add1 cnt)))]
                      [else (cons 0 (make-row i (add1 cnt)))])]))
          (define positions (build-list n identity))
          (define (make-row-0 i) (make-row i 0)))
  (map make-row-0 positions)))
#|
(define (ident n)
  (local (; Number Number -> [List-of Number]
          (define (make-row i cnt)
            (cond
              [(= cnt n) '()]
              [(= i cnt) (cons 1 (make-row i (add1 cnt)))]
              [else (cons 0 (make-row i (add1 cnt)))]))
          ; Number -> [List-of Number]
          (define (building x)
            (cond
              [(zero? x) (list 0)]
              [else (cons x (building (sub1 x)))]))
          ; [List-of Number]
          (define built-list (reverse (building n)))
          (define
  (cond
    [(zero? n) '()]
    [(positive? n) (cons (make-row 0 ) .. n .. (sub1 n))]
|#

; Exercise 251
(inf (list 2 1 3))

; Exercise 252
; Nelon -> Number
; determines the smallest number on l
(define (sup l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define largest-in-rest (sup (rest l))))
       (cond
         [(> (first l) largest-in-rest) (first l)]
         [else largest-in-rest]))]))

(sup (list 2 1 3))
|#
; Exercise 253
((local ((define (f x) (+ (* 4 (sqr x)) 3)))
   f) 1)

; Exercise 254
((local ((define (f x) (+ x 3))
         (define (g x) (* x 4)))
   (if (odd? (f (g 1)))
       f
       g))
 2)


; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list

(check-expect (add-3-to-all (list (make-posn 30 10)
                                  (make-posn 0 0)))
              (list (make-posn 33 10) (make-posn 3 0)))

(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; ...
          (define (add-3-to-one p)
            (make-posn (+ (posn-x p) 3)
                       (posn-y p))))
    (map add-3-to-one lop)))

; [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y-coordinate is larger than 100

(check-expect (keep-good (list (make-posn 0 110) (make-posn 0 60)))
              (list (make-posn 0 60)))

(define (keep-good lop)
  (local (; Posn -> Boolean
          ; should this Posn stay on the list
          (define (good? p)
            (<= (posn-y p) 100)))
    (filter good? lop)))

; good? simply determines whether the y-coord is <= 100

; Posn Posn Number -> Boolean
; is the distance between pa dn q less than d
(define (close-to p q d)
  (< (sqrt (+ (sqr (- (posn-x p) (posn-x q)))
              (sqr (- (posn-y p) (posn-y q)))))
     d))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt
(check-expect (close? (list (make-posn 47 54) (make-posn 0 60))
                      (make-posn 50 50))
              #true)

(define CLOSENESS 10)

(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))

; [List-of Posn] -> Image
; adds the Posns on lop to the empty scene

(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 BACKGROUND))


(define BACKGROUND (empty-scene 200 200))
(define DOT (circle 5 "solid" "red"))

(define (dots lop)
  (local (; Posn Image -> Image
          (define (add-one-dot p scene)
            (place-image DOT (posn-x p) (posn-y p) scene)))
    (foldr add-one-dot BACKGROUND lop)))

; Exercise 255
; [List-of EUR] -> [List-of USD]
; converts based on exchange rate of EUR1.22/USD
(define (convert-euro lod)
  (local (; EUR -> USD
          ; converts euro to USD
          (define (euro->usd p)
            (* p 1.22)))
    (map euro->usd lod)))

(check-expect (convert-euro (list 1 2 3))
              (list 1.22 2.44 3.66))

; [List-of Fahrenheit] -> [List-of Celsius]
; converts..
(define (convertFC lof)
  (local (; Fahrenheit -> Celsius
          (define (f->c f)
            (* (- f 32) 5/9)))
    (map f->c lof)))

(check-expect (convertFC (list -40))
              (list -40))

; [List-of Posn] -> [List-of [List-of Number Number]]
(define (translate lop)
  (local (; Posn -> [List-of Number Number]
          (define (posn->listpair p)
            (list (posn-x p) (posn-y p))))
    (map posn->listpair lop)))

(check-expect (translate (list (make-posn 3 6) (make-posn 5 3)))
              (list (list 3 6) (list 5 3)))

; Exercise 256
(define-struct ir [name desc acq rec])

; [List-of IR] -> [List-of IR]
; sorts a list of IR by the difference between the two prices
(define (sort-ir inventory)
  (local (; IR IR -> Boolean
          ; sorts by diff between two prices
          (define (ir> ir1 ir2)
            (> (abs (- (ir-acq ir1) (ir-rec ir1)))
               (abs (- (ir-acq ir2) (ir-rec ir2))))))
    (sort inventory ir>)))

(check-expect (sort-ir (list (make-ir "boo" "gah" 7 5)
                             (make-ir "who" "me" 8 0)))
              (list (make-ir "who" "me" 8 0)
                    (make-ir "boo" "gah" 7 5)))

; Exercise 257
; Number [List-of IR] -> [List-of IR]
(define (eliminate-exp ua inventory)
  (local (; IR -> Boolean
          ; is the IR sales price below ua?
          (define (check-price? ir)
            (< (ir-rec ir) ua)))
    (filter check-price? inventory)))

(check-expect (eliminate-exp 5 (list (make-ir "boo" "gah" 7 5)
                                     (make-ir "who" "me" 8 0)))
              (list (make-ir "who" "me" 8 0)))

; String [List-of IR] -> [List-of IR]
; takes name of an inventory item, ty, produces
; list of ir records that don't use that name
(define (recall ty inventory)
  (local (; IR -> Boolean
          ; this item not being recalled?
          (define (to-recall? ir)
            (not (string=? ty (ir-name ir)))))
    (filter to-recall? inventory)))

(check-expect (recall "boo" (list (make-ir "boo" "gah" 7 5)
                                  (make-ir "who" "me" 8 0)))
              (list (make-ir "who" "me" 8 0)))

; [List-of String] [List-of String] -> [List-of String]
; finds names on both lists, or 2nd list in 1st list
(define (selection los1 los2)
  (local (; String -> Boolean
          ; tests for whether the string is in the list los1
          (define (test s)
            (local (; String -> Boolean
                    ; tests whether the target is s
                    (define (tester x)
                      (string=? s x)))
            (ormap tester los1))))
    (filter test los2)))

(check-expect (selection (list "whee" "bop" "boop" "beep")
                         (list "bop" "boop" "beep"))
              (list "bop" "boop" "beep"))

; Exercise 258
; N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
;   (build-list n f) == (list (f 0) ... (f (- n 1)))
;(define (build-list n f) ...)

(define (zero-indexed n)
  (build-list n identity))
(define (naturals n)
  (build-list n add1))
(define (build-list-frac n)
  (local (; Number -> Fraction
          ; creates the fraction 1/10^n
          (define (frac n)
            (/ 1 (expt 10 n))))
    (build-list n frac)))
(define (evens n)
  (local (; Number -> Number
          ; doubles the number
          (define (double n) (* n 2)))
    (build-list n double)))
; did that already

; Exercise 259
; String [List-of String] -> Boolean
; determines whether any of the names on the latter are
; equal to or an extension of the former
(define (find-name name lon)
  (local (; String -> Boolean
          ; same as name or extension?
          (define (same-ext? s)
            (string=? name (first-part s)))
          (define l (string-length name))
          ; String -> String
          ; selects the first part of string
          (define (first-part s)
            (substring s 0 l)))
    (ormap same-ext? lon)))

; [List-of String] -> Boolean
; checks all names start with "a"
(define (start-a? lon)
  (local (; String -> Boolean
          ; starts with a?
          (define (first-a? s)
            (string=? "a" (substring s 0 1))))
    (andmap first-a? lon)))
; You should use andmap, because that guarantees
; checking all names for the property. just have
; to "not" to make True.

; Exercise 260
; [List-of X] [List-of X] -> [List-of X]
; like append
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))

; foldl reverses the first list!

; [List-of Image] -> Image
; horizontally composes
(define (horizontalize loi)
  (foldr beside empty-image loi))

; [List-of Image] -> Image
; vertically composes
(define (verticalize loi)
  (foldr above empty-image loi))

; Exercise 261
; [X -> Y] [List-of X] -> [List-of Y]
(define (fold-map f lox)
  (local (; X Y -> Y
          (define (cons-f x y)
            (cons (f x) y)))
    (foldr cons-f '() lox)))

; Exercise 262
; Lo1s -> List-of-Lo1s
; produces all the prefixes of a lo1s
#|
(define (prefixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (cons lo1s (prefixes (reverse (rest (reverse lo1s)))))]))
|#
(define (prefixes lo1s)
  (local (; 1s -> [List-of Lo1s]
          (define positions (build-list (length lo1s) add1))
          (define (take n ls)
            (cond
              [(= n 0) '()]
              [else (cons (first ls) (take (sub1 n) (rest ls)))]))
          (define (taker n)
            (take n lo1s)))          
    (map taker positions)))

(list 1 2 3 4)
(list 1)
(list 1 2)
(list 1 2 3)

; Lo1s -> List-of-Lo1s
; produces all the suffixes of a lo1s
#|
(define (suffixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else (cons lo1s (suffixes (rest lo1s)))]))
|#
(define (suffixes lo1s)
  (local (; 1s -> [List-of Lo1s]
          (define positions (build-list (length lo1s) add1))
          (define (take n ls)
            (cond
              [(= n 0) '()]
              [else (cons (first ls) (take (sub1 n) (rest ls)))]))
          (define (taker n)
            (reverse (take n (reverse lo1s)))))          
    (map taker positions)))


(list 1 2 3 4)
(list 2 3 4)
(list 3 4)
(list 4)