;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part3chap19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 265
;(lambda (x y) (x y y)) ; legal if x is a function
;(lambda () 10) ; not legal, no variables
;(lambda (x) x) ; legal, just returns. identity.
;(lambda (x y) x) ; legal, like first
;(lambda x 10) ; not legal, syntax error

; Exercise 266
; 1. 3
; 2. 14
; 3. 13.25

; Exercise 267
#|
(lambda (n) (< n 10))
(lambda (x y) (number->string (* x y)))
(lambda (ir1 ir2) (< (ir-price ir1)
                     (ir-price ir2)))
(lambda (nat) (if (even? nat) 0 1))
(lambda (p im) (place-image (circle 3 "solid" "red")
                            (posn-x p)
                            (posn-y p)
                            im))
|#

; Exercise 268
(define (f-plain x) (* 10 x))

(define f-lambda (lambda (x)
                   (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

; Exercise 269
; yes it can

; Exercise 270

(map (lambda (x) (* 10 x))
     '(1 2 3))

(foldl (lambda (name rst) (string-append name ", " rst)) "etc."
       '("Matthew" "Robby"))

(define threshold 100)
;(define-struct ir [name price])
;(filter (lambda (ir) (<= (ir-price ir) threshold))
;        (list (make-ir "bear" 10) (make-ir "doll" 33)))

; Exercise 271
((lambda (x) x) (lambda (x) x))
; so.. you just get the original function
((lambda (x) (x x)) (lambda (x) x))
; identity applied twice
;((lambda (x) (x x)) (lambda (x) (x x)))
; infinite identities

; Exercise 272
; [List-of USD] -> [List-of Euro]
(define (convert-euro lod)
  (map (lambda (dollar) (* 1.22 dollar))
       lod))
; [List-of Fahrenheit] -> [List-of Celsius]
(define (convertFC lof)
  (map (lambda (f) (* 5/9 (- f 32))) lof))

; [List-of Posn] -> [List-of [List Number Number]]
(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p)))
       lop))

; Exercise 273
(define-struct ir [name desc acq rec])
; [List-of IR] -> [List-of IR]
; sorts by diff between the two prices
(define (ir-sorter loir)
  (cond
    [(empty? loir) '()]
    [else (insert (first loir) (ir-sorter (rest loir)))]))

; IR [List-of IR] -> [List-of IR]
; inserts IR into the sorted list
(define (insert ir loir)
  (cond
    [(empty? loir) (list ir)]
    [((lambda (x) (<= (abs (- (ir-acq ir) (ir-rec ir)))
                      (abs (- (ir-acq x) (ir-rec x)))))
      (first loir))
     (cons ir (loir))]
    [else (cons (first loir) (insert ir (rest loir)))]))

; Exercise 274
; Number [List-of IR] -> [List-of IR]
; produces list of all with acq price below ua
(define (eliminate-exp n loir)
  (filter (lambda (ir) (< (ir-acq ir) n)) loir))

; String [List-of IR] -> [List-of IR]
; removes all IR that have ty as the name
(define (recall ty loir)
  (filter (lambda (ir) (not (string=? ty (ir-name)))) loir))

; [List-of String] [List-of String] -> [List-of String]
; consumes two lists of names and selects all those from the
; second one that are also on the first
(define (selection l1 l2)
  (filter (lambda (name) (member? name l1)) l2))

; Exercise 275
(define n 5)
(build-list n (lambda (x) x))
(build-list n (lambda (x) (+ x 1)))
(build-list n (lambda (x) (/ 1 (expt 10 x))))
(build-list n (lambda (x) (* (+ x 1) 2)))
(define (identmatrix n)
  (build-list n (lambda (x) (build-list n (lambda (y)
                                            (cond
                                              [(= x y) 1]
                                              [else 0]))))))

; Exercise 276
; Name [List-of Name] -> [List-of Name]
; determines whether any of the names on the list are equal
; to or an extension of the name
(define (find-name name lon)
  (ormap (lambda (x) (cond
                       [(string=? name
                                  (substring 0 (string-length name)))
                        #true]
                       [else false]))
         lon))

; [List-of Name] -> Boolean
; checks that all names start with "a"
(define (check-start-a lon)
  (andmap (lambda (x) (if (string=? "a" (substring 0 1 x)) #true #false))
          lon))

; You should use andmap ("and" for conditions that hold for all)

; Exercise 277
; [List-of X] [List-of X] -> [List-of X]
; concatenates the two lists
(define (append-from-fold l1 l2)
  (foldr (lambda (x y) (cons x y)) l2 l1))

; Exercise 278
; (X -> Y) [List-of X] -> [List-of Y]
(define (map-via-fold f ls)
  (foldr (lambda (x y) (cons (f x) y)) '() ls))
