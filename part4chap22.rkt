;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct no-parent [])
(define MTFT (make-no-parent))
(define-struct child [father mother name date eyes])
; A FT (family tree) is one of:
; - MTFT
; - (make-child FT FT String N String)

(make-child MTFT MTFT "Carl" 1926 "green")
(make-child (make-child MTFT MTFT "Carl" 1926 "green")
            (make-child MTFT MTFT "Bettina" 1926 "green")
            "Adam"
            1950
            "hazel")

; Oldest Generation:
(define Carl (make-child MTFT MTFT "Carl" 1926 "green"))
(define Bettina (make-child MTFT MTFT "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child MTFT MTFT "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

#| Template
; FT -> ???
; ...
(define (fun-for-ft a-ftree)
  (cond
    [(no-parent? a-ftree) ...]
    [else ; (child? a-ftree)
     (... (fun-for-ft (child-father a-ftree)) ...
      ... (fun-for-ft (child-mother a-ftree)) ...
      ... (child-name a-ftree) ...
      ... (child-date a-ftree) ...
      ... (child-eyes a-ftree) ...)]))
|#

; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else
      (or (string=? (child-eyes a-ftree) "blue")
          (blue-eyed-child? (child-father a-ftree))
          (blue-eyed-child? (child-mother a-ftree)))]))

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)

; Exercise 296
; FT -> Number
; counts the child structures in a-ft
(define (count-persons a-ft)
  (cond
    [(no-parent? a-ft) 0]
    [else (+ 1
             (count-persons (child-father a-ft))
             (count-persons (child-mother a-ft)))]))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Dave) 3)
(check-expect (count-persons Gustav) 5)

; Exercise 297
; FT -> Number
; produces the average age of all child structures in the family tree
(define (average-age ft)
  (/ (sum-ages ft 2016)
     (count-persons ft)))

(check-expect (average-age Carl) (- 2016 1926))
(check-expect (average-age Eva) (/ (+ (- 2016 1965)
                                      (- 2016 1926)
                                      (- 2016 1926))
                                   3))

; FT Number -> Number
; sums the ages of all child structures in the family tree
(define (sum-ages ft current-year)
  (cond
    [(no-parent? ft) 0]
    [else ; (child? a-ftree)
     (+ (sum-ages (child-father ft) current-year)
        (sum-ages (child-mother ft) current-year)
        (- current-year (child-date ft)))]))

(check-expect (sum-ages Carl 2013) (- 2013 1926))
(check-expect (sum-ages Eva 2016) (+ (- 2016 1965)
                                     (- 2016 1926)
                                     (- 2016 1926)))

; Exercise 298
; FT -> [List-of Color]
; produces a list of all eye colors in the tree
(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [else ; (child? a-ftree)
     (append (list (child-eyes ft))
             (eye-colors (child-father ft))
             (eye-colors (child-mother ft)))]))

; Exercise 299
; FT -> Boolean
; determines whether any ancestors had blue eyes
(define (blue-eyed-ancestor? ft)
  (cond
    [(no-parent? ft) #false]
    [else (or (blue-eyed-child? (child-father ft))
              (blue-eyed-child? (child-mother ft)))]))

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

; This version fails, because it keeps checking the ancestors
; recursively. Ignores the current child at every level of depth.
; Will always result in #false
#|
(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else (or (blue-eyed-ancestor? (child-father a-ftree))
              (blue-eyed-ancestor? (child-mother a-ftree)))]))
|#

; A FF (family forest) is one of:
; - '()
; - (cons FT FF)

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; Exercise 300
; A FF (family forest) is: [List-of FT]
; - '()
; - (cons FT [List-of FT])

; [List-of FT] -> Boolean
; does the forest contain any child node with "blue" eyes
 
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

#|
(define (blue-eyed-child-in-forest? a-forest)
  (cond
    [(empty? a-forest) #false]
    [else (or (blue-eyed-child? (first a-forest))
              (blue-eyed-child-in-forest? (rest a-forest)))]))
|#

(define (blue-eyed-child-in-forest? ft)
  (ormap blue-eyed-child? ft))

; Exercise 301
; FF Natural -> Number
; produces the average age of all child nodes in the forest
(define (averaged-age ff year)
  (/ (foldl + 0 (map (lambda (ft) (sum-ages ft year)) ff))
     (foldl + 0 (map count-persons ff))))

; An S-expr (S-expression) is one of: 
; – Atom
; – SL
; An SL (S-list) is one of: 
; – '()
; – (cons S-expr SL)
; An Atom is one of: 
; – Number
; – String
; – Symbol

; Exercise 302
; S-expr -> Boolean
; determines if the s-expr is a atom
(define (atom? s-expr)
  (cond
    [(number? s-expr) #true]
    [(string? s-expr) #true]
    [(symbol? s-expr) #true]
    [else #false]))

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
;(define (count sexp sy)
;  0)

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)

#|
; S-expr template
; S-expr -> ???
(define (f-sexp sexp)
  (cond
    [(atom? sexp) (f-atom sexp)]
    [(list? sexp) (f-list sexp)]))

; SL template
; SL -> ???
(define (f-list sl)
  (cond
    [(empty? sl) ...]
    [else (... (first sl) ... (f-list (rest sl)) ...)]))

; Atom template
; Atom -> ???
(define (f-atom atom)
  (cond
    [(number? atom) ...]
    [(string? atom) ...]
    [(symbol? atom) ...]))

; count problem templates
(define (count sexp sy)
 (cond
   [(atom? sexp) (count-atom sexp sy)]
   [else (count-sl sexp sy)]))
 
(define (count-sl sl sy)
  (cond
    [(empty? sl) ...]
    [else (... (count (first sl) sy)
           ... (count-sl (rest sl) sy) ...)]))
 
(define (count-atom at sy)
  (cond
    [(number? at) ...]
    [(string? at) ...]
    [(symbol? at) ...]))


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
 (cond
   [(atom? sexp) (count-atom sexp sy)]
   [else (count-sl sexp sy)]))
 
; SL Symbol -> N 
; counts all occurrences of sy in sl 
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else (+ (count (first sl) sy) (count-sl (rest sl) sy))]))
 
; Atom Symbol -> N 
; counts all occurrences of sy in at 
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))
|#

; Exercise 303
; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (local (; S-expr -> N 
          ; the main function 
          (define (count-sexp sexp)
            (cond
              [(atom? sexp) (count-atom sexp)]
              [else (count-sl sexp)]))
 
          ; SL -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else (+ (count-sexp (first sl))
                       (count-sl (rest sl)))]))
 
          ; Atom -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    ; — IN —
    (count-sexp sexp)))

; Exercise 304
; S-expr -> Number
; determines the depth of sexp
(define (depth sexp)
  (cond
    [(atom? sexp) 1]
    [else (+ (depth-sl sexp) 1)]))

; SL -> Number
; determines the depth of sexp
(define (depth-sl sexp)
  (cond
    [(empty? sexp) 0]
    [else (max (depth (first sexp))
               (depth-sl (rest sexp)))]))

(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)

; Exercise 305
; S-expr Symbol Symbol -> S-expr
; replaces symbol old with new
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (sub-atom sexp old new)]
    [else (sub-sl sexp old new)]))

; Atom Symbol Symbol -> Atom
(define (sub-atom at old new)
  (cond
    [(symbol? at) (if (equal? at old) new at)]
    [else at]))

; SL Symbol Symbol -> SL
(define (sub-sl sl old new)
  (cond
    [(empty? sl) '()]
    [else (cons (substitute (first sl) old new)
                (sub-sl (rest sl) old new))]))

(check-expect (substitute '(abc def (hi jk def)) 'def 'afk)
              '(abc afk (hi jk afk)))