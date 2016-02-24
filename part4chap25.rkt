;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define-struct element [name content])

; <machine><action /><action /><action /></machine> expanded is:
; <machine><action></action><action></action><action></action></machine>

;(make-element "machine" (list (make-element "action" '())))

(define-struct element [name attributes content])
(define-struct attribute [name value])

(make-element "machine" (list (make-attribute 'initial "red")) '())
'(machine (action))

(make-element "machine" (list (make-attribute 'initial "red"))
              (list
               (make-element "action"
                             (list (make-attribute 'state "red")
                                   (make-attribute 'next "green"))
                             '())
               (make-element "action"
                             (list (make-attribute 'state "green")
                                   (make-attribute 'next "yellow"))
                             '())
               (make-element "action"
                             (list (make-attribute 'state "yellow")
                                   (make-attribute 'next "green"))
                             '())))

'(machine ((initial "red"))
          ((action ((state "red") (next "green")))
           (action ((state "green") (next "yellow")))
           (action ((state "yellow") (next "green")))))

; An Xexpr.v0 (short for X-expression) is
; (cons Symbol '())

; An Xexpr.v1 is
; (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is
; (cons Symbol [List-of Xexpr.v2])
; (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; An Attribute is
; (cons Symbol (cons String '())

; Exercise 351
; An Xexpr.v2 is
; (cons Symbol '())
; (cons Symbol (cons Xexpr.v2 Xexpr.v2*))
; (cons Symbol (cons Attribute* Xexpr.v2*))

; (cons Symbol '())
; (cons Symbol Xexpr.v2)
; (cons Symbol (list Attribute..) Xexpr.v2)

; Exercise 352
#|
(cons 'transition (list (list 'from "seen-e")
                        (list 'to "seen-f"))
      '())

(cons 'ul (list (list 'li
                      (list (list 'word)
                            (list 'word)))
                (list 'li
                      (list (list 'word)))))

(cons 'end '()) ; THIS ONE

; Exercise 353
'(server ((name "example.org")))
<server name="example.org"/>

'(carcassonne (board (grass)) (player ((name "sam"))))
<carcassonne board> </carcassonne> ; malformed?

'(start)
<start/>
|#

(define a0 '((initial "red")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  loa-or-x
                  '()))])))

(check-expect (xexpr-attributes e0) '())
(check-expect (xexpr-attributes e1) '((initial "red")))
(check-expect (xexpr-attributes e2) '())
(check-expect (xexpr-attributes e3) '())
(check-expect (xexpr-attributes e4) '((initial "red")))

; [List-of Attribute] or Xexpr.v2 -> ???
; determine whether x is an element of [List-of Attribute]; #false otherwise
; this is such a hack, wtf
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else (local ((define possible-attribute (first x)))
            (cons? possible-attribute))]))

; Exercise 354
; Xexpr.v2 -> Symbol
(define (xexpr-name expr)
  (first expr))

; Xexpr.v2 -> [List-of Xexpr.v2]
(define (xexpr-content expr)
  (cond
    [(empty? (rest expr)) '()]
    [(list-of-attributes? (second expr))
     (rest (rest expr))]
    [else (rest expr)]))

; Exercise 355
(define (xexpr-attributes xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) ...]
      [else (... (first optional-loa+content)
             ... (xexpr-attributes (rest optional-loa+content) ...))])))
; as (first optional-loa+content) is basicaly (second xe),
; we can look to see this is either list of attributes or content
; thus (rest optional-loa+content) is never needed.
