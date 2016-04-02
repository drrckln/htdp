;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo_numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; an Inex is a structure:
;    (make-inex N99 S N99)
; An S is either 1 or -1
; An N99 is an N between 0 and 99 inclusive

; N Number N -> Inex
; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else
     (error 'inex "(<= 0 m 99), s in {+1,-1}, (<= 0 e 99) expected")]))
 
; Inex -> Number
; convert an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt 10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(create-inex 12 1 2)
; (create-inex 120 1 1)
(inex->number (create-inex 50 -1 20))
(inex->number (create-inex 5 -1 19))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))


(check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
              (create-inex 3 1 0))
(check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
              (create-inex 11 -1 1))

#|
(check-expect (inex* (create-inex 2 1 4) (create-inex 8 1 10))
              (create-inex 16 1 14))
(check-expect (inex* (create-inex 20 1 1) (create-inex 5 1 4))
              (create-inex 10 1 6))
(check-expect (inex* (create-inex 27 -1 1) (create-inex 7 1 4))
              (create-inex 19 1 4))
|#

; 27 x 10^-1  *  7 x 10^4
; should be 189 x 10^3, or 18.9 * 10^4
; but we can't represent that with Inex
; the closest is 19 x 10^4
; eg (create-inex 19 1 4)

; Exercise 387
; Inex Inex -> Inex
; constraint is exponent differs by one at most
(define (inex+ n1 n2)
  (cond
    [(= (* (inex-sign n1) (inex-mantissa n1))
        (* (inex-sign n2) (inex-mantissa n2)))
     (cond
       [(and (= 1 (inex-sign n1))
             (= 99 (inex-exponent n1)))
        (if (> 99 (+ (inex-mantissa n1) (inex-mantissa n2)))
            (error "out of bounds")
            (create-inex (+ (inex-mantissa n1) (inex-mantissa n2))
                         1
                         (inex-exponent n1)))]
       [(< 99 (+ (inex-mantissa n1) (inex-mantissa n2)))
        (create-inex (round (/ (+ (inex-mantissa n1) (inex-mantissa n2)) 10))
                     (inex-sign n1)
                     (add1 (inex-exponent n1)))]
       [else (create-inex (+ (inex-mantissa n1) (inex-mantissa n2))
                          (inex-sign n1)
                          (inex-exponent n1))])]
    [(= (abs (- (* (inex-sign n1) (inex-mantissa n1))
                (* (inex-sign n2) (inex-mantissa n2))))
        1)
     (cond
       [(positive? (- (* (inex-sign n1) (inex-mantissa n1))
                      (* (inex-sign n2) (inex-mantissa n2))))
        (create-inex (+ (* (inex-mantissa n1) 10)
                        (inex-mantissa n2))
                     (inex-sign n2)
                     (inex-exponent n2))]
       [else (create-inex (round (/ (+ (inex-mantissa n1)
                                       (* (inex-mantissa n2) 10))
                                    10))
                          (inex-sign n1)
                          (inex-exponent n1))])]
    [else (error "exponent differs too much")]))

; Inex? -> Inex
; normalizes an Inex if it's not proper
(define (normalize num)
  (cond
    [(<= 0 (inex-mantissa num) 99) num]
    [else (normalize
           (create-inex (round (/ (inex-mantissa num) 10))
                        (cond
                          [(and (= (inex-sign num) -1) (> (inex-exponent num) 1)) -1]
                          [else 1])
                        (cond
                          [(positive? (inex-sign num)) (add1 (inex-exponent num))]
                          [(= (inex-exponent num) 0) 1]
                          [(= (inex-exponent num) 1) 0]
                          [else (sub1 (inex-exponent num))])))]))

; will be doing Haskell / Lambda Calculus today