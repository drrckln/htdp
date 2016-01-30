;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; Exercise 315
; read! occurs twice in the directory tree TS:
; /TS/read!
; /TS/Libs/Docs/read!
; the total size of all the files is:
; 99 + 52 + 10 + 17 + 8 + 2 + 19
; with directories, add 5
; TS contains 3 + 1 levels

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a Symbol. 

; Exercise 316
(define TS.v1
  (list (list 'part1 'part2 'part3)
        'read!
        (list (list 'hang 'draw)
              (list 'read!))))

; Exercise 317
; Dir.v1 -> Number
; determines how many files the directory contains
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(symbol? (first dir)) (+ 1 (how-many (rest dir)))]
    [(list? (first dir)) (+ (how-many (first dir))
                            (how-many (rest dir)))]))

(check-expect (how-many TS.v1) 7)

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir Symbol LOFD)
 
; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a Symbol.

; Exercise 318
(define TS.v2
  (make-dir 'TS (list (make-dir 'Text (list 'part1 'part2 'part3))
                      'read!
                      (make-dir 'Libs (list (make-dir 'Code (list 'hang 'draw))
                                            (make-dir 'Docs (list 'read!)))))))

; Exercise 319
; Dir.v2 -> Number
; determines how many files the directory contains
(define (how-many.v2 directory)
  (match directory
    [(dir name '()) 0]
    [(dir name (cons (? symbol?) rst)) (+ 1 (how-many.v2 (make-dir name rst)))]
    [(dir name (cons (? dir?) rst))
     (+ (how-many.v2 (first (dir-content directory)))
        (how-many.v2 (make-dir name rst)))]))

(check-expect (how-many.v2 TS.v2) 7)
