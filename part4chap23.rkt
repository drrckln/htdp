;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
#|
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

; Exercise 320
; isn't this just (define-struct dir [name size readability content])?

(define-struct file [name size content])
; a File.v3 is a structure:
;   (make-file Symbol N String)
(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure:
;   (make-dir.v3 Symbol Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

; Exercise 321
(define TS.dir.v3
  (make-dir.v3 'TS
               (list (make-dir.v3 'Text
                                  '()
                                  (list (make-file 'part1 99 "")
                                        (make-file 'part2 52 "")
                                        (make-file 'part3 17 "")))
                     (make-dir.v3 'Libs
                                  (list (make-dir.v3 'Code
                                                     '()
                                                     (list (make-file 'hang 8 "")
                                                           (make-file 'draw 2 "")))
                                        (make-dir.v3 'Docs
                                                     '()
                                                     (list (make-file 'read! 19 ""))))
                                  '()))
               (list (make-file 'read! 10 ""))))

; Exercise 322
; Dir.v3 -> Number
(define (how-many.v3 directory)
  (match directory
    [(dir n '() '()) 0]
    [(dir n '() fs) (length fs)]
    [(dir n (cons d ds) fs) (+ (length fs)
                                  (how-many.v3 d)
                                  (how-many.v3 (make-dir.v3 n ds '())))]))


(check-expect (how-many.v3 TS.dir.v3) 7)
; You can be sure because you just cover all the cases
; and it's referentially transparent so you can design
; by checking the definitions
|#
; Exercise 323
; A Dir.v3 is a structure:
;   (make-dir.v3 Symbol [List-of Dir] [List-of File])

; Dir.v3 -> Number
(define (how-many.v4 directory)
  (foldr + (length (dir-files directory))
     (map how-many.v4 (dir-dirs directory))))

;(check-expect (how-many.v4 TS.dir.v3) 7)


; Exercise 324
(require htdp/dir)

; String -> Dir.v3
; creates a data representation of the directory that a-path identifies
; (define (create-dir a-path) ...)

(define d0 (create-dir "/Users/derricklin/repos/htdp")) ; on OS X 
(define d1 (create-dir "/Users/derricklin/repos/learnmath"))
(define d2 (create-dir "/Users/derricklin/Documents"))
(how-many.v4 d0)
(how-many.v4 d1)

; Couldn't get the pattern match to work for Exercise 322's version
; I'm confident because it's DEFINITIONAL!

; Exercise 325
; Dir String -> Boolean
; determines whether any file in dir is named name
(define (find? d n)
  (cond
    [(member? (string->symbol n) (map file-name (dir-files d))) #true]
    [(empty? (dir-dirs d)) #false]
    [else (andmap (lambda (dir) (find? dir n)) (dir-dirs d))]))

(check-expect (find? d1 "README.md") #true)

; Exercise 326
; Dir -> [List-of String]
(define (ls d)
  (append (map (lambda (f) (symbol->string (file-name f))) (dir-files d)) 
          (foldr append '() (map ls (dir-dirs d)))))

(ls d2)

; Exercise 327
; Dir -> Number
; computes the total size of all the files
; assumes each Dir is size 1
(define (du directory)
  (+ (foldr + 0 (map (lambda (f) (file-size f)) (dir-files directory)))
     1 ; directory itself
     (foldr + 0 (map du (dir-dirs directory)))))

(check-expect (du (make-dir 'a '() (list (make-file 'f 7 ""))))
              8)