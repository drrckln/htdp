;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname part2chap13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define DICTIONARY-LOCATION "/usr/share/dict/words")
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; String -> List-of-Strings
; find all words that the letters of some given word spell
(define (alternative-words s)
  (in-dictionary (words->strings (arrangements (string->word s)))))

(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))

; List-of-Strings -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

(check-satisfied (alternative-words "rat") all-words-from-rat?)


; A Word is either 
; – '() or
; – (cons 1String Word)
; interpretation a String as a list of single Strings (letters)

; A List-of-words is ...

; Exercise 195
; String -> Word
; convert s to the chosen word representation
(define (string->word s)
  (explode s))

; Word -> String
; convert w to a string
(define (word->string s)
  (implode s))

; We have in mind:
; (in-dictionary (arrangements s))

; Exercise 196
; List-of-words -> List-of-strings
; turn all Words in low into Strings
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; Exercise 197
; List-of-strings -> List-of-strings
; pick out all those Strings that occur in the dictionary
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (member? (first los) DICTIONARY-AS-LIST)
              (cons (first los) (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))

; Exercise 198
; A List-of-words is either:
; - '()
; - (cons Word List-of-words)

; Word examples
'()
(cons "a" '())
(cons "a" (cons "a" '()))
(cons "b" (cons "a" (cons "a" '())))

; List-of-words examples
'()
(cons (list "a") '())
(cons (list "baaaaa") '())
(cons (list "boo") (cons "baa" (cons "bee" '())))

(check-expect (arrangements (list "e" "d"))
              (list (list "e" "d") (list "d" "e")))

; Word -> List-of-words
; find all re-arrangements of word
(define (arrangements word)
  (cond
    [(empty? word) (list '())]
    [else (insert-everywhere/in-all-words (first word)
                                          (arrangements (rest word)))]))


; Exercise 199
; 1String List-of-words -> List-of-words
; inserts the 1string at every position of every word
(define (insert-everywhere/in-all-words letter low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere/word letter (first low))
                  (insert-everywhere/in-all-words letter (rest low)))]))

(check-expect (insert-everywhere/in-all-words "a" '())
              '())
(check-expect (insert-everywhere/in-all-words "a" (list (list "b")))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "b") (list "c")))
              (list (list "a" "b") (list "b" "a") (list "a" "c") (list "c" "a")))
(check-expect (insert-everywhere/in-all-words "a" (list (list "e" "r") (list "r" "e")))
              (list (list "a" "e" "r") (list "e" "a" "r") (list "e" "r" "a")
                    (list "a" "r" "e") (list "r" "a" "e") (list "r" "e" "a")))

; 1String Word -> NEList-of-words
; inserts the 1string at every position of the word
(define (insert-everywhere/word letter word)
  (cond
    [(empty? (rest word)) (list (append (list letter) word)
                                (append word (list letter)))]
    [else (append (list (cons letter word))
                  (fmap (first word) (insert-everywhere/word letter (rest word))))]))

(check-expect (insert-everywhere/word "a" (list "b"))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/word "a" (list "e" "r"))
              (list (list "a" "e" "r") (list "e" "a" "r") (list "e" "r" "a")))

; w List-of-a -> List
; maps w onto ls
(define (fmap w ls)
  (cond
    [(empty? ls) '()]
    [else (cons (cons w (first ls)) (fmap w (rest ls)))]))
    
