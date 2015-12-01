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

; List-of-words -> List-of-strings
; turn all Words in low into Strings
(define (words->strings low)
  empty)

; List-of-strings -> List-of-strings
; pick out all those Strings that occur in the dictionary
(define (in-dictionary los)
  empty)


; A Word is either 
; – '() or
; – (cons 1String Word)
; interpretation a String as a list of single Strings (letters)

; A List-of-words is ...
 
; Word -> List-of-words
; find all re-arrangements of word
(define (arrangements word)
  (list word))

; Exercise 195
; String -> Word
; convert s to the chosen word representation
(define (string->word s)
  (explode s))

; Word -> String
; convert w to a string
(define (word->string s)
  (implode s))