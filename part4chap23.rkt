;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part4chap23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(list (list 'part1 'part2 'part3)
      'read!
      (list (list 'hang 'draw)
            (list 'read!)))