;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname intermezzo_quote) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 217
(list 1 "a" 2 #false 3 "c")
(list)

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl" 1500)
      (list "dawn" 2300))

(define x 42)


; String String -> ... deeply nested list ...
; produces a (representation of) a web page with given author and title
(define (my-first-web-page author title)
  `(html
    (head
     (title ,title)
     (meta ((http-equiv "content-type")
            (content "text-html"))))
    (body
     (h1 ,title)
     (p "I, " ,author ", made this page."))))

; Exercise 218

(list 1 "a" 2 #false 3 "c")

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl , the great" 1500)
      (list "dawn" 2300))

(list html
      (list head (list title "ratings"))
      (list body (list h1 "ratings")
            (list p "A second web page")))