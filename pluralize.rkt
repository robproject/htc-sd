;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pluralize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; string -> string
;; adds "s" to the end of s

(check-expect (pluralize "asdf") "asdfs")
(check-expect (pluralize "wore") "wores")

; (define (pluralize s) "") ;stub

; (define (pluralize s) ;template
;   (... single))

(define (pluralize s)
  (string-append s "s"))