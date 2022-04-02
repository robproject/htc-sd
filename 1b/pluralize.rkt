;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pluralize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; String -> String
;; append "s" to given string

(check-expect (pluralize "asd") "asds")
(check-expect (pluralize "cat") "cats")
(check-expect (pluralize 7) false)

;(define (pluralize s) "") stub

(define (pluralize s)
  (if (string? s)
      (string-append s "s")
      false))