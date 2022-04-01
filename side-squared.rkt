;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |side squared|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Number -> Number
;; n ^ 2, produce area of square from one side length

(check-expect (area 5) 25)
(check-expect (area 10) 100)

;(define (area n) 0)   ;stub

;(define (area n) ; template
;  (... n))

(define (area n) ; template
  (expt n 2))


