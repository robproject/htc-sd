;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname img-larger) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define bigger (square 10 "solid" "blue"))
(define smaller (square 5 "solid" "red"))

;; Image -> Boolean
;; Produces true if image area of first img is greater than image area of second


(check-expect (img-larger bigger smaller) true)
(check-expect (img-larger smaller bigger) false)
(check-expect (img-larger bigger bigger) false)

; (define (img-larger img img1) true) stub

(define (img-larger img0 img1)
  (> (*
      (image-width img0)
      (image-height img0))
     (*
      (image-width img1)
      (image-height img1))))