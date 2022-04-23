;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname demolish) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; demolish.rkt


;; =================
;; Data definitions:

;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; interp. the classification level of a building in Vancouver

;; <examples are redundant for enumerations>

#;
(define (fn-for-BuildingStatus bs)
  (cond [(string=? "new" bs) (...)]
        [(string=? "old" bs) (...)]
        [(string=? "heritage" bs) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: "new"
;; - atomic distinct: "old"
;; - atomic distinct: "heritage"


;; =================
;; Functions:

;; BuildingStatus --> Boolean
;; Produce true if BuildingStatus is "old"
(check-expect (demolish? "new") false)
(check-expect (demolish? "old") true)
(check-expect (demolish? "heritage") false)

;(define (demolish? bs) false) ; this is the stub

(define (demolish? bs)
  (cond [(string=? "new" bs) false]
        [(string=? "old" bs) true]
        [(string=? "heritage" bs) false]))