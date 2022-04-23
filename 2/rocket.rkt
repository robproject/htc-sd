;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rocket.rkt

;; =================
;; Data definitions:

;; RocketDescent is one of:
;; - Number(0,100]
;; - false
;; Interp. the status of a rocket's descent
;;   Number(0, 100] is the current altitude
;;   false          is considered landed

(define a0 100)
(define a1 50)
(define a2 1)
(define a3 false)

#;
(define (fn-for-rocketdescent a)
  (cond [(and (number? a)
              (> 0 a)) (... a)]
        [else (... a)]))
  
;; Template rules used:
;; one-of: 2 cases
;; - atomic distinct: false
;; - atomic non-distinct: Number


;; =================
;; Functions:

;; RocketDescent --> String
;; Produces string conveying current altitude or rocket has landed
(check-expect (rocket-descent-to-msg 100) "The rocket's altitude is 100km.")
(check-expect (rocket-descent-to-msg 50) "The rocket's altitude is 50km.")
(check-expect (rocket-descent-to-msg false) "The rocket has landed!")

;(define (rocket-descent-to-msg a) "The rocket has landed!") ; this is the stub

; template from RocketDescent

(define (rocket-descent-to-msg a)
  (cond [(and (number? a)
              (< 0 a)) (format "The rocket's altitude is ~akm." a)]
        [else "The rocket has landed!"]))
