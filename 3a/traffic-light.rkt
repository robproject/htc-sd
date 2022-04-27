;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light.rkt


;; Flow regulator for primitive human-repositioning devices
;; Humans, in control of cars, must be made aware of how they must navigate their path
;; Traffic lights are used to indicate the required flow of travellers
;; cars << trains


;; LightState

;; =================
;; Constants:

(define WIDTH 300)
(define HEIGHT 900)
(define BODCOL "black")
(define BODY (empty-scene WIDTH HEIGHT BODCOL))
(define CIRR 125)
(define YEL0 (circle CIRR "outline" "yellow"))
(define RED0 (circle CIRR "outline" "red"))
(define GRN0 (circle CIRR "outline" "green"))

(define LX (/ WIDTH 2))
(define YELY (* HEIGHT .5))
(define GRNY (* HEIGHT .84))
(define REDY (* HEIGHT .16))

(define YEL1 (circle CIRR "solid" "yellow"))
(define RED1 (circle CIRR "solid" "red"))
(define GRN1 (circle CIRR "solid" "green"))

(define YELL (place-image
              YEL1 LX YELY
              (place-image
               GRN0 LX GRNY
               (place-image
                RED0 LX REDY
                BODY))))

(define REDL (place-image
              YEL0 LX YELY
              (place-image
               GRN0 LX GRNY
               (place-image
                RED1 LX REDY
                BODY))))

(define GRNL (place-image
              YEL0 LX YELY
              (place-image
               GRN1 LX GRNY
               (place-image
                RED0 LX REDY
                BODY))))

;; =================
;; Data definitions:

;; LightState is one of:
;; - "yel"
;; - "red"
;; - "grn"
;; interp. the color of traffic light
;; <examples redundant>

#;
(define (fn-for-lightstate ls)
  (cond [(string=? "red" ls) (...)]
        [(string=? "yel" ls) (...)]
        [(string=? "grn" ls) (...)]))

;; Template rules used:

;; - one of: 3 cases
;; - atomic distinct: "red"
;; - atomic distinct: "yel"
;; - atomic distinct: "grn"


;; =================
;; Functions:

;; LightState -> LightState
;; COL can be "red", "yel", or "grn"
;; (main COL)
;; 
(define (main ls)
  (big-bang ls                          ; LightState
            (on-tick  next-color 1)     ; LightState -> LightState
            (to-draw   render)))        ; LightState -> Image

;; LightState -> LightState
;; produce the next color in sequence R-G-Y
(define (next-color ls)
  (cond [(string=? "red" ls) "grn"]
        [(string=? "yel" ls) "red"]
        [(string=? "grn" ls) "yel"]))


;; LightState -> Image
;; render current light color
;; !!!
(define (render ls)
  (cond [(string=? "red" ls) REDL]
        [(string=? "yel" ls) YELL]
        [(string=? "grn" ls) GRNL]))
