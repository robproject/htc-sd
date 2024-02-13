;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:
(define WIDTH  300)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define INVADER-DX-MAX 5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 10)

(define HIT-RANGE 18)

(define INVADE-RATE 80)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listOfInvader) (listOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define lom1 empty)
(define lom2 (cons M1 (cons M2 empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first lom) is Missile
;; - self reference: (rest lom) is ListOfMissile



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start program by evaluating (main G0)
(define (main g)
  (big-bang g                ; Game
    (on-tick tock)           ; Game -> Game
    (to-draw render)         ; Game -> Image
    (stop-when end?)         ; Game -> Boolean
    (on-key handle-key)))     ; Game KeyEvent -> Game



;; Game -> Game
;; produce the next game state with incremented positions and updated missile and invader collisions
(check-expect (tock G0) (make-game empty empty (next-tank T0)))
(check-expect (tock G2) (make-game (next-invaders (list I1) (list M1)) (next-missiles (list M1) (list I1)) (next-tank T1)))

;(define (tock g) (make-game empty empty T1)) ;stub
;<template from Game>

(define (tock g)
  (make-game (next-invaders
              (cons (spawn-invader INVADE-RATE
                                   (random WIDTH)
                                   (random INVADER-DX-MAX)
                                   (random 2))
                    (game-invaders g)) (game-missiles g))
       (next-missiles (game-missiles g) (game-invaders g))
       (next-tank (game-tank g ))))



;; Number Number Number Number -> Invader or empty
;; produce Invader randomly based on invade rate from 0 to 100
;; Invade-Rate, Random Width, Random dx, Random (0,1)
(check-expect (spawn-invader 0 40 3 0) empty)
(check-expect (spawn-invader 10000 40 3 0) ; 10000 all but guarantees spawn
              (make-invader 40 (- HEIGHT (image-height INVADER)) (rand-dx 3 0)))

; (define (spawn-invader 100 50 5 0) (make-invader 20 20 1)) ;stub
;<template from Invader data def plus condition>

(define (spawn-invader rate rand-width rand-dx-max rand-two)
  (cond [(< (random 100) (/ rate 4))
         (make-invader rand-width (- HEIGHT (image-height INVADER)) (rand-dx rand-dx-max rand-two))]
        [else empty]))



;; Number Number -> Number
;; Returns random number in given range -n-1 < 0 < n+1
;; Dx, 0 or 1
(check-expect (rand-dx 5 1)
              (* (- 1 (* 2 1)) 6))
(check-expect (rand-dx 5 0)
              (* (- 1 (* 2 0)) 6))

; (define (rand-dx 1 0) 0) ;stub
#;
(define (rand-dx num rand-two)
  (... (+ num rand-two) 0))
  
(define (rand-dx num rand-two)
  (* (- 1 (* 2 rand-two)) (+ 1 num)))



;; Tank -> Tank
;; produce the next tank position
;; if position == 0 or scene width, don't move
(check-expect (next-tank (make-tank 20 1)) (make-tank (- 20 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 10 -1)) (make-tank (/ (image-width TANK) 2) -1))
(check-expect (next-tank (make-tank -1 -1)) (make-tank 15 -1))
(check-expect (next-tank (make-tank (+ WIDTH 1) 1)) (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))


; (define (next-tank t) (make-tank 10 3)) ;stub
;<template from data def>

(define (next-tank t)
  (cond[(> (move-tx t) (- WIDTH (/ (image-width TANK) 2))) (make-tank (- WIDTH (/ (image-width TANK) 2)) (tank-dir t))]
       [(< (move-tx t) (+ 0 (/ (image-width TANK) 2))) (make-tank (+ 0  (/ (image-width TANK) 2)) (tank-dir t))]
       [else
        (make-tank (move-tx t) (tank-dir t))]))



;; Tank -> Number
;; produce next tank x
(check-expect (move-tx (make-tank 20 1)) (+ 20 (* TANK-SPEED -1)))

; (define (move-tx (make-tank 10 3)) 4); stub
;<template from data def>

(define (move-tx tank)
  (+ (tank-x tank) (* TANK-SPEED (- (tank-dir tank)))))



;; listOfInvader listOfMissile -> listOfInvader
;; produce the next listOfInvaders after colliding with Missiles and incrementing position
(check-expect (next-invaders (cons I1 (cons I2 empty)) (cons M1 empty))
              (cons (move-invader I1)
              (cons (move-invader I2) empty))) ; no missiles hit

(check-expect (next-invaders (cons I1 (cons empty (cons I2 empty))) (cons M1 empty))
              (cons (move-invader I1)
              (cons (move-invader I2) empty))) ; no missiles hit, check for emtpy invader

(check-expect (next-invaders (cons I1 (cons I2 empty)) (cons M1 (cons M2 empty)))
              (cons empty (cons (move-invader I2) empty))) ; hit first invader

(check-expect (next-invaders (cons empty (cons (move-invader I2) empty)) (cons empty (cons M2 empty)))
              (cons (move-invader (move-invader I2)) empty)) ; remove empty invader, missile also empty

; (define (next-invaders (list I2 I1) (list M2 M1)) (list I1)) ;stub
#;
(define (fn-for-loi-lom loi lom)
  (cond [(empty? loi) empty]
        [(empty? (first loi)) (fn-for-loi-lom (rest loi) lom)]
        [else
         (... (fn-for-invader (first loi) lom)
              (fn-for-loi-lom (rest loi) lom))]))

(define (next-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? (first loi)) (next-invaders (rest loi) lom)]
        [else
             (cons (handle-invader (first loi) lom)
              (next-invaders (rest loi) lom))]))



;; Invader listOfMissile -> Invader
;; produce Invader with incremented position if not hit by missile
(check-expect (handle-invader (make-invader 20 20 1) empty)
              (make-invader (+ (* INVADER-X-SPEED 1) 20) (- 20 (* INVADER-Y-SPEED (abs 1))) 1)) ; no missiles
(check-expect (handle-invader (make-invader 20 20 1) (cons (make-missile 40 40) empty))
              (make-invader (+ (* INVADER-X-SPEED 1) 20) (move-iy (make-invader 20 20 1)) 1)) ; invader position incremented, not hit
(check-expect (handle-invader (make-invader 20 20 1)
                              (cons (make-missile 20 20) empty)) empty)   ; missile hit invader

; (define (handle-invader I1 (list M1 M2)) I1) ;stub
;<template from lom>

(define (handle-invader invader lom)
    (cond [(invader-collision? invader lom) empty]
          [else (move-invader invader)]))



;; Invader listOfMissile -> boolean
;; produce true if Invader is hit by any missile in list
(check-expect (invader-collision? I1 (cons M1 (cons M2 empty))) true)
(check-expect (invader-collision? I1 (cons M1 empty)) false)
(check-expect (invader-collision? I1 (cons M2 empty)) true)
(check-expect (invader-collision? I1 (cons M3 empty)) true)
(check-expect (invader-collision? I1 empty) false)

; (define (invader-collision? I1 (list M1 M2)) true) ;stub
;<template from lom>

(define (invader-collision? invader lom)
  (cond [(empty? lom) false]
        [(within-radius? invader (first lom)) true]
        [else (invader-collision? invader (rest lom))]))



;; Invader or Empty  Missile or Empty -> boolean
;; produce true if Missile and Invader have collided
(check-expect (within-radius? I1 M1) false)
(check-expect (within-radius? I1 M2) true)
(check-expect (within-radius? I1 M3) true)
(check-expect (within-radius? I1 empty) false)
(check-expect (within-radius? empty M1) false)

; (define (within-radius? I1 M1) true) ;stub
;<template is combo of fn-for-invader and fn-for-missile>

(define (within-radius? invader missile)
  (cond [(or (empty? invader) (empty? missile)) false]
    [(>= HIT-RANGE
            (sqrt (+
                   (expt (- (invader-x invader) (missile-x missile)) 2)
                   (expt (- (invader-y invader) (missile-y missile)) 2))))
            true]
        [else false]))



;; Invader -> Invader
;; produce Invader with incremented position
(check-expect (move-invader (make-invader 30 10 -3))
              (make-invader (move-ix (make-invader 30 10 -3))
                            (move-iy (make-invader 10 10 -3))-3)) ; in middle, continue current direction

(check-expect (move-invader (make-invader WIDTH 10 3))
              (make-invader (- WIDTH (image-width INVADER))
                            (move-iy (make-invader WIDTH 10 3)) -3)); at left edge, switch direction

(check-expect (move-invader (make-invader 0 10 -3))
              (make-invader 0 (move-iy (make-invader 0 10 -3)) 3)); at right edge, switch direction

; (define (move-invader I1) I2) ;stub
;<template from Invader>

(define (move-invader invader)
  (cond [(> (move-ix invader) (- WIDTH (image-width INVADER)))
         (make-invader (- WIDTH (image-width INVADER)) (move-iy invader) (* -1 (invader-dx invader))) ]
        [(< (move-ix invader)  0)
         (make-invader 0 (move-iy invader) (* -1 (invader-dx invader))) ]
        [else
         (make-invader
   (move-ix invader)
   (move-iy invader)
   (invader-dx invader))]))



;; Invader -> Number
;; produce incremented Invader Y given Invader
(check-expect (move-iy (make-invader 10 10 2))
              (- 10 (* INVADER-Y-SPEED 2)))

; (define (move-iy I1) 0) ;stub
;<template from Invader>

(define (move-iy invader)
  (-  (invader-y invader) (* INVADER-Y-SPEED (abs (invader-dx invader)))))



;; Invader -> Number
;; produce incremented Invader X given Invader
(check-expect (move-ix (make-invader 30 10 2))
              (+ 30 (* INVADER-X-SPEED 2)))

; (define (move-ix I1) 0) ;stub
;<template from Invader>

(define (move-ix invader)
  (+ (invader-x invader) (* INVADER-X-SPEED (invader-dx invader))))



;; listOfMissile listOfInvader -> listOfMissile
;; produce the next listOfMissile after colliding with Invaders and incrementing position
(check-expect (next-missiles (cons M1 (cons M2 empty)) (cons I1 empty))
                             (cons (move-missile M1) (cons empty empty))) ; Second missile hits only invader
(check-expect (next-missiles (cons M2 empty) (cons I2 empty))
              (cons (move-missile M2) empty)) ; No hits

; (define (next-missiles (list M1 M2) (list I1 I2)) (list M1 M2) ;stub
;<template like next-invaders>

(define (next-missiles lom loi)
  (cond [(empty? lom) empty]
        [(empty? (first lom)) (next-missiles (rest lom) loi)]
        [else
         (cons (handle-missile (first lom) loi)
              (next-missiles (rest lom) loi))]))



;; Missile listOfInvader -> Missile
;; produce Invader with incremented position if not hit by missile
(check-expect (handle-missile M1 (cons I1 empty)) (move-missile M1)) ; no hit, position incremented
(check-expect (handle-missile M2 (cons I1 empty)) empty) ; missile hit invader
(check-expect (handle-missile M2 (list I1 I2 I3)) empty) ; missile hit invader in list
(check-expect (handle-missile (make-missile 10 505) empty) (make-missile 10 515)) ; missle flew off screen 

; (define (handle-missile M1 (list I1 I2)) M2) ;stub
;<template from loi>

(define (handle-missile missile loi)
    (cond [(missile-collision? missile loi) empty] ; missile collided
          [(< (- HEIGHT 20) (missile-y missile)) empty]      ; missile off screen
          [else (move-missile missile)]))          ; move missile



;; Missile listOfInvader -> boolean
;; produce true if Invader is hit by any missile in list
(check-expect (missile-collision? M1 (cons I1 empty)) false)
(check-expect (missile-collision? M2 (cons I1 empty)) true)
(check-expect (missile-collision? M3 (cons I1 empty)) true)
(check-expect (missile-collision? M1 (cons empty (cons I1 empty))) false)

; (define (missile-collission? M1 (list I1 I2)) true) ;stub
;<template from data def>

(define (missile-collision? missile loi)
  (cond [(empty? loi) false]
        [(within-radius? (first loi) missile) true]
        [else (missile-collision? missile (rest loi))]))



;; Missile -> Missile
;; produce Missile with incremented position
(check-expect (move-missile M1) (make-missile (missile-x M1) (+ (missile-y M1) MISSILE-SPEED)))

; (define (move-missile M1) M2);
#; 
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define (move-missile m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))
  


;; Game -> Image
;; produce an image of the current game state
(check-expect (render G1)
              (overlay-invaders (game-invaders G1)
                                (overlay-tank (game-tank G1)
                                              (overlay-missiles (game-missiles G1) MTS)))) 

; (define (render g) MTS) ;stub
;<template from data def>

(define (render g)
  (overlay-invaders (game-invaders g)
                    (overlay-tank (game-tank g)
                                      (overlay-missiles (game-missiles g) MTS))))



;; listOfInvader Image -> Image
;; produce image of Invaders on top of given Image
(check-expect (overlay-invaders (cons I1 empty) MTS) (overlay/align/offset "right" "bottom" INVADER (invader-x I1) (invader-y I1) MTS))
(check-expect (overlay-invaders empty MTS) MTS)

; (define (overlay-invaders (list I1 I2) MTS) MTS) ;stub
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

(define (overlay-invaders loi img)
  (cond [(empty? loi) img]
        [(empty? (first loi)) (overlay-invaders (rest loi) img)]
        [else
         (overlay/align/offset "right" "bottom" INVADER
                               (invader-x (first loi)) (invader-y (first loi))
                               (overlay-invaders (rest loi) img))]))

  

;; listOfMissile Image -> Image
;; add Missiles to Image
(check-expect (overlay-missiles (cons M1 (cons M2 empty)) MTS)
              (overlay/align/offset "right" "bottom" MISSILE
                                    (missile-x M1) (missile-y M1)
                                    (overlay/align/offset "right" "bottom" MISSILE
                                                    (missile-x M2) (missile-y M2)
                                                    MTS)))

; (define (overlay-missiles (list M1 M2) MTS) MTS) ;stub
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define (overlay-missiles lom img)
  (cond [(empty? lom) img]
        [(empty? (first lom)) (overlay-missiles (rest lom) img)]
        [else
         (overlay/align/offset "right" "bottom" MISSILE
                               (missile-x (first lom)) (missile-y (first lom))
                               (overlay-missiles (rest lom) img))]))

   
;; Tank Image -> Image
;; add Tank to background Image
(check-expect (overlay-tank (make-tank 10 10) MTS)
              (overlay/align/offset "right" "bottom"
                                    TANK (- 10 (/ (image-width TANK) 2)) 0 MTS))

; (define (overlay-tank T1 MTS) MTS) ;stub
;<template from Tank>

(define (overlay-tank tank mts)
  (overlay/align/offset "right" "bottom"
                        TANK (- (tank-x tank) (/ (image-width TANK) 2)) 0 mts))



;; Game -> Boolean
;; stop the game if an invader reaches the bottom
(check-expect (end? (make-game (list (make-invader 10 -1 1)) empty T1)) true)
(check-expect (end? (make-game (list (make-invader 10 0 1)) empty T1)) true)
(check-expect (end? (make-game empty empty T1)) false)

; (define (end? G0) true) ;stub
;<template from Game>

(define (end? g)
  (invader-bottom? (game-invaders g)))



;; listOfInvader -> Boolean
;; return true if any invaders reach the bottom
(check-expect (invader-bottom? (list I1 empty)) false)
(check-expect (invader-bottom? (list (make-invader 10 0 1))) true)
(check-expect (invader-bottom? empty) false)

; (define (invader-bottom? (list I1 I2)) true) ;stub
;<template from Invader>

(define (invader-bottom? loi)
  (cond [(empty? loi) false]
        [(empty? (first loi)) false]
        [(>= 0 (invader-y (first loi))) true]
        [else (invader-bottom? (rest loi))]))



;; Game key-event -> Game
;; If keypress is space or arrow, fire missile or change tank direction
(check-expect (handle-key G0 " ") (fire-missile G0))
(check-expect (handle-key G0 "left")  (left-tank G0))
(check-expect (handle-key G0 "right") (right-tank G0))
(check-expect (handle-key G0 "b") G0)

; (define (handle-key G0 " ") G0) ;stub
;<template from Game>

(define (handle-key g key)
  (cond [(key=? key " ") (fire-missile g)]
        [(key=? key "left") (left-tank g)]
        [(key=? key "right") (right-tank g)]
        [else g]))

;; Game -> Game
;; Produce Game with missile fired at current tank position
(check-expect (fire-missile G0) (make-game empty (list (make-missile (tank-x (game-tank G0)) 5)) (game-tank G0)))

; (define (fire-missile G0) G0) ;stub
;<template from Game>

(define (fire-missile g)
     (make-game 
     (game-invaders g)
         (cons (make-missile (tank-x (game-tank g)) 5) (game-missiles g))
       (game-tank g)))



;; Game -> Game
;; Produce Game with tank moving left
(check-expect (left-tank G0) (make-game empty empty (make-tank (tank-x (game-tank G0)) (- (abs (tank-dir (game-tank G0)))))))

; (define (left-tank G0) G0) ;stub
;<template from Game>

 (define (left-tank g)
     (make-game 
     (game-invaders g)
         (game-missiles g)
       (make-tank (tank-x (game-tank g)) (- (abs (tank-dir (game-tank g)))))))



;; Game -> Game
;; Produce Game with tank moving right
(check-expect (right-tank G0) (make-game empty empty (make-tank (tank-x (game-tank G0)) (abs (tank-dir (game-tank G0))))))

; (define (right-tank G0) G0) ;stub
;<template from Game>

(define (right-tank g)
     (make-game 
     (game-invaders g)
         (game-missiles g)
       (make-tank (tank-x (game-tank g)) (abs (tank-dir (game-tank g))))))
