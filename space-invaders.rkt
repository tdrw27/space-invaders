;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
  
;; Space Invaders
  
  
;; Constants:
  
(define WIDTH  300)
(define HEIGHT 500)
(define CENTER-W (/ WIDTH  2))
(define CENTER-H (/ HEIGHT 2))
  
  
(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)
  
(define HIT-RANGE 10)
  
(define INVADE-RATE 100)
  
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))
  
(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
  
(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "outline" "green")       ;gun
                     (rectangle 20 10 "outline" "green"))))   ;main body
  
(define BARRELH 14)
  
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-GROUND (- HEIGHT TANK-HEIGHT/2))
  
(define MISSILE (ellipse 5 15 "solid" "gold"))
  
  
  
;; Data Definitions:
  
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position
  
;; Game constants defined below Missile data definition
  
#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))
  
  
  
(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1
  
(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
;; for testing:
(define TT1 (make-tank WIDTH 1)) ;touching right wall moving right
(define TT2 (make-tank 0 -1))    ;touching left wall moving left
  
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))
  
  
  
(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick
  
(define I1 (make-invader 150 100 10))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
  
  
#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))
  
  
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates
  
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
  
#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))
  
  
  
;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missle ListOfMissile)
;; interp. a list of missiles
  
(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M1 (cons (make-missile 200 250) empty)))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))
  
  
  
;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders
  
(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I1 (cons (make-invader 175 125 10) empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))
  
  
  
  
  
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
  
;; ================================================================================
  
;; Functions:
  
;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                        ; Game
    (name "Space Invaders")
    (on-tick   update)               ; Game -> Game
    (to-draw   render)               ; Game -> Image
    (on-key    move-shoot-tank)      ; Game KeyEvent -> Game
    (stop-when invader-landed?)))    ; Game -> Boolean
  
;; Game -> Game
;; produce the next tick of the game updating game state
;(check-expect (update G0) (make-game empty empty
;                                     (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
;(check-expect (update G1) (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (update G2) (make-game (update-invaders (list I1))
                                     (update-missiles (list M1))
                                     (update-tank T1)))
;(check-expect (update G3) (make-game (update-invaders (list I1 I2))
;                                     (update-missiles (list M1 M2))
;                                     (update-tank T1)))
  
;(define (update g) g)
  
(define (update g)
  (check-hits (make-game (spawn (update-invaders (game-invaders g)))
                         (filter-missiles   (update-missiles (game-missiles g)))
                         (update-tank                        (game-tank g)))))
  
  
;; Game -> Image
;; render state of the game
(check-expect (render G0) (place-image TANK 150 TANK-GROUND BACKGROUND))
(check-expect (render G2) (place-image INVADER 150 100
                                       (place-image MISSILE 150 300
                                                    (place-image TANK 50 TANK-GROUND BACKGROUND))))
(check-expect (render G3) (place-image INVADER 150 100
                                       (place-image INVADER 150 500
                                                    (place-image MISSILE 150 300
                                                                 (place-image MISSILE 150 110
                                                                              (place-image TANK 50 TANK-GROUND BACKGROUND))))))
                                                                                
  
(define (render g)
  (render-loi-on (game-invaders g) (render-lom-on (game-missiles g) (render-tank (game-tank g)))))
  
  
;; ListOfInvader Image -> Image
;; render list of invaders on given image
(check-expect (render-loi-on LOI0 empty-image) empty-image)
(check-expect (render-loi-on LOI1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
  
                                               
;(define (render-loi-on loi) loi)
  
(define (render-loi-on loi img)
  (cond [(empty? loi) img]
        [else
         (overlay (render-invader-on (first loi)
                                     (render-loi-on (rest loi) img))
                  img)]))
  
  
;; Invader Image -> Image
;; produce image of invader on given image
(check-expect (render-invader-on I1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invader-on I2 BACKGROUND) (place-image INVADER 150 500 BACKGROUND))
  
;(define (render-invader-on i img) img)
  
(define (render-invader-on i img)
  (place-image INVADER (invader-x i) (invader-y i) img))
  
  
;; ListOfMissile -> Image
;; render list of missiles on screen
  
(define (render-lom-on lom img)
  (cond [(empty? lom) img]
        [else
         (overlay (render-missile-on (first lom)
                                     (render-lom-on (rest lom) img))
                  img)]))
  
;; Missile Image -> Image
;; render missile on given image
  
(define (render-missile-on m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))
  
  
;; ListOfMissile -> ListOfMissile
;; filters missiles offscreen out
(check-expect (filter-missiles (cons M1 (cons (make-missile 100 -10) empty))) (cons M1 empty))
  
(define (filter-missiles lom)
  (cond [(empty? lom) lom]
        [else
         (if (offscreen? (first lom))
             (filter-missiles (rest lom))
             (cons (first lom) (filter-missiles (rest lom))))]))
  
;; Missle -> Boolean
;; determines if missile is offscreen
(check-expect (offscreen? (make-missile 100 -10)) true)
(check-expect (offscreen? (make-missile 100 10)) false)
  
(define (offscreen? m)
  (if (< (missile-y m) 0)
      true
      false))
  
  
;; Tank -> Image
;; render tank on screen
  
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-GROUND BACKGROUND))
  
  
;; Image Image Image Image -> Image
;; render all images of invader, missile and tank onto background
  
(define (render-on iimg mimg timg bimg)
  (place-image iimg CENTER-W CENTER-H
               (place-image mimg CENTER-W CENTER-H
                            (place-image timg CENTER-W CENTER-H bimg)))) 
    
  
  
  
;; Game KeyEvent -> Game
;; move the tank left on press of left arrow and right on press of right arrow
(check-expect (move-shoot-tank (make-game empty empty T0) "left")   ;moving right, change
              (make-game empty empty (make-tank 150 -1)))
(check-expect (move-shoot-tank (make-game empty empty T0) "right")  ;moving right, don't change
              (make-game empty empty (make-tank 150 1)))
(check-expect (move-shoot-tank (make-game empty empty T2) "right")  ;moving left,  change
              (make-game empty empty (make-tank 50 1)))
(check-expect (move-shoot-tank (make-game empty empty T2) "left")   ;moving left,  don't change
              (make-game empty empty (make-tank 50 -1)))
  
;(define (move-shoot-tank g ke) g)
  
(define (move-shoot-tank g ke)
  (cond[(and (key=? ke "right") (< (tank-dir (game-tank g)) 0))
        (make-game (game-invaders g)
                   (game-missiles g)
                   (make-tank (tank-x (game-tank g)) (- (tank-dir (game-tank g)))))]
       [(and (key=? ke "left") (> (tank-dir (game-tank g)) 0))
        (make-game (game-invaders g)
                   (game-missiles g)
                   (make-tank (tank-x (game-tank g)) (- (tank-dir (game-tank g)))))]
       [(key=? ke " ")
        (make-game (game-invaders g)
                   (new-missile (game-missiles g) (game-tank g))
                   (game-tank g))]
       [else
        g]))
  
  
;; ListOfMissile Tank -> ListOfMissile
;; produce new missile at tank location
(check-expect (new-missile LOM1 T0) (cons (make-missile 150 (- HEIGHT BARRELH)) LOM1))
  
(define (new-missile lom t)
  (cond [(empty? lom) (cons (make-missile (tank-x t) (- HEIGHT BARRELH)) lom)]
        [else
         (cons (make-missile (tank-x t) (- HEIGHT BARRELH)) lom)]))
  
  
  
  
  
;; ListOfInvader -> ListOfInvader
;; updates the position (x/dx) and status(hit or not hit by missile) of invaders
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders LOI1) (cons (make-invader (+ 150 INVADER-X-SPEED)
                                                         (+ 100 INVADER-Y-SPEED)
                                                         10) empty))
(check-expect (update-invaders LOI2) (cons (make-invader (+ 150 INVADER-X-SPEED)
                                                         (+ 100 INVADER-Y-SPEED)
                                                         10)
                                           (cons (make-invader (+ 175 INVADER-X-SPEED)
                                                               (+ 125 INVADER-Y-SPEED)
                                                               10) empty)))
(check-expect (update-invaders (cons (make-invader WIDTH 100 10) empty))   ; touching right wall
              (cons (make-invader (- WIDTH INVADER-X-SPEED)                ; switch direction
                                  (+ 100 INVADER-Y-SPEED)
                                  -10) empty))
(check-expect (update-invaders (cons (make-invader 0 100 -10) empty))      ; touching left wall
              (cons (make-invader (+ 0 INVADER-X-SPEED)                    ; switch direction
                                  (+ 100 INVADER-Y-SPEED)
                                  10) empty))
                                       
                
;(define (update-invaders loi) loi)
  
(define (update-invaders loi)
  (cond [(empty? loi) loi]
        [else
         (cons (tick-invader (first loi))
                (update-invaders (rest loi)))]))


;; ListOfInvader -> ListOfInvader
;; Spawn new invader if invader crosses INVADE-RATE Line

;(define (spawn loi) loi)

(define (spawn loi)
  (cond [(empty? loi) (cons (make-invader (random WIDTH) 0 10) empty)]
        [else
         (if (= (modulo (round (invader-y (first loi))) INVADE-RATE) 99)
             (cons (make-invader (random WIDTH) 0 10) loi)
             loi)]))


  
  
  
;; Invader -> Invader
;; moves invader right if dx is positive; left if dx is negative
;; also bounce invader off wall if touching
;; spawns new invader if invader crosses INVADE-RATE line
  
(define (tick-invader i)
  (if (bounce-invader? i)
      (tick-invader (make-invader (invader-x i)
                                  (invader-y i)
                                  (- (invader-dx i))))
      (cond [(> (invader-dx i) 0)
             (make-invader (+ (invader-x i) INVADER-X-SPEED)
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (invader-dx i))]
            [(< (invader-dx i) 0)
             (make-invader (- (invader-x i) INVADER-X-SPEED)
                           (+ (invader-y i) INVADER-Y-SPEED)
                           (invader-dx i))])))
  
  
  
;; Invader -> Boolean
;; checks to see if invader is touching a wall and changes direction if so
;; !!!
(define (bounce-invader? i)
  (cond [(and (>= (invader-x i) WIDTH) (> (invader-dx i) 0))
         true]
        [(and (<= (invader-x i) 0) (< (invader-dx i) 0))
         true]
        [else
         false]))
  
  
  
;; ListOfMissile -> ListOfMissile
;; updates the position of missiles
(check-expect (update-missiles LOM0) empty)
(check-expect (update-missiles LOM1) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (update-missiles LOM2) (cons (make-missile 150 (- 300 MISSILE-SPEED))
                                           (cons (make-missile 200 (- 250 MISSILE-SPEED)) empty)))
  
;(define (update-missiles lom) lom)
  
(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile (first lom))
               (update-missiles (rest lom)))]))
  
  
;; Missile -> Missile
;; produce next tick of missile moving MISSILE-SPEED
  
(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))
  
  
  
;; Tank -> Tank
;; updates the position and direction of tank
(check-expect (update-tank T1) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (update-tank T2) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (update-tank TT1) TT1)
(check-expect (update-tank TT2) TT2)
  
;(define (update-tank t) t)
  
(define (update-tank t)
  (cond [(and (< (tank-x t) WIDTH) (> (tank-dir t) 0))
         (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]
        [(and (> (tank-x t) 0) (< (tank-dir t) 0))
         (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]
        [else
         t]))
  
;; Game -> Game
;; checks for hits between missiles and invaders
;; returns state accordingly
(check-expect (check-hits (make-game LOI1
                                     (list M2)
                                     T0))
              (make-game empty empty T0))
(check-expect (check-hits (make-game (list I1 I2) (list M1 M2) T0))
              (make-game (list I2) (list M1) T0))
;(define (check-hits g) g)
  
(define (check-hits g)
  (make-game (destroy-invaders (game-invaders g) (game-missiles g))
             (destroy-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))
  
;; ListOfInvader ListOfMissile -> ListOfInvader
;; returns list of invaders with hit invaders removed
  
;(define (destroy-invaders loi lom) loi)
  
(define (destroy-invaders loi lom)
  (cond [(empty? loi) loi]
        [else
         (if (was-hit? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))

;; Invader ListOfMissile -> Boolean
;; checks to see if Invader is withing hit range of any missile in list of missiles
(check-expect (was-hit? I1 (list M2)) true)
(check-expect (was-hit? I1 (list (make-missile 200 300))) false)

;(define (was-hit? i lom) false)

(define (was-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (>= (missile-x (first lom)) (- (invader-x i) HIT-RANGE))
                  (<= (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
                  (<= (missile-y (first lom)) (+ (invader-y i) HIT-RANGE))
                  (>= (missile-y (first lom)) (- (invader-y i) HIT-RANGE)))
             true
             (was-hit? i (rest lom)))]))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; returns list of missiles with missiles that hit invaders removed

;(define (destroy-missiles lom loi) lom)

(define (destroy-missiles lom loi)
  (cond [(empty? lom) lom]
        [else
         (if (missile-strike? (first lom) loi)
             (destroy-missiles (rest lom) loi)
             (cons (first lom) (destroy-missiles (rest lom) loi)))]))

;; Missile ListOfInvader -> Boolean
;; checks to see if missile has passed the hit range of any invader in list of invaders

(define (missile-strike? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (>= (missile-x m) (- (invader-x (first loi)) HIT-RANGE))
                  (<= (missile-x m) (+ (invader-x (first loi)) HIT-RANGE))
                  (<= (missile-y m) (+ (invader-y (first loi)) HIT-RANGE))
                  (>= (missile-y m) (- (invader-y (first loi)) HIT-RANGE)))
             true
             (missile-strike? m (rest loi)))]))

;; Game -> Boolean
;; stops the game if an invader touches the ground

(define (invader-landed? g)
  (if (on-ground? (game-invaders g))
      true
      false))

;; ListOfInvader -> Boolean
;; determines if any invader has landed

(define (on-ground? loi)
  (cond [(empty? loi) false]
        [else
         (if (>  (invader-y (first loi)) HEIGHT)
             true
             (on-ground? (rest loi)))]))



