;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

;; Space Invaders

;; =========Wishlist=========

;; Invaders can move ALONG waypoints instead of teleporting between them
;; Waypoint variety
;; More accurate sprites
;; Cooler Background
;; More interactive win and lose handling
;; More interactive interface with options for resetting and quitting
;; Smooth movement for the player
;; Boundary case for player movement, shouldn't move beyond boundary
;; more comprehensive unit-testing
;; Invaders produce missiles

;; =================
;; Data definitions:

(define-struct coord (x y))
;; Coordininate is (make-coord Number Number)
;; interp. represents the coordinates of a point on the screen
(define COORD1 (make-coord 0 0))
(define COORD2 (make-coord 50 100))

#;
(define (fn-for-coord c)
  (... (coord-x c)
       (coord-y c)))

(define-struct waypoint (coord next))
;; Waypoint is (make-waypoint Coordinate ListOfNWayPoint)
;; interp. A graph representing a network of waypoints on the screen where the invaders will move to
;; ASSUME: (waypoint-next waypoint) can never be empty

#;
(define (fn-for-waypoint-graph wp0)
  (local [(define (fn-for-waypoint wp)
            (fn-for-coord (waypoint-coord wp))
            (fn-for-lowp (waypoint-next wp)))

          (define (fn-for-lowp lowp)
            (cond [(empty? lowp) ...]
                  [else
                   (fn-for-waypoint (first lowp))
                   (fn-for-lowp (rest lowp))]))]))


(define-struct invader (coord wp))
;; Invader is (make-invader Coordinate Waypoint)
;; interp. an invader in the game at coordinates coord with waypoint graph wp

#;
(define (fn-for-invader inv)
  (... (fn-for-coord (invader-coord inv))
       (fn-for-waypoint (invader-wp inv))))

(define-struct player (coord))
;; Player is (make-player Cooordinate)
;; interp. the player's ship at coordinates coord

#;
(define (fn-for-player p)
  (fn-for-coord (player-coord p)))

(define-struct invmissile (coord))
;; InvaderMissile is (make-invmissile Coordinate)
;; interp. a missile fired by an invader
(define IMISSILE1 (make-invmissile (make-coord 200 400)))
(define IMISSILE2 (make-invmissile (make-coord 220 420)))
(define IMISSILESLIST1 (list IMISSILE1 IMISSILE2))
(define IMISSILESLIST2 empty)

#;
(define (fn-for-inv-missile invmis)
  (fn-for-coord (invmissile-coord invmis)))

(define-struct missile (coord))
;; Missile is (make-missile Coordinate)
;; interp. a missile fired by the player
(define MISSILE1 (make-missile (make-coord 100 300)))
(define MISSILE2 (make-missile (make-coord 105 280)))
(define MISSILE3 (make-missile (make-coord 110 260)))
(define MISSILELIST1 (list MISSILE1 MISSILE2 MISSILE3))
(define MISSILELIST2 empty)

#;
(define (fn-for-missile mis)
  (fn-for-coord (missile-coord mis)))

;; Game is one of:
;; - "over"
;; - (make-game Player ListofInvader ListofMissile ListofInvaderMissile ListofString)
;; - "won"
(define-struct game (player loi lom loim))
;; interp. the running state of a game including the player and the currently in play invaders and missiles.

#;
(define (fn-for-game game)
  (cond [(and (string? game) (string=? game "over")) ...]
        [(and (string? game) (string=? game "won")) ...]
        [else
         (... (fn-for-player (game-player game))
              (fn-for-loinv (game-loi game))
              (fn-for-lom (game-lom game))
              (fn-for-loinvmis (game-loim game)))]))

(define-struct loi-lom-pair (loi lom))
;; PairofListofInvaderListofMissile is (make-loi-lom-pair ListofInvader ListofMissile)

;; =================
;; Constants:

;; Background
(define BACKGROUND-WIDTH 1000)
(define BACKGROUND-HEIGHT 1000)
(define CTR-X (/ BACKGROUND-WIDTH 2))
(define CTR-Y (/ BACKGROUND-HEIGHT 2))
(define MTS (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

;; Colors
(define TILE-COLOR1 (make-color 236 229 207)) ; light beige-cream
(define TILE-COLOR2 (make-color 213 201 174)) ; darker tan

;; Base square tile
(define TILE-SIZE 40)
(define TILE-RADIUS (* TILE-SIZE (cos (/ pi 4))))
(define TILE1 (rotate 45 (square TILE-SIZE "solid" TILE-COLOR1)))
(define TILE2 (rotate 45 (square TILE-SIZE "solid" TILE-COLOR2)))

;; Background Generation

;; Number Image -> Image
;; generates a row of k tiles with a single colour
(define (draw-tiles-row k tile)
  (if (equal? k 1)
      tile
      (beside tile (draw-tiles-row (sub1 k) tile))))

;; Number Number Image -> Image
;; generates a grid of j.k tiles with a single colour
(define (draw-tiles-grid j k tile)
  (if (equal? j 1)
      (draw-tiles-row k tile)
      (above (draw-tiles-row k tile) (draw-tiles-grid (sub1 j) k tile))))

;; Number Number -> Image
;; generates the staggered diagonal offset on tiles of two colours
(define (draw-background j k)
  (overlay/offset (draw-tiles-grid j k TILE1)
                  0
                  0
                  (draw-tiles-grid (+ j 1) (+ k 1) TILE2)))

(define BACKGROUND (place-image (draw-background 18 18) CTR-X CTR-Y MTS))

;; Sprites
(define PROTAG-ORIG (bitmap "assets/protagonist.png"))
(define INVADER-ORIG (bitmap "assets/invader.png"))
(define PROTAG-SCALE 0.1)
(define INVADER-SCALE 0.1)
(define PROTAG (scale PROTAG-SCALE PROTAG-ORIG))
(define INVADER (scale INVADER-SCALE INVADER-ORIG))

(define MISSILE-WIDTH 3)
(define MISSILE-HEIGHT 15)
(define MISSILE-ALLY (rectangle MISSILE-WIDTH MISSILE-HEIGHT "solid" "blue"))
(define MISSILE-ENEMY (rectangle MISSILE-WIDTH MISSILE-HEIGHT "solid" "red"))

;; Waypoint Helpers
(define PENTAGON-TOP-PADDING (+ (image-height INVADER) 5))
(define PENTAGON-RIGHT-PADDING (+ (image-width INVADER) 5))
(define PENTAGON-LEFT-PADDING (+ (image-width INVADER) 5))
(define PENTAGON-INNER-RADIUS 100) 
(define PENTAGON-OUTER-RADIUS 250)
(define PENTAGON-CENTER (make-coord CTR-X
                                    (+ PENTAGON-TOP-PADDING
                                       PENTAGON-OUTER-RADIUS)))

;; Helper to get coordinates for a regular polygon
(define (polygon-coords center radius sides)
  (local [(define angle-step (/ (* 2 pi) sides))
          (define start-angle (/ pi 2))]
    (for/list ([i (in-range sides)])
      (make-coord
       (+ (coord-x center) (* radius (cos (- start-angle (* i angle-step)))))
       (+ (coord-y center) (* radius (sin (- start-angle (* i angle-step)))))))))

;; Generate coordinates
(define INNER-PENTAGON (polygon-coords PENTAGON-CENTER PENTAGON-INNER-RADIUS 5))
(define OUTER-PENTAGON (polygon-coords PENTAGON-CENTER PENTAGON-OUTER-RADIUS 5))

;; Inner Pentagon
(define I1 (list-ref INNER-PENTAGON 0))
(define I2 (list-ref INNER-PENTAGON 1))
(define I3 (list-ref INNER-PENTAGON 2))
(define I4 (list-ref INNER-PENTAGON 3))
(define I5 (list-ref INNER-PENTAGON 4))

;; Outer Pentagon
(define O1 (list-ref OUTER-PENTAGON 0))
(define O2 (list-ref OUTER-PENTAGON 1))
(define O3 (list-ref OUTER-PENTAGON 2))
(define O4 (list-ref OUTER-PENTAGON 3))
(define O5 (list-ref OUTER-PENTAGON 4))

;; Test for Pentagons
(define (draw-points scene points)
  (foldl (lambda (p img)
           (place-image (rectangle 50 50 "solid" "blue")
                        (coord-x p)
                        (coord-y p)
                        img))
         scene
         points))

(define all-points (append INNER-PENTAGON OUTER-PENTAGON))
(define test-scene (draw-points MTS all-points))

;; Waypoint Graphs
(define WP1
  (shared ((-I1- (make-waypoint I1 (list -I2- -O2-)))
           (-I2- (make-waypoint I2 (list -I3- -O3-)))
           (-I3- (make-waypoint I3 (list -I4- -O4-)))
           (-I4- (make-waypoint I4 (list -I5- -O5-)))
           (-I5- (make-waypoint I5 (list -I1- -O1-)))
           (-O1- (make-waypoint O1 (list -O2- -I2-)))
           (-O2- (make-waypoint O2 (list -O3- -I3-)))
           (-O3- (make-waypoint O3 (list -O4- -I4-)))
           (-O4- (make-waypoint O4 (list -O5- -I5-)))
           (-O5- (make-waypoint O5 (list -O1- -I1-))))
    -I1-))
(define WP2
  (shared ((-I1- (make-waypoint I1 (list -I2- -O2-)))
           (-I2- (make-waypoint I2 (list -I3- -O3-)))
           (-I3- (make-waypoint I3 (list -I4- -O4-)))
           (-I4- (make-waypoint I4 (list -I5- -O5-)))
           (-I5- (make-waypoint I5 (list -I1- -O1-)))
           (-O1- (make-waypoint O1 (list -O2- -I2-)))
           (-O2- (make-waypoint O2 (list -O3- -I3-)))
           (-O3- (make-waypoint O3 (list -O4- -I4-)))
           (-O4- (make-waypoint O4 (list -O5- -I5-)))
           (-O5- (make-waypoint O5 (list -O1- -I1-))))
    -O2-))
(define WP3
  (shared ((-I1- (make-waypoint I1 (list -I2- -O2-)))
           (-I2- (make-waypoint I2 (list -I3- -O3-)))
           (-I3- (make-waypoint I3 (list -I4- -O4-)))
           (-I4- (make-waypoint I4 (list -I5- -O5-)))
           (-I5- (make-waypoint I5 (list -I1- -O1-)))
           (-O1- (make-waypoint O1 (list -O2- -I2-)))
           (-O2- (make-waypoint O2 (list -O3- -I3-)))
           (-O3- (make-waypoint O3 (list -O4- -I4-)))
           (-O4- (make-waypoint O4 (list -O5- -I5-)))
           (-O5- (make-waypoint O5 (list -O1- -I1-))))
    -I3-))
(define WP4
  (shared ((-I1- (make-waypoint I1 (list -I2- -O2-)))
           (-I2- (make-waypoint I2 (list -I3- -O3-)))
           (-I3- (make-waypoint I3 (list -I4- -O4-)))
           (-I4- (make-waypoint I4 (list -I5- -O5-)))
           (-I5- (make-waypoint I5 (list -I1- -O1-)))
           (-O1- (make-waypoint O1 (list -O2- -I2-)))
           (-O2- (make-waypoint O2 (list -O3- -I3-)))
           (-O3- (make-waypoint O3 (list -O4- -I4-)))
           (-O4- (make-waypoint O4 (list -O5- -I5-)))
           (-O5- (make-waypoint O5 (list -O1- -I1-))))
    -O4-))
(define WP5
  (shared ((-I1- (make-waypoint I1 (list -I2- -O2-)))
           (-I2- (make-waypoint I2 (list -I3- -O3-)))
           (-I3- (make-waypoint I3 (list -I4- -O4-)))
           (-I4- (make-waypoint I4 (list -I5- -O5-)))
           (-I5- (make-waypoint I5 (list -I1- -O1-)))
           (-O1- (make-waypoint O1 (list -O2- -I2-)))
           (-O2- (make-waypoint O2 (list -O3- -I3-)))
           (-O3- (make-waypoint O3 (list -O4- -I4-)))
           (-O4- (make-waypoint O4 (list -O5- -I5-)))
           (-O5- (make-waypoint O5 (list -O1- -I1-))))
    -I5-))

;; Invaders
(define INVADER1 (make-invader I1 WP1))
(define INVADER2 (make-invader O2 WP2))
(define INVADER3 (make-invader I3 WP3))
(define INVADER4 (make-invader O4 WP4))
(define INVADER5 (make-invader I5 WP5))
(define STARTING-INVADERS (list INVADER1 INVADER2 INVADER3 INVADER4 INVADER5))

;; Player
(define PLAYER (make-player (make-coord CTR-X (- BACKGROUND-HEIGHT (* 0.1 BACKGROUND-HEIGHT)))))
(define PLAYER-SPEED 10)

;; Missiles
(define MISSILE-SPEED 20)

;; Game
(define STARTING-STATE (make-game PLAYER STARTING-INVADERS empty empty ))
(define GAME-TEST
  (make-game PLAYER
             STARTING-INVADERS
             MISSILELIST1
             IMISSILESLIST1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main STARTING-STATE)
;; Tune BACKGROUND-WIDTH, BACKGROUND-HEIGHT, 
(define (main game)
  (big-bang game                   ; Game
    (on-tick   get-next-game 0.5)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (on-key    handle-key)))    ; Game KeyEvent -> WS

;; Game -> Game
;; produce the next game state
(define (get-next-game game)
  (cond [(string? game) game]
        [else
         (cond [(and (not (empty? (game-loim game))) (in-player-bounding-box? (first (game-loim game)) (game-player game))) "over"]
               [(empty? (game-loi game)) "won"]
               [else
                (local [(define next-inv-mis (next-invaders-and-missiles (game-loi game) (game-lom game)))]
                (make-game (game-player game)
                           (loi-lom-pair-loi next-inv-mis)
                           (next-lom (loi-lom-pair-lom next-inv-mis))
                           (game-loim game)))])]))

;; Missile Player -> boolean
;; produces true if the center of the missile lies in the bounding box of the player-sprite
(check-expect (in-player-bounding-box? IMISSILE1 PLAYER) false)
(check-expect (in-player-bounding-box? IMISSILE2 PLAYER) false)
(define IMISSILE-CENTER (make-invmissile (player-coord PLAYER)))
(check-expect (in-player-bounding-box? IMISSILE-CENTER PLAYER) true)
(define IMISSILE-LEFT-EDGE
  (make-invmissile
   (make-coord (- (coord-x (player-coord PLAYER))
                  (/ (image-width PROTAG) 2))
               (coord-y (player-coord PLAYER)))))
(check-expect (in-player-bounding-box? IMISSILE-LEFT-EDGE PLAYER) true)
(define IMISSILE-RIGHT-EDGE
  (make-invmissile
   (make-coord (+ (coord-x (player-coord PLAYER))
                  (/ (image-width PROTAG) 2))
               (coord-y (player-coord PLAYER)))))
(check-expect (in-player-bounding-box? IMISSILE-RIGHT-EDGE PLAYER) true)
(define IMISSILE-TOP-EDGE
  (make-invmissile
   (make-coord (coord-x (player-coord PLAYER))
               (- (coord-y (player-coord PLAYER))
                  (/ (image-height PROTAG) 2)))))
(check-expect (in-player-bounding-box? IMISSILE-TOP-EDGE PLAYER) true)
(define IMISSILE-BOTTOM-EDGE
  (make-invmissile
   (make-coord (coord-x (player-coord PLAYER))
               (+ (coord-y (player-coord PLAYER))
                  (/ (image-height PROTAG) 2)))))
(check-expect (in-player-bounding-box? IMISSILE-BOTTOM-EDGE PLAYER) true)
(define (in-player-bounding-box? invmis player)
  (local [(define player-left-boundary (- (coord-x (player-coord player)) (/ (image-width PROTAG) 2)))
          (define player-right-boundary (+ (coord-x (player-coord player)) (/ (image-width PROTAG) 2)))
          (define player-top-boundary (- (coord-y (player-coord player)) (/ (image-height PROTAG) 2)))
          (define player-bottom-boundary (+ (coord-y (player-coord player)) (/ (image-height PROTAG) 2)))
          (define mx (coord-x (invmissile-coord invmis)))
          (define my (coord-y (invmissile-coord invmis)))]
    (and (>= mx player-left-boundary)
         (<= mx player-right-boundary)
         (>= my player-top-boundary)
         (<= my player-bottom-boundary))))

;; !!!
;; ListofInvader ListofMissile -> PairofListofInvaderListofMissile
;; produces the filtered lists of invaders and missiles after collisions
(define (next-invaders-and-missiles loi lom)
  (local [(define moved-invaders (move-invaders loi))]
    (filter-invaders-and-missiles moved-invaders lom moved-invaders)))

;; ListofInvader ListofMissile -> PairofListofInvaderListofMissile
;; filters out any collided invaders and missiles

;; missile that misses everything
(define MISSILE-MISS (make-missile (make-coord 999 999)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-MISS) STARTING-INVADERS)
 (make-loi-lom-pair STARTING-INVADERS (list MISSILE-MISS)))

;; missile hits INVADER1
(define MISSILE-HIT-INV1 (make-missile (invader-coord INVADER1)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-INV1) STARTING-INVADERS)
 (make-loi-lom-pair (remove INVADER1 STARTING-INVADERS) empty))

;; missile hits INVADER3
(define MISSILE-HIT-INV3 (make-missile (invader-coord INVADER3)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-INV3) STARTING-INVADERS)
 (make-loi-lom-pair (remove INVADER3 STARTING-INVADERS) empty))

;; two missiles hit INVADER2 and INVADER4
(define MISSILE-HIT-INV2 (make-missile (invader-coord INVADER2)))
(define MISSILE-HIT-INV4 (make-missile (invader-coord INVADER4)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-INV2 MISSILE-HIT-INV4) STARTING-INVADERS)
 (make-loi-lom-pair (list INVADER1 INVADER3 INVADER5) empty))

;; mixed case: one missile hits, one misses
(define MISSILE-HIT-INV5 (make-missile (invader-coord INVADER5)))
(define MISSILE-MISS-2 (make-missile (make-coord 2000 2000)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-INV5 MISSILE-MISS-2) STARTING-INVADERS)
 (make-loi-lom-pair (remove INVADER5 STARTING-INVADERS) (list MISSILE-MISS-2)))

;; all missiles hit all invaders
(define MISSILES-ALL
  (map (lambda (inv) (make-missile (invader-coord inv))) STARTING-INVADERS))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS MISSILES-ALL STARTING-INVADERS)
 (make-loi-lom-pair empty empty))

;; no missiles at all
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS empty STARTING-INVADERS)
 (make-loi-lom-pair STARTING-INVADERS empty))

;; one missile misses completely → all invaders remain, missile stays
(define MISSILE-OFFSCREEN (make-missile (make-coord 5000 5000)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-OFFSCREEN) STARTING-INVADERS)
 (make-loi-lom-pair STARTING-INVADERS (list MISSILE-OFFSCREEN)))

;; one missile hits INVADER2, another misses → INVADER2 removed, miss missile remains
(define MISSILE-HIT-2 (make-missile (invader-coord INVADER2)))
(define MISSILE-MISS-3 (make-missile (make-coord -999 -999)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-2 MISSILE-MISS-3) STARTING-INVADERS)
 (make-loi-lom-pair (remove INVADER2 STARTING-INVADERS) (list MISSILE-MISS-3)))

;; two missiles both miss → all invaders remain, both missiles remain
(define MISSILE-MISS-A (make-missile (make-coord 1500 1000)))
(define MISSILE-MISS-B (make-missile (make-coord 800 2000)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-MISS-A MISSILE-MISS-B) STARTING-INVADERS)
 (make-loi-lom-pair STARTING-INVADERS (list MISSILE-MISS-A MISSILE-MISS-B)))

;; mix of multiple hits and misses
(define MISSILE-HIT-4 (make-missile (invader-coord INVADER4)))
(define MISSILE-HIT-5 (make-missile (invader-coord INVADER5)))
(define MISSILE-MISS-C (make-missile (make-coord 9999 9999)))
(check-expect
 (filter-invaders-and-missiles STARTING-INVADERS (list MISSILE-HIT-4 MISSILE-HIT-5 MISSILE-MISS-C) STARTING-INVADERS)
 (make-loi-lom-pair (list INVADER1 INVADER2 INVADER3) (list MISSILE-MISS-C)))

(define (filter-invaders-and-missiles loi lom inv-remaining)
  ;; inv-remaining: result-so-far accumulator, preserves all remaining invaders
  (cond [(or (empty? loi) (empty? lom)) (make-loi-lom-pair inv-remaining lom)]
        [else
         (local [;; Invader Missile -> Missile
                 ;; checks if the bounding box of mis lies in the bounding box of inv
                 (define (collision? inv mis)
                   (local [(define inv-left   (- (coord-x (invader-coord inv)) (/ (image-width INVADER) 2)))
                           (define inv-right  (+ (coord-x (invader-coord inv)) (/ (image-width INVADER) 2)))
                           (define inv-top    (- (coord-y (invader-coord inv)) (/ (image-height INVADER) 2)))
                           (define inv-bottom (+ (coord-y (invader-coord inv)) (/ (image-height INVADER) 2)))]
                     (and (>= (coord-x (missile-coord mis)) inv-left)
                          (<= (coord-x (missile-coord mis)) inv-right)
                          (>= (coord-y (missile-coord mis)) inv-top)
                          (<= (coord-y (missile-coord mis)) inv-bottom))))
                 
                 ;; Invader ListofMissile -> ListofMissile
                 ;; if there is a collision between inv and a missile from the list, remove that missile from the list
                 ;; mis-remaining: result-so-far accumulator, preserves all remaining missiles
                 (define (collision-result inv lom mis-remaining)
                   (if (empty? lom)
                       mis-remaining
                       (if (collision? inv (first lom))
                           (remove (first lom) mis-remaining)
                           (collision-result inv (rest lom) mis-remaining))))
                 
                 (define remaining-lom (collision-result (first loi) lom lom))]
         
         (if (equal? remaining-lom lom)
             (filter-invaders-and-missiles (rest loi) lom inv-remaining)
             (filter-invaders-and-missiles (rest loi) remaining-lom (remove (first loi) inv-remaining))))]))

;; ListofInvader -> ListofInvader
;; Performs move-invader on all Invader in loi
(check-expect
 (member (invader-coord (first (move-invaders (list INVADER1))))
         (map waypoint-coord (waypoint-next WP1)))
 true)
(check-expect
 (member (invader-coord (first (move-invaders STARTING-INVADERS)))
         (map waypoint-coord (waypoint-next WP1)))
 true)

(check-expect
 (member (invader-coord (second (move-invaders STARTING-INVADERS)))
         (map waypoint-coord (waypoint-next WP2)))
 true)

(check-expect
 (member (invader-coord (third (move-invaders STARTING-INVADERS)))
         (map waypoint-coord (waypoint-next WP3)))
 true)

(check-expect
 (member (invader-coord (fourth (move-invaders STARTING-INVADERS)))
         (map waypoint-coord (waypoint-next WP4)))
 true)

(check-expect
 (member (invader-coord (fifth (move-invaders STARTING-INVADERS)))
         (map waypoint-coord (waypoint-next WP5)))
 true)
(define (move-invaders loi)
  (if (empty? loi)
      loi
      (cons (move-invader (first loi))
            (move-invaders (rest loi)))))

;; Invader -> Invader
;; Moves an invader to randomly to one of the points the current in the graph leads to
(check-expect (member (invader-wp (move-invader INVADER1))
                      (waypoint-next (invader-wp INVADER1)))
              true)
(check-expect (member (invader-coord (move-invader INVADER1))
                      (map waypoint-coord
                           (waypoint-next (invader-wp INVADER1))))
              true)
(define (move-invader inv)
  (local [(define destination (next-waypoint (invader-wp inv)))]
    (make-invader (waypoint-coord destination)
                  destination)))

;; Waypoint -> Waypoint
;; produces a random child waypoint
(check-expect (member (next-waypoint WP1)
                      (waypoint-next WP1))
              true)
(define (next-waypoint wp)
  (get-random-waypoint (waypoint-next wp)))

;; ListofWaypoint -> Waypoint
;; produces a random Waypoint from lowp
(check-expect (member (get-random-waypoint (waypoint-next WP1))
                      (waypoint-next WP1))
              true)

(define (get-random-waypoint lowp)
  (list-ref lowp (random (length lowp))))

;; ListofMissile -> ListofMissile
;; Moves the missiles upwards by MISSILE-SPEED and filters them out of the game if they reach the screen edge
(define M1 (make-missile (make-coord 50 300)))
(define M2 (make-missile (make-coord 60 20)))
(define M3 (make-missile (make-coord 70 5)))
(define M4 (make-missile (make-coord 80 -5)))
(define LOM-ABC (list M1 M2 M3))
(define LOM-EMPTY empty)
(check-expect (next-lom LOM-EMPTY) empty)
(check-expect
 (next-lom (list (make-missile (make-coord 200 3))))
 empty)
(check-expect
 (next-lom LOM-ABC)
 (list (make-missile (make-coord 50 290))
       (make-missile (make-coord 60 10))))

(define (next-lom lom)
  (if (empty? lom)
      empty
      (filter-lom (move-lom lom))))

;; ListofMissile -> ListofMissile
;; Moves the missiles in the list upwards

;; test assumes SPEED=10
(check-expect
 (move-lom LOM-ABC)
 (list (make-missile (make-coord 50 290))
       (make-missile (make-coord 60 10))
       (make-missile (make-coord 70 -5))))

(check-expect (move-lom LOM-EMPTY) empty)
(define (move-lom lom)
  (map (λ (m)
         (make-missile (make-coord (coord-x (missile-coord m))
                                   (- (coord-y (missile-coord m))
                                      MISSILE-SPEED))))
       lom))

;; ListofMissile -> ListofMissile
;; Filters any missiles outside the screen edge
(check-expect
 (filter-lom (list (make-missile (make-coord 100 10))
                   (make-missile (make-coord 110 -1))))
 (list (make-missile (make-coord 100 10))))

(check-expect (filter-lom LOM-EMPTY) empty)
(define (filter-lom lom)
  (filter (λ (m)
            (>= (coord-y (missile-coord m))
                0))
          lom))

;; !!!
(define (next-invmis loi loim) ...)

;; Game -> Image
;; render the current game state
(check-expect
 (render-game "over")
 (place-image (text "Game Over!" 50 "red")
              CTR-X CTR-Y
              BACKGROUND))
(check-expect
 (render-game "won")
 (place-image (text "Game Won!" 50 "darkgreen")
              CTR-X CTR-Y
              BACKGROUND))
(check-expect
 (render-game GAME-TEST)
 (place-image PROTAG (coord-x (player-coord PLAYER))
              (coord-y (player-coord PLAYER))
              (place-image INVADER (coord-x (invader-coord INVADER1))
                           (coord-y (invader-coord INVADER1))
                           (place-image INVADER (coord-x (invader-coord INVADER2))
                                        (coord-y (invader-coord INVADER2))
                                        (place-image INVADER (coord-x (invader-coord INVADER3))
                                                     (coord-y (invader-coord INVADER3))
                                                     (place-image INVADER (coord-x (invader-coord INVADER4))
                                                                  (coord-y (invader-coord INVADER4))
                                                                  (place-image INVADER (coord-x (invader-coord INVADER5))
                                                                               (coord-y (invader-coord INVADER5))
                                                                               (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE1))
                                                                                            (coord-y (missile-coord MISSILE1))
                                                                                            (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE2))
                                                                                                         (coord-y (missile-coord MISSILE2))
                                                                                                         (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE3))
                                                                                                                      (coord-y (missile-coord MISSILE3))
                                                                                                                      (place-image MISSILE-ENEMY (coord-x (invmissile-coord IMISSILE1))
                                                                                                                                   (coord-y (invmissile-coord IMISSILE1))
                                                                                                                                   (place-image MISSILE-ENEMY (coord-x (invmissile-coord IMISSILE2))
                                                                                                                                                (coord-y (invmissile-coord IMISSILE2))
                                                                                                                                                BACKGROUND))))))))))))
(define (render-game game)
  (cond [(and (string? game) (string=? game "over"))
         (place-image (text "Game Over!" 50 "red")
                      CTR-X CTR-Y
                      BACKGROUND)]
        [(and (string? game) (string=? game "won"))
         (place-image (text "Game Won!" 50 "darkgreen")
                      CTR-X CTR-Y
                      BACKGROUND)]
        [else
         (render-player (game-player game)
                        (render-invaders (game-loi game)
                                         (render-missiles (game-lom game)
                                                          (render-invmissiles (game-loim game)
                                                                              BACKGROUND))))]))


;; Player -> Image
;; renders a player onto a scene
(check-expect
 (render-player PLAYER BACKGROUND)
 (place-image PROTAG
              (coord-x (player-coord PLAYER))
              (coord-y (player-coord PLAYER))
              BACKGROUND))
(define (render-player p scene)
  (place-image PROTAG
               (coord-x (player-coord p))
               (coord-y (player-coord p))
               scene))

;; ListofInvader -> Image
;; renders a list of Invader onto a scene
(check-expect
 (render-invaders STARTING-INVADERS BACKGROUND)
 (place-image INVADER (coord-x (invader-coord INVADER1))
              (coord-y (invader-coord INVADER1))
              (place-image INVADER (coord-x (invader-coord INVADER2))
                           (coord-y (invader-coord INVADER2))
                           (place-image INVADER (coord-x (invader-coord INVADER3))
                                        (coord-y (invader-coord INVADER3))
                                        (place-image INVADER (coord-x (invader-coord INVADER4))
                                                     (coord-y (invader-coord INVADER4))
                                                     (place-image INVADER (coord-x (invader-coord INVADER5))
                                                                  (coord-y (invader-coord INVADER5))
                                                                  BACKGROUND))))))
(define (render-invaders loi rsf)
  ;; rsf: result-so-far accumulator, the invaders rendered so far onto the screen
  (local [(define (render-invader inv scene)
            (place-image INVADER
                         (coord-x (invader-coord inv))
                         (coord-y (invader-coord inv))
                         scene))]
    
    (cond [(empty? loi) rsf]
          [else
           (render-invaders (rest loi)
                            (render-invader (first loi) rsf))])))

;; ListOfMissile -> Image
;; renders a list of player missiles onto a scene
(check-expect (render-missiles MISSILELIST1 BACKGROUND)
              (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE1))
                           (coord-y (missile-coord MISSILE1))
                           (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE2))
                                        (coord-y (missile-coord MISSILE2))
                                        (place-image MISSILE-ALLY (coord-x (missile-coord MISSILE3))
                                                     (coord-y (missile-coord MISSILE3))
                                                     BACKGROUND))))
(define (render-missiles loi rsf)
  ;; rsf: result-so-far accumulator, the missiles rendered so far onto the screen
  (local [(define (render-missile mis scene)
            (place-image MISSILE-ALLY
                         (coord-x (missile-coord mis))
                         (coord-y (missile-coord mis))
                         scene))]
    
    (cond [(empty? loi) rsf]
          [else
           (render-missiles (rest loi)
                            (render-missile (first loi) rsf))])))

;; ListOfInvMissile Image -> Image
;; renders a list of invader missiles onto a scene
(check-expect
 (render-invmissiles IMISSILESLIST1 BACKGROUND)
 (place-image MISSILE-ENEMY (coord-x (invmissile-coord IMISSILE1))
              (coord-y (invmissile-coord IMISSILE1))
              (place-image MISSILE-ENEMY (coord-x (invmissile-coord IMISSILE2))
                           (coord-y (invmissile-coord IMISSILE2))
                           BACKGROUND)))
(define (render-invmissiles loi rsf)
  (local [(define (render-invmissile mis scene)
            (place-image MISSILE-ENEMY
                         (coord-x (invmissile-coord mis))
                         (coord-y (invmissile-coord mis))
                         scene))]

    (cond [(empty? loi) rsf]
          [else
           (render-invmissiles (rest loi)
                               (render-invmissile (first loi) rsf))])))


;; Game KeyEvent -> Game
;; moves the player left or right when arrow keys are pressed, and fires a missile on space

;; pressing space should add a new missile at the player’s position
;; (slightly above the player sprite).
(check-expect
 (handle-key STARTING-STATE " ")
 (make-game
  (game-player STARTING-STATE)
  (game-loi STARTING-STATE)
  (cons (make-missile
         (make-coord (coord-x (player-coord (game-player STARTING-STATE)))
                     (- (coord-y (player-coord (game-player STARTING-STATE)))
                        (+ (/ (image-height PROTAG) 2)
                           (/ MISSILE-HEIGHT 2)))))
        (game-lom STARTING-STATE))
  (game-loim STARTING-STATE)))

;; pressing left moves the player’s x-coordinate left by PLAYER-SPEED
;; (invaders, missiles, and loim unchanged).
(check-expect
 (handle-key STARTING-STATE "left")
 (make-game
  (make-player
   (make-coord (- (coord-x (player-coord (game-player STARTING-STATE)))
                  PLAYER-SPEED)
               (coord-y (player-coord (game-player STARTING-STATE)))))
  (game-loi STARTING-STATE)
  (game-lom STARTING-STATE)
  (game-loim STARTING-STATE)))

;; pressing right moves the player’s x-coordinate right by PLAYER-SPEED
;; (invaders, missiles, and loim unchanged).
(check-expect
 (handle-key STARTING-STATE "right")
 (make-game
  (make-player
   (make-coord (+ (coord-x (player-coord (game-player STARTING-STATE)))
                  PLAYER-SPEED)
               (coord-y (player-coord (game-player STARTING-STATE)))))
  (game-loi STARTING-STATE)
  (game-lom STARTING-STATE)
  (game-loim STARTING-STATE)))

(define (handle-key game ke)
  (if (string? game)
      game
      (cond [(key=? ke " ")
             (make-game (game-player game)
                        (game-loi game)
                        (cons (make-missile
                               (make-coord (coord-x (player-coord (game-player game)))
                                           (- (coord-y (player-coord (game-player game)))
                                              (+ (/ (image-height PROTAG) 2) (/ MISSILE-HEIGHT 2)))))
                              (game-lom game))
                        (game-loim game))]
            [(key=? ke "left") (make-game (make-player (make-coord (- (coord-x (player-coord (game-player game)))
                                                                      PLAYER-SPEED)
                                                                   (coord-y (player-coord (game-player game)))))
                                          (game-loi game)
                                          (game-lom game)
                                          (game-loim game))]
            [(key=? ke "right") (make-game (make-player (make-coord (+ (coord-x (player-coord (game-player game)))
                                                                       PLAYER-SPEED)
                                                                    (coord-y (player-coord (game-player game)))))
                                           (game-loi game)
                                           (game-lom game)
                                           (game-loim game))]
            [else 
             game])))

(main STARTING-STATE)