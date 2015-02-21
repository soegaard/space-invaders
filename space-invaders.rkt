#lang racket
(require racket/gui)

;;;
;;; SPACE INVADERS
;;;

; Jens Axel Søgaard, Feb 2014
;     https://github.com/soegaard/space-invaders

; A Racket remake of Mary Rose Cook's JavaScript version of Space Invaders.
;     https://github.com/maryrosecook/annotated-code

;;; Data Representation
(struct world        (player invaders bullets) #:transparent)
(struct body         (x y size)                #:transparent)
(struct invader body (patrol-x speed-x)        #:transparent)
(struct bullet  body (velocity-x velocity-y)   #:transparent)
(struct player  body (dead?)                   #:transparent)

; A world represents the state of the game.
; A world consists of a player, a list of invaders and a list of bullets.
; The invaders, the bullets and the player are all bodies.
; All bodies are drawn as little squares.
; A body contains the x and y coordinate og its upper left corner as well as its size.
; Invaders move back and forth horisontally - this is called patrolling.
; The field patrol-x determines how far the invader has moved from its original position.
; A positive speed-x means rightward movement, and a negative sign leftward movement.

;;; Configuration
(define width       400)
(define height      400)
(define player-size  15)
(define bullet-size   3)
(define invader-size 15)

;;; Smart Constructors

(define (new-bullet x y vx vy)
  (bullet x y bullet-size vx vy))

(define (new-invader x y)
  (define patrol-x  0)
  (define speed-x   0.3)
  (invader x y invader-size patrol-x speed-x))

(define (new-player x y)
  (define size 15)
  (define dead? #f)
  (player x y size dead?))

;;;
;;; MODEL
;;;

;;; Creation

; create-world : -> world
;  the initial world contains the player and a bunch of invaders
(define (create-world)
  (world (create-player) (create-invaders) '()))

; create-invaders : -> (list body)
;   create list of twenty-four invaders
(define (create-invaders)
  (for/list ([i 24])
    (define x (+ 30 (* 30 (remainder i 8)))) ; 8 columns
    (define y (+ 30 (* 30 (remainder i 3)))) ; 3 rows
    (new-invader x y)))

; create-player : -> player
(define (create-player)
  (define x (/ width 2.))
  (define y (- height (* 2. player-size)))
  (new-player x y))

;;; Updaters

; update-invader : invader -> invader
(define (update-invader i)
  (match-define (invader x y size patrol-x speed-x) i)
  ; If the invader is outside the patrol bound we flip the speed
  (define speed-x-factor (if (<= 0 patrol-x 29) 1 -1))
  (define new-speed-x    (* speed-x-factor speed-x))
  (define new-x          (+        x new-speed-x))
  (define new-patrol-x   (+ patrol-x new-speed-x))
  (invader new-x y size new-patrol-x new-speed-x))

; update-bullet : bullet -> bullet
(define (update-bullet b)
  (match-define (bullet x y size vx vy) b)
  (bullet (+ x vx) (+ y vy) size vx vy))

; update-player : world -> world
(define (update-player w)
  (match-define (world p is bs) w)
  (match-define (player x y size d) p)
  (define dead? (or d (collisions? p bs)))  
  (define moved-player
    (cond [d                   p]
          [(key-down? 'left)  (player (- x 2.) y size dead?)]
          [(key-down? 'right) (player (+ x 2.) y size dead?)]
          [else               (player x y size dead?)]))
  (world moved-player is bs))

; spawn-invader-bullets : world -> world
;   maybe create new bullets below an invader
(define (spawn-invader-bullets w)
  (match-define (world p is bs) w)
  (define (maybe-spawn-bullet i)
    (match-define (invader x y size _ _) i)
    ; once in a while, if no friends below, create a bullet
    (if (and (> (random) 0.995)
             (not (invader-below? i is)))
        (bullet x (+ y size 1) 3 (- (random) 0.5) 2.)
        #f))
  (define new-bullets (filter-map maybe-spawn-bullet is))
  (world p is (append new-bullets bs)))

(define (invader-below? i is)
  (match-define (body x y size) i)
  (for/or ([b is])
    (and (<= x (body-x b) (+ x size))
         (> (body-y b) (+ y size)))))

; spawn-player-bullet : world -> world
;   a non-dead player shoots when space is pressed
(define (spawn-player-bullet w)
  (match-define (world p is bs) w)
  (cond
    [(player-dead? p) w] ; no shooting, when dead
    [(key-down? #\space) ; space => new bullet
     (match-define (player x y size d) p)
     (define b (new-bullet (+ x (/ size 2.)) y 0 -7))
     (world p is (cons b bs))]
    [else w]))

;;; UPDATES

(define (update w)
  (restart-on-r
   (remove-colliding-bodies
    (spawn-player-bullet
     (spawn-invader-bullets
      (update-player
       (update-invaders
        (update-bullets w))))))))


(define (update-invaders w)
  (define is (world-invaders w))
  (struct-copy world w [invaders (map update-invader is)]))

(define (update-bullets w)
  (struct-copy world w [bullets (map update-bullet (world-bullets w))]))

(define (restart-on-r w)
  (if (key-down? #\r)
      (create-world)
      w))

;;; Collision

(define (colliding? b1 b2)
  (match-define (body x1 y1 s1) b1)
  (match-define (body x2 y2 s2) b2)
  (not (or (eq? b1 b2)
           (< (+ x1 s1) x2) (> x1 (+ x2 s2))
           (< (+ y1 s1) y2) (> y1 (+ y2 s2)))))

(define (collisions? x bs)
  (for/or ([b bs]) (colliding? x b)))

(define (inside-screen? b)
  (match-define (body x y size) b)
  (and (< -40 x (+ width  40))
       (< -40 y (+ height 40))))

(define (remove-colliding-bodies w)
  (match-define (world p is bs) w)
  (define (no-bullet-collisons? x)
    (not (collisions? x bs)))
  (world p 
         (filter no-bullet-collisons? is)
         (filter inside-screen? ; remove non-visible bullets
                 (filter no-bullet-collisons? bs))))

;;; DRAWING

; draw-bodies : (list body) drawing-context -> void
;   draw the bodies in the world w to the drawing context dc
(define (draw-bodies bs dc)
  (for ([b bs])
    (match-define (body x y s) b)
    (define c (if (player? b) (if (player-dead? b) "red" "green") "black"))
    (send dc set-brush (new brush% [color c] [style 'solid]))
    (send dc draw-rectangle x y s s)))

(define (draw-world w dc)
  (match-define (world p is bs) w)
  (draw-bodies (append (list p) is bs) dc))

;;; GUI STATE

(define the-world (create-world))

;;; Keyboard
; The keyboard state is kept in a hash table. 
; Use key-down? to find out, whether a key is pressed or not.
(define the-keyboard (make-hasheq))
(define (key-down! k) (hash-set! the-keyboard k #t))
(define (key-up! k)   (hash-set! the-keyboard k #f))
(define (key-down? k) (hash-ref  the-keyboard k #f))

;;; Canvas
; Key events sent to the canvas updates the information in the-keyboard.
; Paint events calls draw-world. To prevent flicker we suspend flushing
; while drawing commences.
(define game-canvas%
  (class canvas%
    (define/override (on-event e) ; mouse events
      'ignore)
    (define/override (on-char e)  ; key event
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (when (eq? release 'press)  ; key down?
        (key-down! key))
      (when (eq? key 'release)    ; key up?
        (key-up! release)
        (when (eq? release #\space)
          (play-sound "shoot.mp3" #t))))
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (send dc clear)
      (draw-world the-world dc)
      (send this resume-flush))
    (super-new)))

; Create frame with canvas and show it.
(define frame  (new frame%  [label "Space Invaders"]))
(define canvas (new game-canvas% [parent frame] [min-width width] [min-height height]))
(send frame show #t)

; Start a timer. Each time the timer triggers, the world is updated.
(define timer (new timer% 
                   [notify-callback 
                    (λ () 
                      (set! the-world (update the-world))
                      (send canvas on-paint))]
                   [interval 100])) ; in milliseconds
(send timer start 20)


;;;
;;; INSTRUCTIONS
;;;

(displayln "SPACE INVADERS")
(displayln "Move:  left and right arrow")
(displayln "Shoot: space")
(displayln "Reset: r")
