#lang racket/gui
(require "constants.rkt")
(require "logic.rkt")
(require "store_high_score.rkt")
(require "world_init.rkt")
(require "alien.rkt")
(require "player.rkt")
(provide (all-defined-out))
#|
Purpose of this file: Uses data from all other rkt files to present the game graphically.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
Global variables: *frame*, *canvas*, *graphics-timer*
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Drawing procedures

;;Note: We are drawing the rectangle so the center is placed at pos=(pos_x,pos_y).
(define (draw-objects-on-canvas canvas dc obj_constants_hash list_of_obj_hash)
  (send dc set-pen (hash-ref obj_constants_hash "color") 0 'solid)
  (send dc set-brush (hash-ref obj_constants_hash "color") 'solid)
  (for-each
   (lambda (obj_hash_)
     (let ((radius (hash-ref obj_constants_hash "radius")))
       (send dc translate (- radius) (- radius))
       (send dc draw-rectangle (hash-ref obj_hash_ "pos_x") (hash-ref obj_hash_ "pos_y") (hash-ref obj_constants_hash "width") (hash-ref obj_constants_hash "width"))
       (send dc translate radius radius)))
   list_of_obj_hash))

(define (draw-herd-on-canvas canvas dc)
  (draw-objects-on-canvas canvas dc alien_constants_hash (hash-ref *hash_of_lists* "herd")))

(define (draw-alien-cluster-on-canvas canvas dc)
  (draw-objects-on-canvas canvas dc alien_bullet_constants_hash (hash-ref *hash_of_lists* "alien_cluster")))

(define (draw-player-cluster-on-canvas canvas dc)
  (draw-objects-on-canvas canvas dc player_bullet_constants_hash (hash-ref *hash_of_lists* "player_cluster")))

(define (draw-player-on-canvas canvas dc)
  (let ((player (hash-ref *hash_of_lists* "player")))
    (draw-objects-on-canvas canvas dc player_constants_hash player)))

(define (draw-all-objects! canvas dc)
  (draw-player-on-canvas canvas dc)
  (draw-herd-on-canvas canvas dc)
  (draw-player-cluster-on-canvas canvas dc)
  (draw-alien-cluster-on-canvas canvas dc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Frame

; Make a frame by instantiating the frame% class
(define *frame*
  (let ((lives (hash-ref player_constants_hash "lives"))
        (score (hash-ref player_constants_hash "score"))
        (high_score (hash-ref player_constants_hash "high_score")))
    (new frame% [label (string-join (list "Lives left:" (number->string lives) " " "Score:" (number->string score) "  "  "High Score:" (number->string high_score)))]
         [width (hash-ref frame_constants_hash "width")]
         [height (hash-ref frame_constants_hash "height")]
         [min-width (hash-ref frame_constants_hash "min_width")]
         [min-height (hash-ref frame_constants_hash "min_height")]	 
         [stretchable-width (hash-ref frame_constants_hash "stretchable_width")]	 
         [stretchable-height (hash-ref frame_constants_hash "stretchable_height") ]
         )))
; Show the frame by calling its show method
(send *frame* show (hash-ref frame_constants_hash "show"))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Canvas
  
;Draws on the canvas
(define (drawing-proc canvas dc)
  (cond  [(or (any-alien-at-canon?) (no-player-lives-left?))
          (stop-timer)
          (send dc clear)
          (send dc draw-text "Game Over" 400 300)
          (store-if-new-high-score)]
         [else
          (when (no-aliens-left?)
            (make-aliens-harder)
            (init-aliens-to-herd))
          (when (any-alien-at-wall?)
            (switch-alien-direction!))
          (if-alien-hit-add-score-and-remove-alien-and-bullet *frame*)
          (if-canon-hit-remove-bullet-and-one-life *frame*)
          (update-all-objects!)
          (draw-all-objects! canvas dc)]))

;Define input-canvas
(define input-canvas%
  (class canvas%
    [init-field mouse-handler]

    (define/override (on-event mouse-event)
      (mouse-handler mouse-event))
    (super-new)))

;From mouse input: Move player and shoot player bullets
(define (our-mouse-handler event)
  (let* ((player_radius (hash-ref player_constants_hash "radius"))
         (player (car (hash-ref *hash_of_lists* "player")))
         (canvas_width (hash-ref canvas_constants_hash "width"))
         (pos_x (send event get-x))
         (pos_y (hash-ref player "pos_y"))
         (min_x player_radius)
         (max_x (- canvas_width player_radius))
         (new_bullet_speed_y (hash-ref player_bullet_constants_hash "speed_y"))
         (new_bullet_speed_x (hash-ref player_bullet_constants_hash "speed_x")))
    (when (< pos_x min_x)
      (set! pos_x min_x))
    (when (> pos_x max_x)
      (set! pos_x max_x))
    (update-player-pos! event pos_x)
    (when (and (left-button-down? event) (ready-to-shoot?))
      (reset-counter!)
      (add-player-bullet! (list (cons
                                 "pos_x" pos_x) (cons "pos_y" (- pos_y player_radius)) (cons "speed_x" new_bullet_speed_x) (cons "speed_y" new_bullet_speed_y))))))

;Initialize canvas.
(define *canvas* (new input-canvas%
                    [parent *frame*]
                    [mouse-handler our-mouse-handler]
                    [style (hash-ref canvas_constants_hash "style")]
                    [vert-margin (hash-ref canvas_constants_hash "vert_margin")]	 
                    [horiz-margin (hash-ref canvas_constants_hash "horiz_margin")]
                    [min-width (hash-ref canvas_constants_hash "min_width")]
                    [min-height (hash-ref canvas_constants_hash "min_height")]	 
                    [stretchable-width (hash-ref canvas_constants_hash "stretchable_width")]	 
                    [stretchable-height (hash-ref canvas_constants_hash "stretchable_height") ]	
                    [label (hash-ref canvas_constants_hash "label")]
                    [paint-callback
                     drawing-proc]))

(send *canvas* set-canvas-background (make-object color% (hash-ref canvas_constants_hash "background_color")))

;refresh canvas and update counter
(define (refresh-canvas)
  (update-counter!)
  (send *canvas* refresh))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Timer
(define *graphics-timer* (new timer%
                             [notify-callback refresh-canvas]))

(define (start-timer)
  (send *graphics-timer* start (hash-ref timer_constants_hash "interval_msec")))

(define (stop-timer)
  (send *graphics-timer* stop))