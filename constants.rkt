#lang racket/gui
(provide (all-defined-out))
#|
Purpose of this file: This file stores all the constants used in all other files in hash tables.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
Global variables: All the (make-hash) definitions.
|#
;;I was forced to store the constants in hash-tables since you are otherwise not allowed
;;to mutate constants when the request comes from another module.
;;With hash-tables you can use the hash-set! command to mutate the constants.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Constants

;Counter
;Used for bullet delay
(define counter_constants_hash (make-hash))
(hash-set! counter_constants_hash "counter" 0)

;difficulty
;;Factor by which alien properties [speed, shooting frequency] are increasing after every round.
;Recommended factor: 1-1.5
(define difficulty_constants_hash (make-hash))
(hash-set! difficulty_constants_hash "speed_x_factor" 1.5) 
(hash-set! difficulty_constants_hash "speed_y_factor" 1.5)
(hash-set! difficulty_constants_hash "shoot_prob_factor" 1.5)


;alien
(define alien_constants_hash (make-hash))
(hash-set! alien_constants_hash "speed_x" 0.4) 
(hash-set! alien_constants_hash "speed_y" 0.1)
(hash-set! alien_constants_hash "radius" 15)
(hash-set! alien_constants_hash "width" 30)
(hash-set! alien_constants_hash "color" "red")
(hash-set! alien_constants_hash "shoot_probability" 0.3) ;This is the probability [in %] the alien will shoot during a canvas refresh.
(hash-set! alien_constants_hash "wall_margin" 250)
(hash-set! alien_constants_hash "alien_margin" 30)
(hash-set! alien_constants_hash "rows" 5) ;5
(hash-set! alien_constants_hash "columns" 11) ;11

;alien bullets
(define alien_bullet_constants_hash (make-hash))
(hash-set! alien_bullet_constants_hash "radius" 2)
(hash-set! alien_bullet_constants_hash "width" 4)
(hash-set! alien_bullet_constants_hash "speed_y" 1)
(hash-set! alien_bullet_constants_hash "speed_x" 0)
(hash-set! alien_bullet_constants_hash "color" "black")

;player
(define player_constants_hash (make-hash))
(hash-set! player_constants_hash "radius" 10)
(hash-set! player_constants_hash "width" 20)
(hash-set! player_constants_hash "color" "blue")
(hash-set! player_constants_hash "lives" 1)
(hash-set! player_constants_hash "score" 0)
(hash-set! player_constants_hash "high_score" 0)

;player bullets
(define player_bullet_constants_hash (make-hash))
(hash-set! player_bullet_constants_hash "delay" 0.2) ;delay in seconds between every bullet fired
(hash-set! player_bullet_constants_hash "radius" 2)
(hash-set! player_bullet_constants_hash "width" 4)
(hash-set! player_bullet_constants_hash "speed_y" -4)
(hash-set! player_bullet_constants_hash "speed_x" 0)
(hash-set! player_bullet_constants_hash "color" "green")

;frame
(define frame_constants_hash (make-hash))
(hash-set! frame_constants_hash "label" "Space Invaders")
(hash-set! frame_constants_hash "width" 900)
(hash-set! frame_constants_hash "height" 600)
(hash-set! frame_constants_hash "show" #t)
(hash-set! frame_constants_hash "min_width" 900)
(hash-set! frame_constants_hash "min_height" 600)
(hash-set! frame_constants_hash "stretchable_width" #f)
(hash-set! frame_constants_hash "stretchable_height" #f)

;canvas
(define canvas_constants_hash (make-hash))
(hash-set! canvas_constants_hash "background_color" "white")
(hash-set! canvas_constants_hash "width" 900)
(hash-set! canvas_constants_hash "height" 600)
(hash-set! canvas_constants_hash "style" (list 'border))
(hash-set! canvas_constants_hash "vert_margin" 0)
(hash-set! canvas_constants_hash "horiz_margin" 0)
(hash-set! canvas_constants_hash "min_width" 900)
(hash-set! canvas_constants_hash "min_height" 600)
(hash-set! canvas_constants_hash "stretchable_width" #f)
(hash-set! canvas_constants_hash "stretchable_height" #f)
(hash-set! canvas_constants_hash "label" "canvas")

;timer
(define timer_constants_hash (make-hash))
(hash-set! timer_constants_hash "interval_msec" 10)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;update constant procedures

(define (set-alien_speed! speedX speedY)
  (hash-set! alien_constants_hash "speed_x" speedX)
  (hash-set! alien_constants_hash "speed_y" speedY))

