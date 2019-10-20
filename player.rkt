#lang racket
(require "world_init.rkt")
(require "abstraction.rkt")
(provide (all-defined-out))
#|
Purpose of this file: Contains player specific procedures made from the object procedures from "abstraction.rkt".
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#

;Player bullets
(define (player-bullet-centered-at-pos? pos_x pos_y)
  (object-centered-at-pos? "player_cluster" pos_x pos_y))

(define (player-bullet-near-pos? pos_x pos_y)
  (object-near-pos? "player_cluster" pos_x pos_y))

(define (remove-player-bullet! pos_x pos_y)
  (remove-object! "player_cluster" pos_x pos_y))

(define (remove-player-bullet-near-pos! pos_x pos_y)
  (remove-object-near-pos! "player_cluster" pos_x pos_y))

(define (add-player-bullet! const_list)
  (add-object! "player_cluster" const_list))

(define (set-player-bullet-speed! speed_x speed_y)
  (set-object-speed! "player_cluster" speed_x speed_y))

(define (update-all-player-bullet-pos!)
  (update-all-object-pos! "player_cluster"))

(define (get-player-bullet-near-pos pos_x pos_y)
  (get-object-near-pos "player_cluster" pos_x pos_y))

(define (get-player-bullet pos_x pos_y)
  (get-object "player_cluster" pos_x pos_y))

(define (get-list-of-player-bullets)
  (get-list-of-objects "player_cluster"))

(define (kill-all-player-bullets)
  (kill-all-objects "player_cluster"))

;Player
(define (update-player-pos! event pos_x)
  (let ((player (car (hash-ref *hash_of_lists* "player"))))
    (hash-set! player "pos_x" pos_x)
    (hash-set! *hash_of_lists* "player" (list player))))

(define (player-near-pos? pos_x pos_y)
  (object-near-pos? "player" pos_x pos_y))