#lang racket
(require "constants.rkt")
(require "abstraction.rkt")
#|
Purpose of this file: Contains alien specific procedures made from the object procedures from "abstraction.rkt".
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#

(provide (all-defined-out))

(define (switch-alien-direction!)
  (hash-set! alien_constants_hash "speed_x" (- (hash-ref alien_constants_hash "speed_x")))
  (set-alien-speed! (hash-ref alien_constants_hash "speed_x") (hash-ref alien_constants_hash "speed_y")))

(define (alien-centered-at-pos? pos_x pos_y)
  (object-centered-at-pos? "herd" pos_x pos_y))

(define (alien-near-pos? pos_x pos_y)
  (object-near-pos? "herd" pos_x pos_y))

(define (remove-alien! pos_x pos_y)
  (remove-object! "herd" pos_x pos_y))

(define (remove-alien-near-pos! pos_x pos_y)
  (remove-object-near-pos! "herd" pos_x pos_y))

(define (add-alien! const_list)
  (add-object! "herd" const_list))

(define (set-alien-speed! speed_x speed_y)
  (set-object-speed! "herd" speed_x speed_y))

(define (update-all-alien-pos!)
  (update-all-object-pos! "herd"))

(define (get-alien-near-pos pos_x pos_y)
  (get-object-near-pos "herd" pos_x pos_y))

(define (get-alien pos_x pos_y)
  (get-object "herd" pos_x pos_y))

(define (get-list-of-aliens)
  (get-list-of-objects "herd"))

(define (kill-all-aliens)
  (kill-all-objects "herd"))

;Alien bullets
(define (alien-bullet-centered-at-pos? pos_x pos_y)
  (object-centered-at-pos? "alien_cluster" pos_x pos_y))

(define (alien-bullet-near-pos? pos_x pos_y)
  (object-near-pos? "alien_cluster" pos_x pos_y))

(define (remove-alien-bullet! pos_x pos_y)
  (remove-object! "alien_cluster" pos_x pos_y))

(define (remove-alien-bullet-near-pos! pos_x pos_y)
  (remove-object-near-pos! "alien_cluster" pos_x pos_y))

(define (add-alien-bullet! const_list)
  (add-object! "alien_cluster" const_list))

(define (set-alien-bullet-speed! speed_x speed_y)
  (set-object-speed! "alien_cluster" speed_x speed_y))

(define (update-all-alien-bullet-pos!)
  (update-all-object-pos! "alien_cluster"))

(define (get-alien-bullet-near-pos pos_x pos_y)
  (get-object-near-pos "alien_cluster" pos_x pos_y))

(define (get-alien-bullet pos_x pos_y)
  (get-object "alien_cluster" pos_x pos_y))

(define (get-list-of-alien-bullets)
  (get-list-of-objects "alien_cluster"))

(define (kill-all-alien-bullets)
  (kill-all-objects "alien_cluster"))