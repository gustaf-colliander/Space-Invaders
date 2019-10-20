#lang racket
(require "constants.rkt")
(provide (all-defined-out))
#|
Purpose of this file: Create the data structure for the world and initialize objects to the world.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
Global variables: *hash_of_lists*, *hash_of_constants*
|#
;Create hash-table
(define *hash_of_lists* (make-hash))
(define *hash_of_constants* (make-hash))

;initialize object constants to hash_of_constants
(hash-set! *hash_of_constants* "herd" alien_constants_hash)
(hash-set! *hash_of_constants* "alien_cluster" alien_bullet_constants_hash)
(hash-set! *hash_of_constants* "player_cluster" player_bullet_constants_hash)
(hash-set! *hash_of_constants* "player" player_constants_hash)


;initialize objects to hash_of_lists
(hash-set! *hash_of_lists* "herd" null)
(hash-set! *hash_of_lists* "alien_cluster" null)
(hash-set! *hash_of_lists* "player_cluster" null)
(hash-set! *hash_of_lists* "player" null)

;initialize aliens to the hash_of_lists herd
(define (init-aliens-to-herd)
  (for ([j (hash-ref alien_constants_hash "rows")])
    (for ([i (hash-ref alien_constants_hash "columns")])
      (let ((some_hash (make-hash))
            (width (hash-ref alien_constants_hash "width"))
            (radius (hash-ref alien_constants_hash "radius"))
            (speed_x (hash-ref alien_constants_hash "speed_x"))
            (speed_y (hash-ref alien_constants_hash "speed_y"))
            (alien_margin (hash-ref alien_constants_hash "alien_margin"))
            (herd (hash-ref *hash_of_lists* "herd")))
        (hash-set! some_hash "pos_x" (+ 130 (* i (+ width alien_margin))))
        (hash-set! some_hash "pos_y" (+ radius (* j (+ width alien_margin))))
        (hash-set! some_hash "speed_x" speed_x)
        (hash-set! some_hash "speed_y" speed_y)
        (hash-set! *hash_of_lists* "herd" (cons some_hash herd))))))
(init-aliens-to-herd)

;initialize canon to the hash_of_lists player
(define (init-canon)
  (let ((middle_of_canvas (/ (hash-ref canvas_constants_hash "width") 2))
        (bottom_of_canvas (- (hash-ref canvas_constants_hash "height") (hash-ref player_constants_hash "radius")))
        (canon (make-hash)))
    (hash-set! canon "pos_x" middle_of_canvas)
    (hash-set! canon "pos_y" bottom_of_canvas)
    (hash-set! *hash_of_lists* "player" (list canon))))
(init-canon)
