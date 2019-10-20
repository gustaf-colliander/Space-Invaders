#lang racket/gui

(require "constants.rkt")
(require "alien.rkt")
(require "player.rkt")
(require "world_init.rkt")
(provide (all-defined-out))
#|
Purpose of this file: Contains some useful procedures used in other files and most importantly the collision logic procedures.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Some useful procedures

(define (update-all-objects!)
  (update-all-alien-pos!)
  (update-all-alien-bullet-pos!)
  (update-all-player-bullet-pos!)
  (add-bullet-to-cluster-at-random!))

;After every loop; add "interval_msec" to counter
(define (update-counter!)
  (let ((counter (hash-ref counter_constants_hash "counter")))
    (hash-set! counter_constants_hash "counter" (+ counter (/ (hash-ref timer_constants_hash "interval_msec") 1000)))))

;Resets counter to 0
(define (reset-counter!)
  (hash-set! counter_constants_hash "counter" 0))

;returns #t if left mouse button is pressed down
(define (left-button-down? event)
  (send event button-down? 'left))

;returns #t if enough time has passed since last fired bullet
(define (ready-to-shoot?)
  (let ((bullet_delay (hash-ref player_bullet_constants_hash "delay"))
        (counter (hash-ref counter_constants_hash "counter")))
    (>= counter bullet_delay)))
;adds 10 to the player score and updates the frame label.
(define (add-score! frame)
  (let* ((score (hash-ref player_constants_hash "score"))
         (lives (hash-ref player_constants_hash "lives"))
         (high_score (hash-ref player_constants_hash "high_score"))
         (new_score (+ score 10)))
    (hash-set! player_constants_hash "score" new_score)
    (send frame set-label (string-join (list "Lives left:" (number->string lives) " " "Score:" (number->string new_score) "  "  "High Score:" (number->string high_score))))))
    
;removes one life from player and updates the frame label.
(define (remove-player-life! frame)
  (let* ((score (hash-ref player_constants_hash "score"))
         (lives (hash-ref player_constants_hash "lives"))
         (high_score (hash-ref player_constants_hash "high_score"))
         (new_lives (- lives 1)))
    (hash-set! player_constants_hash "lives" new_lives)
    (send frame set-label (string-join (list "Lives left:" (number->string new_lives) " " "Score:" (number->string score) "  "  "High Score:" (number->string high_score))))))


(define (no-player-lives-left?)
  (zero? (hash-ref player_constants_hash "lives")))

;If there are no aliens left on canvas return #t.
(define (no-aliens-left?)
  (empty? (get-list-of-aliens)))

;Increase difficulty by increasing moving speed and shooting frequency of aliens.
(define (make-aliens-harder)
  (let ((speed_x (* (hash-ref difficulty_constants_hash "speed_x_factor") (hash-ref alien_constants_hash "speed_x")))
        (speed_y (* (hash-ref difficulty_constants_hash "speed_y_factor") (hash-ref alien_constants_hash "speed_y")))
        (shoot_prob (* (hash-ref difficulty_constants_hash "shoot_prob_factor") (hash-ref alien_constants_hash "shoot_probability"))))
    (when (> shoot_prob 0.5)
      (set! shoot_prob 0.5))
    (hash-set! alien_constants_hash "speed_x" speed_x)
    (hash-set! alien_constants_hash "speed_y" speed_y)
    (hash-set! alien_constants_hash "shoot_probability" shoot_prob)
    (printf "speed_x set to ~a~n speed_y set to ~a~n shoot_probability set to ~a~n" speed_x speed_y shoot_prob)))

;;Returns #f if no bullets have been added due to empty herd or
;;due to shoot-alien-bullet? returns #f.
;;Otherwise adds bullet to cluster positioned at a random alien.
(define (add-bullet-to-cluster-at-random!)
  (cond
    [(shoot-alien-bullet?)
     (let* ((herd (hash-ref *hash_of_lists* "herd"))
            (random_number (random (length herd)))
            (random_alien (list-ref herd random_number))
            (random_alien_posX (hash-ref random_alien "pos_x"))
            (random_alien_posY (hash-ref random_alien "pos_y"))
            (new_bullet_posX (+ (hash-ref alien_constants_hash "radius")
                                random_alien_posX))
            (new_bullet_posY (+ (hash-ref alien_constants_hash "width")
                                random_alien_posY))
            (bullet_speed_x (hash-ref alien_bullet_constants_hash "speed_x"))
            (bullet_speed_y (hash-ref alien_bullet_constants_hash "speed_y")))
       (cond
         [(empty? herd)
          #f]
         [else
          (add-alien-bullet! (list (cons "pos_x" new_bullet_posX) (cons "pos_y" new_bullet_posY) (cons "speed_x" bullet_speed_x) (cons "speed_y" bullet_speed_y)))]))]
    [else #f]))

;;Probability function
;; -> boolean
(define (shoot-alien-bullet?)
  (let ((random_number (random 1 100000)))
    (cond
      [(and
        (>= random_number 1)
        (<= random_number (* 1000 (hash-ref alien_constants_hash "shoot_probability"))))
       #t]
      [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Collision

(define (any-alien-at-wall?)
  (let ((boolean #f))
    (for-each
     (lambda (alien_)
       (let ((alien_pos_x (hash-ref alien_ "pos_x"))
             (alien_radius (hash-ref alien_constants_hash "radius"))
             (canvas_width (hash-ref canvas_constants_hash "width")))
         (when
             (or
              (>=
               alien_pos_x
               (- canvas_width
                  alien_radius))
              (<=
               alien_pos_x
               alien_radius))
           (set! boolean #t))))
     (hash-ref *hash_of_lists* "herd"))
    boolean))

(define (any-alien-at-canon?)
  (let ((boolean #f))
    (for-each
     (lambda (alien_)
       (let ((alien_pos_y (hash-ref alien_ "pos_y"))
             (alien_radius (hash-ref alien_constants_hash "radius"))
             (player_width (hash-ref player_constants_hash "width"))
             (canvas_height (hash-ref canvas_constants_hash "height")))
         (when
             (>=
              (+ alien_pos_y alien_radius)
              (- canvas_height
                 player_width))
           (set! boolean #t))))
     (hash-ref *hash_of_lists* "herd"))
    boolean))

;If a canon bullet hits an alien then remove alien and bullet
(define  (if-alien-hit-add-score-and-remove-alien-and-bullet frame)
  (for-each
   (lambda (bullet_)
     (let ((bullet_pos_x (hash-ref bullet_ "pos_x"))
           (bullet_pos_y (hash-ref bullet_ "pos_y")))
       (when
           (alien-near-pos? bullet_pos_x bullet_pos_y)
         (add-score! frame)
         (remove-alien-near-pos! bullet_pos_x bullet_pos_y)
         (remove-player-bullet! bullet_pos_x bullet_pos_y))))
   (hash-ref *hash_of_lists* "player_cluster")))

;If an alien bullet hits the canon then remove the bullet and remove one life from player.
(define  (if-canon-hit-remove-bullet-and-one-life frame)
  (for-each
   (lambda (bullet_)
     (let ((bullet_pos_x (hash-ref bullet_ "pos_x"))
           (bullet_pos_y (hash-ref bullet_ "pos_y")))
       (when
           (player-near-pos? bullet_pos_x bullet_pos_y)
         (init-canon)
         (remove-player-life! frame)
         (remove-alien-bullet! bullet_pos_x bullet_pos_y))))
   (hash-ref *hash_of_lists* "alien_cluster")))