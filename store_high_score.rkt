#lang racket
(require "constants.rkt")
(provide (all-defined-out))
#|
Purpose of this file: stores score as the all-time high score to the high_score file
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#

(define (store-if-new-high-score)
  (let ((all_time_high_score (hash-ref player_constants_hash "high_score"))
        (score (hash-ref player_constants_hash "score")))
    (when (> score all_time_high_score) 
      (define high_score_output (open-output-file "high_score" #:exists 'can-update))
      (write-string (number->string score) high_score_output)
      (close-output-port high_score_output))))