#lang racket
(require "constants.rkt")
(provide (all-defined-out))
#|
Purpose of this file: loads all-time high score from the high_score file
Coded by: Gustaf Colliander
Last edit: 2019-02-03
Global variables: *high_score_input*, *current_high_score*
|#
(define *high_score_input* (open-input-file "high_score"))

(define *current_high_score* (read-line *high_score_input*))

(close-input-port *high_score_input*)

(hash-set! player_constants_hash "high_score" (string->number *current_high_score*))