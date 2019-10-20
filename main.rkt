#lang racket/gui
(require "constants.rkt")
(require "world_init.rkt")
(require "load_high_score.rkt")
(require "store_high_score.rkt")
(require "abstraction.rkt")
(require "alien.rkt")
(require "player.rkt")
(require "logic.rkt")
(require "graphics.rkt")
#|
Purpose of this file: Gather all rkt files for the project and launch the game.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#
(define (run-game)
  (start-timer))

(run-game)