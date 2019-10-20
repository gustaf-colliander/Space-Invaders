#lang racket
(require "world_init.rkt")
(require "constants.rkt")
(provide (all-defined-out))
#|
Purpose of this file: procedures for manipulating and extracting info from objects.
Coded by: Gustaf Colliander
Last edit: 2019-02-03
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Object procedures

;;NOTE: We presume that pos=(pos_x,pos_y) is the CENTER of an object (rectangle) on the canvas.

;;tests if the center of an object is placed at pos = (pos_x,pos_y) from the list given by hash_key
;;pos -> boolean
(define (object-centered-at-pos? hash_key pos_x pos_y)
  (let ((boolean #f)
        (list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (obj_hash)
       (when
           (object-centered-at-pos?-helper obj_hash pos_x pos_y)
         (set! boolean #t)))
     list_of_hash)
    boolean))

(define (object-centered-at-pos?-helper obj_hash pos_x pos_y)
  (and
   (equal? (hash-ref obj_hash "pos_x")
           pos_x)
   (equal? (hash-ref obj_hash "pos_y")
           pos_y)))

;;Tests if the pos = (pos_x,pos_y) is inside an object from the list given by hash_key
;;pos -> boolean
(define (object-near-pos? hash_key pos_x pos_y)
  (let* ((boolean #f)
         (list_of_hash (hash-ref *hash_of_lists* hash_key))
         (obj_const_hash (hash-ref *hash_of_constants* hash_key))
         (radius  (hash-ref obj_const_hash "radius")))
    (for-each
     (lambda (obj_hash)
       (when
           (object-near-pos?-helper obj_hash pos_x pos_y radius)
         (set! boolean #t)))
     list_of_hash)
    boolean))

;Checks if obj near pos by calculating: | current_pos - pos| <= object radius 
(define (object-near-pos?-helper obj_hash pos_x pos_y radius)
  (and
   (<= (abs (- (hash-ref obj_hash "pos_x")
               pos_x))
       radius)
   (<= (abs (- (hash-ref obj_hash "pos_y")
               pos_y))
       radius)))
  

;;Removes the object at pos = (pos_x,pos_y) from the list given by hash_key
;;If successful, returns #t otherwise #f
;;pos -> boolean
(define (remove-object! hash_key pos_x pos_y)
  (let ((boolean #f)
        (list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (obj_hash)
       (when
           (object-centered-at-pos?-helper obj_hash pos_x pos_y)
         (hash-set! *hash_of_lists* hash_key (remove obj_hash list_of_hash))
         (set! boolean #t)))
     list_of_hash)
    boolean))

(define (remove-object-near-pos! hash_key pos_x pos_y)
  (when (not (equal? #f (get-object-near-pos hash_key pos_x pos_y)))
    (let* ((object_ (get-object-near-pos hash_key pos_x pos_y))
           (posx (hash-ref object_ "pos_x"))
           (posy (hash-ref object_ "pos_y")))
      (remove-object! hash_key posx posy))))

;Adds an object to a list given by hash_key
;;pos -> void
;;const_list is a (list (cons "pos_x" 50) (cons "speed_x" 0) ..) type of list
(define (add-object! hash_key const_list)
  (let ((obj_hash (make-hash))
        (list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (const_pair)
       (hash-set! obj_hash (car const_pair) (cdr const_pair)))
     const_list)
    (hash-set! *hash_of_lists* hash_key (cons obj_hash list_of_hash))))

;Changes the speed of an object from the list given by hash_key
(define (set-object-speed! hash_key speed_x speed_y)
  (let ((list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (obj_hash)
       (hash-set! obj_hash "speed_x" speed_x)
       (hash-set! obj_hash "speed_y" speed_y)
       obj_hash)
     list_of_hash)))

;;Updates the position of all the objects in the list given by hash_key
(define (update-all-object-pos! hash_key)
  (let ((list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (obj_hash)
       (let ((obj_pos_x (hash-ref obj_hash "pos_x"))
             (obj_pos_y (hash-ref obj_hash "pos_y"))
             (obj_speed_x (hash-ref obj_hash "speed_x"))
             (obj_speed_y (hash-ref obj_hash "speed_y")))
         (hash-set! obj_hash "pos_x" (+ obj_pos_x
                                        obj_speed_x))
         (hash-set! obj_hash "pos_y" (+ obj_pos_y
                                        obj_speed_y))
         obj_hash))
     list_of_hash)))

;;If object-near-pos? = #t return the object
;;pos,hash_key -> object
(define (get-object-near-pos hash_key pos_x pos_y)
  (let* ((object_near_pos #f)
         (list_of_hash (hash-ref *hash_of_lists* hash_key))
         (obj_const_hash (hash-ref *hash_of_constants* hash_key))
         (radius (hash-ref obj_const_hash "radius")))
    (for-each
     (lambda (obj_hash)
       (when
           (object-near-pos?-helper obj_hash pos_x pos_y radius)
         (set! object_near_pos obj_hash)))
     list_of_hash)
    object_near_pos))


;;Returns from the list given by hash_key the object at pos = (pos_x,pos_y)
;;If successful: pos -> object
;;Otherwise: pos -> boolean
(define (get-object hash_key pos_x pos_y)
  (let ((wanted_object #f)
        (list_of_hash (hash-ref *hash_of_lists* hash_key)))
    (for-each
     (lambda (obj_hash)
       (when
           (object-centered-at-pos?-helper obj_hash pos_x pos_y)
         (set! wanted_object obj_hash)))
     list_of_hash)
    wanted_object))
;;Returns for example: a list of aliens if hash_key="herd"
;;If successful: string -> list
;;Otherwise: string -> error: [hash-ref: no value found for key]
(define (get-list-of-objects hash_key)
  (hash-ref *hash_of_lists* hash_key))

(define (kill-all-objects hash_key)
  (hash-set! *hash_of_lists* hash_key null))
