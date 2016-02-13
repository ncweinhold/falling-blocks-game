;;;; falling-blocks-game.lisp

(in-package #:falling-blocks-game)

(defparameter *screen-width* 600)
(defparameter *screen-height* 750)
(defparameter *window* nil)

(defun main ()
  (write-line "Starting game")
  (sdl:with-init ()
    (setf *window* (sdl:window *screen-width* *screen-height*
			       :sw t
			       :position t
			       :title-caption "Falling Blocks Game"
			       :icon-caption "Falling Blocks Game"))
    (unless *window*
      (error "~&Unable to create an SDL window~&"))
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl:draw-filled-circle-* (random 400) (random 400) (random 40)
				       :color (sdl:color :r 255
							 :g 255
							 :b 255
							 :a 255)
				       :alpha 255))
      (:key-down-event (:key k)
		       (cond
			 ((sdl:key= k :sdl-key-escape)
			  (sdl:push-quit-event))))
      (sdl:update-display))))
