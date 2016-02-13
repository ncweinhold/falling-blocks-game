;;;; falling-blocks-game.lisp

(in-package #:falling-blocks-game)

(defconstant +screen-width+ 650)
(defconstant +screen-height+ 750)
(defparameter *window* nil)
(defparameter *score* 0)
(defparameter *background-color* (sdl:color :r 0 :g 0 :b 150 :a 255))
(defparameter *ui-font* (sdl:initialise-font sdl:*font-10x20*))

(defun update-game ()
  (write-line "Updating"))

(defun render-game ()
  (render-gui))

(defun render-gui ()
  (sdl:clear-display *background-color*)
  (sdl:draw-box-* 10 10 420 730 :color sdl:*black*)
  (sdl:draw-string-solid-* (format nil "Score: ~d" *score*)
			   450 10
			   :font *ui-font*)
  (render-next-piece))

(defun render-next-piece ()
  (sdl:draw-string-solid-* "Next"
			   450 60
			   :font *ui-font*)
  (sdl:draw-box-* 450 110 150 150 :color sdl:*black*))

(defun main ()
  (sdl:with-init ()
    (setf *window* (sdl:window +screen-width+ +screen-height+
			       :title-caption "Falling Blocks Game"
			       :icon-caption "Falling Blocks Game"))
    (unless *window*
      (error "~&Unable to create an SDL window~&"))
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (render-gui))
      (:key-down-event (:key k)
		       (cond
			 ((sdl:key= k :sdl-key-escape)
			  (sdl:push-quit-event))))
      (sdl:update-display))))
