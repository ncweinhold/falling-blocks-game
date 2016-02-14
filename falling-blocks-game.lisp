;;;; falling-blocks-game.lisp

(in-package #:falling-blocks-game)

(defconstant +screen-width+ 600)
(defconstant +screen-height+ 820)
(defconstant +well-height+ 25)
(defconstant +well-width+ 12)
(defconstant +block-size+ 32)
(defparameter *window* nil)
(defparameter *score* 0)
(defparameter *background-color* (sdl:color :r 0 :g 0 :b 150 :a 255))
(defparameter *ui-font* (sdl:initialise-font sdl:*font-10x20*))

(defclass piece () ())

(defclass piece-O (piece)
  ((blocks :accessor blocks
	   :initarg :blocks
	   :initform (make-array '(4) :initial-contents
				 '(#x0660 #x0660 #x0660 #x0660)))
   (current-rotation-idx :accessor current-rotation-idx
			 :initarg :current-rotation-idx
			 :initform 0)
   (current-rotation-data :accessor current-rotation-data
			  :initarg :current-rotation-data
			  :initform #x0660)))

(defclass piece-L (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x4460 #x0740 #x0622 #x0)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x4460)))

(defclass piece-J (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x2260 #x0470 #x0644 #x0E20)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x2260)))

(defclass piece-T (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x0720 #x0262 #x04E0 #x4640)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x0720)))

(defclass piece-I (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x2222 #x0F00 #x4444 #x00F0)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x2222)))

(defclass piece-Z (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x0630 #x0264 #x0C60 #x2640)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x0630)))

(defclass piece-S (piece)
  ((blocks :accessor blocks
           :initarg :blocks
           :initform (make-array '(4) :initial-contents
                                 '(#x06C0 #x4620 #x0360 #x0462)))
   (current-rotation-idx :accessor current-rotation-idx
                         :initarg :current-rotation-idx
                         :initform 0)
   (current-rotation-data :accessor current-rotation-data
                          :initarg :current-rotation-data
                          :initform #x06C0)))

(defmethod rotate-clockwise ((p piece))
  (let ((current-idx (current-rotation-idx p)))
    (if (= current-idx 3)
        (setf current-idx 0)
        (setf current-idx (+ current-idx 1)))
    (setf (current-rotation-idx p) current-idx)
    (setf (current-rotation-data p) (aref (blocks p) current-idx))))

(defmethod rotate-anticlockwise ((p piece))
  (let ((current-idx (current-rotation-idx p)))
    (if (= current-idx 0)
        (setf current-idx 3)
        (setf current-idx (- current-idx 1)))
    (setf (current-rotation-idx p) current-idx)
    (setf (current-rotation-data p) (aref (blocks p) current-idx))))

(defun render-blocks (line)
  "Each line is made up of 4 squares"
  (loop for i from 3 downto 0 do
    (if (logtest line (expt 2 i))
        (format t "#")
        (format t "-")))
  (format t "~%"))

(defmethod render-piece ((p piece))
  "Each piece is 4x4 in size - so render each line one by one"
  (loop for i from 0 to 3 do
    (render-blocks (ldb (byte 4 (* i 4)) (current-rotation-data p)))))

(defmethod slow-drop-piece ((p piece))
  (write-line "Slow drop"))

(defmethod fast-drop-piece ((p piece))
  (write-line "fast drop"))

(defun generate-new-piece ()
  (let ((index (random 7)))
    (case index
      (0 (make-instance 'piece-O))
      (1 (make-instance 'piece-L))
      (2 (make-instance 'piece-J))
      (3 (make-instance 'piece-T))
      (4 (make-instance 'piece-I))
      (5 (make-instance 'piece-Z))
      (6 (make-instance 'piece-S)))))

(defmethod bottom-edge ((p piece))
  (block outer
    (loop for i from 3 downto 0 do
      (let ((line (ldb (byte 4 (* i 4)) (current-rotation-data p))))
        (when (logtest line #xE) (return-from outer i))))))

(defun update-game ()
  (write-line "Updating"))

(defun render-game ()
  (render-gui))

(defun render-gui ()
  (sdl:clear-display *background-color*)
  (sdl:draw-box-* 10 10 390 800 :color sdl:*black*)
  (sdl:draw-string-solid-* (format nil "Score: ~d" *score*)
			   420 10
			   :font *ui-font*)
  (render-next-piece))

(defun render-next-piece ()
  (sdl:draw-string-solid-* "Next"
			   420 60
			   :font *ui-font*)
  (sdl:draw-box-* 420 110 150 150 :color sdl:*black*))

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
			  (sdl:push-quit-event)))))))
     ; (sdl:update-display))))
