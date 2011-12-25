(in-package :ball-in-box)

(defclass object ()
  ((cx :initform 0
       :initarg :cx
       :accessor cx)
   (cy :initform 0
       :initarg :cy
       :accessor cy)
   (cz :initform 0
       :initarg :cz
       :accessor cz)))

(defmethod draw ((o object))
  (log-for (output warn) "Trying to draw a singularity: ~A" o))

(defclass rectangle (object)
  ((color :initform '(255 0 0)
          :initarg :color
          :accessor color)
   (width :initform 100
          :initarg :width
          :accessor width)
   (height :initform 100
           :initarg :height
           :accessor height)))

(defmethod draw ((o rectangle))
  (let ((top (+ (cy o) (/ (height o) 2)))
        (bottom (- (cy o) (/ (height o) 2)))
        (left (- (cx o) (/ (width o) 2)))
        (right (+ (cx o) (/ (width o) 2))))

    (apply #'gl:color (color o))

    (gl:with-primitive :polygon
      (gl:vertex right top (cz o))
      (gl:vertex right bottom (cz o))
      (gl:vertex left bottom (cz o))
      (gl:vertex left top (cz o)))))

;; Entry
(defmethod ball-in-box :around (&key)
  "Setup the traps, int the right parts of SDL"
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:SDL-INIT-AUDIO sdl:SDL-INIT-VIDEO)
      (call-next-method))))

(defmethod ball-in-box (&key)
  "Main entry method. Set up the world, initialize it and enter the game loop."
  (log-for (output) "BOOTING!")
  (let ((world (make-instance 'game-world)))

    (log-for (output) "Initing world.")
    (init world :width (getf *window* :width)
                :height (getf *window* :height))

    ;; Add some stuff
    (push (make-instance 'rectangle :width 100 :height 100)
          (objects world))
    (push (make-instance 'rectangle :width 30 :height 30 :color '(0 255 0) :cx -100)
          (objects world))

    (log-for (output) "Entering event loop.")
    (sdl:with-events (:poll)
      (:quit-event () (terminate world))
      (:idle
       (sdl:with-timestep
         (tick world (sdl:dt)))

       ;; Draw the game world
       (draw world)

       ;; Finish the frame
       (sdl:update-display)))))
