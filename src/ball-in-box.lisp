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

(defclass game-world ()
  ((objects :accessor objects
            :initform nil)))

(defmethod tick ((world game-world) dt)
  (log-for (trace) "Timestep: ~A FPS: ~F" (sdl:ticks) (sdl:average-fps)))

(defmethod draw ((world game-world))
  (mapc #'draw (objects world)))

;; Entry
(defmethod ball-in-box (&key)
  (log-for (output) "BOOTING!")
  (let ((world (make-instance 'game-world)))

    (push (make-instance 'rectangle :width 100 :height 100)
          (objects world))
    (push (make-instance 'rectangle :width 30 :height 30 :color '(0 255 0) :cx -100)
          (objects world))

    (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
      (sdl:with-init (sdl:SDL-INIT-AUDIO sdl:SDL-INIT-VIDEO)
        (sdl:window (getf *window* :width) (getf *window* :height)
                    :fps (make-instance 'sdl:fps-timestep :dt 10)
                    :title-caption (getf *window* :title)
                    :icon-caption (getf *window* :title)
                    :opengl t
                    :double-buffer t
                    :hw t
                    :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))

        (gl:clear-color 0 0 0 0)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:ortho 0 (getf *window* :width) (getf *window* :height) 0 -1 1)

        (sdl:with-events (:poll)
          (:quit-event () t)
          (:idle ()
                 (sdl:with-timestep
                   (tick world (sdl:dt)))

                 (gl:clear :color-buffer-bit)
                 (gl:matrix-mode :modelview)
                 (gl:load-identity)
                 (gl:translate (/ (getf *window* :width) 2.0)
                               (/ (getf *window* :height) 2.0)
                               0)


                 (draw world)

                 (sdl:update-display)))))))
