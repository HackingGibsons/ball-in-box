(in-package :ball-in-box)

(defclass game-world ()
  ((screen-width :initarg :screen-width
                 :accessor screen-width)
   (screen-height :initarg :screen-height
                  :accessor screen-height)
   (screen :accessor screen)

   (gravity :initarg :gravity :initarg :g
            :accessor gravity :accessor g
            :initform 9.8)

   (objects :accessor objects
            :initform nil)))

(defmethod tick ((world game-world) dt)
  (mapc #L(tick !1 dt) (objects world)))

(defmethod add-object ((world game-world) (o object))
  (setf (world o) world)
  (pushnew o (objects world)))

(defmethod init ((world game-world) &key (width 1024) (height 768))
  (with-slots (screen screen-width screen-height) world
    (setf screen-width width
          screen-height height
          screen (sdl:window (getf *window* :width) (getf *window* :height)
                             :fps (make-instance 'sdl:fps-timestep :dt 10)
                             :title-caption (getf *window* :title)
                             :icon-caption (getf *window* :title)
                             :opengl t
                             :double-buffer t
                             :hw t
                             :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))))

  (prog1 world
    (gl:enable :depth-test :blend)
    (gl:clear-color 0 0 0 0)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 width height 0 -1 1)))

(defmethod terminate ((world game-world))
  (log-for (output) "Terminating world. ~A" world)
  t)

(defmethod draw :before ((world game-world))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate (/ (screen-width world) 2.0)
                (/ (screen-height world) 2.0)
                0))

(defmethod draw ((world game-world))
  (mapc #'draw (objects world)))

