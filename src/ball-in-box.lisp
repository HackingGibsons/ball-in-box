(in-package :ball-in-box)

;; Entry
(defmethod ball-in-box (&key)
  (log-for (output) "BOOTING!")
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
                 (log-for (output) "Timestep: ~A FPS: ~F" (sdl:ticks) (sdl:average-fps)))

               (gl:clear :color-buffer-bit)
               (gl:matrix-mode :modelview)
               (gl:load-identity)

               (gl:with-pushed-matrix
                 (gl:translate (+ (/ (getf *window* :width) 2.0) 300)
                               (+ (/ (getf *window* :height) 2.0) 300)
                               0)
                 (gl:with-primitive :polygon
                   (gl:color 10 0 0)
                   (gl:vertex 20 20)
                   (gl:vertex 20 -20)
                   (gl:vertex -20 -20)
                   (gl:vertex -20 20)))

               (gl:with-pushed-matrix
                 (gl:translate (/ (getf *window* :width) 2.0) (/ (getf *window* :height) 2.0) 0)
                 (gl:with-primitive :polygon
                   (gl:color 1 1 1)
                   (gl:vertex 50 50)
                   (gl:vertex 50 -50)
                   (gl:vertex -50 -50)
                   (gl:vertex -50 50)))


               (gl:flush)
               (sdl:update-display))))))
