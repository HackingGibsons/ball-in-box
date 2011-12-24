(in-package :ball-in-box)

;; Entry
(defmethod ball-in-box (&key)
  (log-for (output) "BOOTING!")
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:SDL-INIT-AUDIO sdl:SDL-INIT-VIDEO)
      (sdl:window (getf *window* :width) (getf *window* :height)
                  :title-caption (getf *window* :title)
                  :icon-caption (getf *window* :title)
                  :opengl t
                  :double-buffer t
                  :hw t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
      (setf (sdl:frame-rate) *fps*)

      (gl:clear-color 0 0 0 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -1 1)

      (sdl:with-events (:poll)
        (:quit-event () t)
        (:idle ()
               (gl:clear :color-buffer-bit)
               (gl:color 1 1 1)
               (gl:with-primitive :polygon
                 (gl:vertex 0.25 0.25 0)
                 (gl:vertex 0.75 0.25 0)
                 (gl:vertex 0.75 0.75 0)
                 (gl:vertex 0.25 0.75 0))
               (gl:flush)

               (sdl:update-display))))))