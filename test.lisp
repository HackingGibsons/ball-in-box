(in-package :cl-user)
(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-mixer)
(ql:quickload :cl-opengl)

(defun test-mixer ()
    (ql:quickload :lispbuilder-sdl-mixer-examples)
    (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
      (SDL-MIXER-EXAMPLES:MIXER)))

(defun test-sdl-opengl-drawing ()
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:SDL-INIT-AUDIO sdl:SDL-INIT-VIDEO)
      (SDL:SET-AUDIO-DRIVER "coreaudio")
      (sdl:window 1024 768
                  :title-caption "OpenGL Example"
                  :icon-caption "OpenGL Example"
                  :opengl t
                  :resizable t
                  :double-buffer t
                  :hw t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
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

               (sdl:update-display)
               t)))))
