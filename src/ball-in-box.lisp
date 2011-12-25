(in-package :ball-in-box)

;; This is just to try out sdl surface rendering.
(defun surface-rendering-test ()
  (log-for (output) "BOOTING!")
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid :inexact :underflow :overflow)
    (sdl:with-init (sdl:SDL-INIT-AUDIO sdl:SDL-INIT-VIDEO)
      (sdl:window (getf *window* :width) (getf *window* :height)
                  :fps (make-instance 'sdl:fps-timestep :dt 10)
                  :title-caption (getf *window* :title)
                  :icon-caption (getf *window* :title)
                  :double-buffer t
                  :hw t)
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color))
      (let* ((x (/ (getf *window* :width) 2))
             (y (/ (getf *window* :height) 2))
             (surface (sdl:create-surface 40 40 :x (- x 20) :y (- y 20))))
        (sdl:clear-display sdl:*blue* :surface surface)
        (sdl:draw-circle-* 20 20 10 :color sdl:*red* :surface surface)
        (sdl:blit-surface surface)
        (sdl:with-events (:poll)
          (:quit-event () t)
          (:idle ()
                 (let ()
                   (sdl:with-timestep
                     (log-for (output) "Timestep: ~A FPS: ~F" (sdl:ticks) (sdl:average-fps)))
                   (sdl:update-display))))))))

;; Textures
(defun red-circle-texture ()
  (let* ((gl-texture (first (gl:gen-textures 1)))
         (surface (sdl:create-surface 600 600 :x 300 :y 300 :alpha 0 :pixel-alpha 0)))
    (sdl:clear-display (sdl:color :a 0) :surface surface)
    (sdl:draw-filled-circle-* 300 300 150 :surface surface :color sdl:*red*)

    (sdl:with-pixel (var (sdl:fp surface))
      (gl:bind-texture :texture-2d gl-texture)
      (gl:tex-image-2d :texture-2d 0 :rgb 600 600 0 :rgba :unsigned-byte (sdl:pixel-data var))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:bind-texture :texture-2d 0)
      (loop for x from 0 to (1- (sdl:width surface)) do
           (format t "Pixel Data ~A ~%" (sdl:fp (sdl:read-pixel-* x 200 :surface surface))))
      gl-texture)))
(defun font-texture (string)
  (let* ((gl-texture (first (gl:gen-textures 1)))
         (surface (sdl:create-surface 600 600 :alpha 0 :pixel-alpha 0)))
    (sdl:clear-display (sdl:color :a 0) :surface surface)
    (sdl:with-color (sdl:*white*)
      (sdl:with-font (font sdl:*ttf-font-vera*)
        (sdl:initialise-default-font sdl:*ttf-font-vera*)
        (sdl:draw-string-blended-* string 150 300 :surface surface :font font)))

    (sdl:with-pixel (var (sdl:fp surface))
      (gl:bind-texture :texture-2d gl-texture)
      (gl:tex-image-2d :texture-2d 0 :rgb 600 600 0 :rgba :unsigned-byte (sdl:pixel-data var))
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:bind-texture :texture-2d 0)
      gl-texture)))

(defun draw-quad (texture)
  (gl:enable :texture-2d :blend)
  (gl:bind-texture :texture-2d texture)
  (gl:with-pushed-matrix
    (gl:translate (/ (getf *window* :width) 2.0) (/ (getf *window* :height) 2.0) 0)
    (gl:with-primitive :quads
      (gl:tex-coord 0 0)
      (gl:vertex -400 -400)
      (gl:tex-coord 1 0)
      (gl:vertex 400 -400)

      (gl:tex-coord  1 1)
      (gl:vertex 400 400)
      (gl:tex-coord 0 1)
      (gl:vertex -400 400))))
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
      (let ((texture (red-circle-texture))
            (font-texture (font-texture "This is a test")))
        (gl:clear-color 0 0 1 0)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:ortho 0 (getf *window* :width) (getf *window* :height) 0 -1 1)
        (gl:enable :texture-2d :blend)
        (gl:shade-model :smooth)
        (gl:blend-func :src-alpha :one)
        (sdl:with-events (:poll)
          (:quit-event () t)
          (:idle ()
                 (let ()
                   (sdl:with-timestep
                     (log-for (output) "Timestep: ~A FPS: ~F" (sdl:ticks) (sdl:average-fps)))

                   (gl:clear :color-buffer-bit)
                   (gl:matrix-mode :modelview)
                   (gl:load-identity)
                   (draw-quad font-texture) ;; or could be texture for the red circle texture
                   (gl:flush)
                   (sdl:update-display))))))))


