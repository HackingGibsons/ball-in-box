(in-package :ball-in-box)

(defparameter *visited* nil)

(defun make-shape (bitmap x y)
  (labels ((color-at (point)
             (sdl:fp (sdl:read-pixel-* (svref point 0) (svref point 1) :surface bitmap)))

           (black-p (point)
             (let ((result (equalp #(0 0 0 255) (color-at point))))
               result))

           (onscreen-p (v)
             (let ((x (svref v 0))
                   (y (svref v 1)))
             (and (> x 0)
                  (< x (sdl:width bitmap))
                  (> y 0)
                  (< y (sdl:height bitmap)))))

           (neighbors (p)
             (let ((x (svref p 0))
                   (y (svref p 1)))
               (remove-if-not #L(and (onscreen-p !1) (not (black-p !1)))
                              (list (vector (1+ x) (1+ y))
                                    (vector x (1+ y))
                                    (vector (1- x) (1+ y))
                                    (vector (1- x) y)
                                    (vector (1- x) (1- y))
                                    (vector x (1- y))
                                    (vector (1+ x) (1- y))
                                    (vector (1+ x) y)))))
           (leaf-p (p)
             (let ((n (neighbors p)))
               (or (< (length n) 8)
                   (find-if #L(not (equalp (color-at p) (color-at !1)))
                            n)))))
    (let ((visited (make-hash-table :test 'equalp)))
      (labels ((visited-p (p)
                 (gethash p visited))
               (visit (p)
                 (setf (gethash p visited) (hash-table-count visited)))
               (walk (p)
                 (let ((friends (remove-if #L(or (visited-p !1)
                                                 (not (leaf-p !1)))
                                           (neighbors p))))
                   (cond ((visited-p p) :done)
                         (t
                          (visit p)
                          (mapcar #'walk friends))))))
        (walk (vector x y)))
      (arnesi:hash-table-keys visited))))

           ;; (edge-direction (p &optional exclude-vector)
           ;;   (let* ((n (neighbors p))
           ;;          (leafs (remove-if-not #'leaf-p n))
           ;;          (leafs (if exclude-vector
           ;;                     (remove-if #'zerop leafs :key #L(round (angle exclude-vector
           ;;                                                                   (diff p !1))))
           ;;                     leafs)))
           ;;     (when leafs
           ;;       (diff (car leafs) p))))

           ;; (longest-vector (point v)
           ;;   (do* ((longest v (mag+ longest))
           ;;         (end (map 'vector #'round (sum longest point))
           ;;              (map 'vector #'round (sum longest point))))
           ;;        ((or (black-p end) (not (leaf-p end)))
           ;;         (mag+ longest -1)))))

           ;; (do* ((start (vector x y) start)
           ;;       (points nil (append points (list p)))
           ;;       (last nil (longest-vector p direction))
           ;;       (p start (map 'vector #'round (sum last p)))
           ;;       (direction (edge-direction p) (edge-direction p last)))
           ;;      ((and last (equalp p start)) points))))


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

        (gl:shade-model :smooth)
        (gl:clear-color 0 0 0 1)

        (gl:depth-func :lequal)
        (gl:matrix-mode :projection)
        (gl:enable :depth-test)
        (gl:load-identity)

        (gl:ortho 0 1024 768 1 -1 1)
        (let* ((bitmap (sdl-image:load-image (merge-pathnames (pathname "src/test.bmp")
                                                              (asdf:system-source-directory :ball-in-box)) :image-type :bitmap))
               (shapes (make-hash-table :test 'equalp)))

          (sdl:update-display bitmap)


          (loop for x from 0 to (sdl:width bitmap) do
               (loop for y from 0 to (sdl:height bitmap)
                     for color = (sdl:fp (sdl:read-pixel-* x y :surface bitmap))
                  do
                    (unless (or (equalp #(0 0 0 255) color) (gethash color shapes))
                      (setf (gethash color shapes) (make-shape bitmap x y))
                      (format t "Color ~A Points: ~A~%" color (length (gethash color shapes))))))

          (format t "shapes: ~A~%" shapes)

          (sdl:with-events (:poll)
            (:quit-event () t)
            (:idle ()

                   (gl:matrix-mode :modelview)
                   (gl:load-identity)
                   (gl:translate (- (/ 1024 2) 520) (- (/ 768 2) 280)  0)
                   (gl:scale 3 3 0)
                   (gl:clear :color-buffer-bit :depth-buffer-bit)
                   (maphash #'(lambda (color points)
                                (apply #'gl:color (map 'list #'identity color))
                                (gl:with-primitive :points
                                  (dolist (point points)
                                    (gl:vertex (svref point 0) (svref point 1)))))
                            shapes)
                   (gl:flush)
                   (sdl:update-display)
                   t))))))
