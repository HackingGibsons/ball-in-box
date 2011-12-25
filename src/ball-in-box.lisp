(in-package :ball-in-box)

;; Debugging
(defcategory fps)

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
    (add-object world
                (make-instance 'accelerating-rectangle :width 25 :height 25 :color '(0 0 255) :center #(150 -350 0)
                               :acceleration (vector 0 (gravity world) 0)))

    (add-object world
                (make-instance 'accelerating-rectangle :width 35 :height 25 :color '(255 0 255) :center #(-150 -350 0)
                               :acceleration (vector 0 (* 4 (gravity world)) 0)))

    (add-object world
                (make-instance 'moving-rectangle :width 30 :height 30 :color '(0 255 0) :center #(-100 0 0.5)
                               :v #(10 5 0)))

    (add-object world
                (make-instance 'rectangle :width 100 :height 100))

    (add-object world
                (make-instance 'solid-accelerating-circle :radius 30 :color '(255 0 100) :center #(-300 -350 0)
                               :acceleration (vector 0 (* 8 (gravity world)) 0)))
    (add-object world
                (make-instance 'solid-rectangle :width 40 :height 40 :center #(-300 100 0)))

    (add-object world
                (make-instance 'circle :radius 150 :center #(300 0 0)))

    (push (make-instance 'sdl-overlay :width 800 :height 600)
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
       (log-for (fps) "FPS: ~F" (sdl:average-fps))
       (sdl:update-display)))))
