(in-package :ball-in-box)

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
    (push (make-instance 'moving-rectangle :width 30 :height 30 :color '(0 255 0) :center #(-100 0 0)
                         :v #(0.001 0.001 0))
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
