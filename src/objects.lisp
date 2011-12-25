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
