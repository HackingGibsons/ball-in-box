(in-package :ball-in-box)

(defclass object ()
  ((center :initform #(0 0 0) :initarg :center
           :accessor center
           :accessor c)))

(defmethod cx ((o object))
  (svref (center o) 0))

(defmethod cy ((o object))
  (svref (center o) 1))

(defmethod cz ((o object))
  (svref (center o) 2))

(defmethod draw ((o object))
  (log-for (output warn) "Trying to draw a singularity: ~A" o))

(defmethod tick ((o object) dt) nil)

(defclass moving-object (object)
  ((velocity :initform #(0 0 0)
             :initarg :velocity :initarg :v
             :accessor velocity :accessor v)))

(defmethod tick :before ((o moving-object) dt)
  "Translate the moving object `o' along it's velocity vector scaled for timeslice `dt'"
  (let* ((scale (/ 1000 dt))
         (scaled (map 'vector #'(lambda (i) (* scale i)) (velocity o))))
    (setf (center o)
          (map 'vector #'+ (center o) scaled))))

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

(defclass moving-rectangle (rectangle moving-object)
  ())

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
