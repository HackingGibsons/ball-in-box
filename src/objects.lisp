(in-package :ball-in-box)

(defclass object ()
  ((center :initform #(0 0 0) :initarg :center
           :accessor center
           :accessor c)))

;; Center coordinate accessors
(defmethod cx ((o object))
  (svref (center o) 0))
(defmethod cy ((o object))
  (svref (center o) 1))
(defmethod cz ((o object))
  (svref (center o) 2))

(defmethod draw ((o object)) nil)
(defmethod tick ((o object) dt) nil)

(defclass moving-object (object)
  ((velocity :initform #(0 0 0)
             :initarg :velocity :initarg :v
             :accessor velocity :accessor v)))

(defmethod tick :before ((o moving-object) dt)
  "Translate the moving object `o' along it's velocity vector scaled for timeslice `dt'"
  (let* ((scale (/ dt 1000))
         (scaled (map 'vector #L(* scale !1) (velocity o))))
    (setf (center o)
          (map 'vector #'+ (center o) scaled))))

(defclass accelerating-object (moving-object)
  ((acceleration :initform #(0 0 0)
                 :initarg :acceleration :initarg :accel
                 :accessor acceleration :accessor accel)))

(defmethod tick :before ((o accelerating-object) dt)
  (let* ((scale (/ dt 1000))
         (scaled (map 'vector #L(* scale !1) (acceleration o))))
    (log-for (output) "Accel: ~A" scaled)
    (setf (velocity o)
          (map 'vector #'+ (velocity o) scaled))
    (log-for (output) "New v of ~A: ~A" o (velocity o))))

;;; Demo objects
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

(defclass accelerating-rectangle (rectangle accelerating-object)
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
