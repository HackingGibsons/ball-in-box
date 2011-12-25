(in-package :ball-in-box)

(defclass object ()
  ((world :initform nil :initarg :world :accessor world)
   (center :initform #(0 0 0) :initarg :center
           :accessor center
           :accessor c)))

;; Center coordinate accessors
(defmethod cx ((o object))
  (svref (center o) 0))
(defmethod cy ((o object))
  (svref (center o) 1))
(defmethod cz ((o object))
  (svref (center o) 2))

(defmethod distance ((o1 object) (o2 object))
  "Distance between two objects."
  (diff-mag (center o1) (center o2)))
(defmethod vertexes ((o object)) nil)
(defmethod draw ((o object)) nil)
(defmethod tick ((o object) dt) nil)
(defmethod radius ((o object))
  "Calculate a generalized radius for `o' by determining the
furthest point from center and reporting its distance."
  (let ((distances (mapcar #L(diff-mag (center o) !1)
                           (vertexes o))))
    (and distances
         (apply #'max distances))))

(defcategory solid-object)
(defclass solid-object (object)
  ())

(defmethod collision ((o solid-object) &rest objects)
  (declare (ignorable objects))
  nil)

(defmethod tick :after ((o solid-object) dt)
  "Attempt to signal collisions."
  (let* ((solids (remove-if-not #L(and (not (eq !1 o))
                                       (typep !1 'solid-object))
                                (objects (world o))))
         (radius (radius o))
         (in-radius (remove-if #L(> (distance o !1)
                                    (+ radius (radius !1)))
                               solids)))
    (when in-radius
      (apply #'collision o in-radius))))

(defclass moving-object (object)
  ((velocity :initform #(0 0 0)
             :initarg :velocity :initarg :v
             :accessor velocity :accessor v)))

(defmethod collision :after ((o accelerating-object) &rest others)
  (declare (ignorable others))
  (log-for (trace solid-object) "Decellerating: ~A" o)
  (setf (accel o) #(0 0 0)))

(defmethod collision :after ((o moving-object) &rest others)
  (declare (ignorable others))
  (log-for (trace solid-object) "Stopping: ~A" o)
  (setf (v o) #(0 0 0)))

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

(defclass solid-rectangle (rectangle solid-object)
  ())

(defclass moving-rectangle (rectangle moving-object)
  ())

(defclass accelerating-rectangle (rectangle accelerating-object)
  ())

(defclass solid-accelerating-rectangle (rectangle accelerating-object solid-object)
  ())

(defmethod vertexes ((o rectangle))
  "Returns a list of vector3 vertexes in drawing order
for the object"
  (flet ((half (v) (/ v 2)))
    (let ((top (+ (cy o) (half (height o))))
          (bottom (- (cy o) (half (height o))))
          (left (- (cx o) (half (width o))))
          (right (+ (cx o) (half (width o)))))

      (list (vector left bottom (cz o))
            (vector left top (cz o))
            (vector right top (cz o))
            (vector right bottom (cz o))))))


(defmethod draw ((o rectangle))
  (when (color o)
    (apply #'gl:color (color o)))

  (gl:with-primitive :polygon
    (mapc #L(apply #'gl:vertex (map 'list #'identity !1))
          (vertexes o))))
