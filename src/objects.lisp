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
  "Signal a collision between `o' and `objects'"
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

(defmethod collision :after ((o accelerating-object) &rest others)
  (declare (ignorable others))
  (log-for (trace solid-object) "Decelerating: ~A" o)
  (setf (accel o) #(0 0 0)))

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
(defclass circle (object)
  ((color :initform `(255 255 255)
          :initarg :color
          :accessor color)
   (radius :initform 100
           :accessor radius :accessor r
           :initarg :radius :initarg :r)))

(defmethod vertexes ((o circle))
  (let ((arc-deg 10))
    (loop for angle from 0 to 360 by arc-deg collecting
         (vector (+ (cx o) (* (radius o) (sin (deg-to-rad angle))))
                 (+ (cy o) (* (radius o) (cos (deg-to-rad angle))))
                 (cz o)))))

(defmethod draw ((o circle))
  (when (color o)
    (apply #'gl:color (color o)))

  (gl:with-primitive :triangle-fan
    (gl:vertex (cx o) (cy o) (cz o))
    (mapc #L(apply #'gl:vertex (map 'list #'identity !1))
          (vertexes o))))


(defclass solid-rectangle (rectangle solid-object)
  ())

(defclass moving-rectangle (rectangle moving-object)
  ())

(defclass accelerating-rectangle (rectangle accelerating-object)
  ())

(defclass solid-accelerating-rectangle (rectangle accelerating-object solid-object)
  ())

(defclass solid-accelerating-circle (circle accelerating-object solid-object)
  ())


;; Textured Quad
(defclass gl-texture-mixin ()
    ((tex-id :reader tex-id :initform (first (gl:gen-textures 1)))))

(defclass sdl-overlay (rectangle gl-texture-mixin)
  ((surface :accessor overlay-surface :initform nil)))

(defmethod initialize-instance :after ((o gl-texture-mixin) &key)
  (gl:bind-texture :texture-2d (tex-id o))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:bind-texture :texture-2d 0))

(defmethod initialize-instance :after ((o sdl-overlay) &key)
  (with-slots (surface width height tex-id) o
    (setf surface
          (sdl:create-surface width height :alpha 0 :pixel-alpha 0))
    (gl:bind-texture :texture-2d tex-id)
    (sdl:with-color (sdl:*white*)
      (sdl:with-font (font sdl:*ttf-font-vera*)
        (sdl:clear-display (sdl:color :a 0) :surface surface)
        (sdl:draw-string-solid-* "Texture" 0 0 :surface surface)))
    (sdl:with-pixel (pixels (sdl:fp surface))
      (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgba :unsigned-byte (sdl:pixel-data pixels)))
    (gl:bind-texture :texture-2d 0)))

(defmethod tex-coords ((o sdl-overlay))
  (list (vector 0 0 (cz o))
        (vector 0 1 (cz o))
        (vector 1 1 (cz o))
        (vector 1 0 (cz o))))

(defmethod draw ((o sdl-overlay))
  (gl:enable :texture-2d :blend)
  (gl:color 1 1 1 1)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:bind-texture :texture-2d (tex-id o))

  (gl:with-primitive :polygon
    (mapc #'(lambda (tex v)
              (apply #'gl:tex-coord (map 'list #'identity tex))
              (apply #'gl:vertex (map 'list #'identity v)))
          (tex-coords o)
          (vertexes o)))

  (gl:bind-texture :texture-2d 0)
  (gl:disable :texture-2d :blend))
