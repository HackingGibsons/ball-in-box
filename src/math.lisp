(in-package :ball-in-box)

;; Conversion
(defun deg-to-rad (deg)
  (* deg (/ pi 180)))

;; Some vector helpers
(defmethod dot ((v1 vector) (v2 vector))
  "Dot product of two vectors: `v1' . `v2'"
  (reduce #'+ (map 'vector #'* v1 v2)))

(defmethod mag+ ((v1 vector) &optional (scale 1))
  (let ((normalized (map 'vector #L(* !1 scale) (normalize v1))))
    (sum v1 normalized)))

(defmethod normalize ((v1 vector))
  (let ((mag (mag v1)))
    (map 'vector #L(/ !1 mag) v1)))

(defmethod angle ((v1 vector) (v2 vector))
  (acos (dot (normalize v1)
             (normalize v2))))

(defmethod diff ((v1 vector) (v2 vector))
  "Vector difference of `v1' and `v2'"
  (map 'vector #'- v1 v2))

(defmethod sum ((v1 vector) (v2 vector))
  "Vector sum of `v1' and `v2'"
  (map 'vector #'+ v1 v2))

(defmethod mag ((v1 vector))
  "Vector magnitude of `v1'"
  (sqrt (dot v1 v1)))

(defmethod diff-mag ((v1 vector) (v2 vector))
  "The magnitude of a difference of `v1' `v2'"
  (mag (diff v1 v2)))
