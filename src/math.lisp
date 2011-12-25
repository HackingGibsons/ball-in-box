(in-package :ball-in-box)

;; Some vector helpers
(defmethod dot ((v1 vector) (v2 vector))
  "Dot product of two vectors: `v1' . `v2'"
  (reduce #'+ (map 'vector #'* v1 v2)))

(defmethod diff ((v1 vector) (v2 vector))
  "Vector difference of `v1' and `v2'"
  (map 'vector #'- v1 v2))
