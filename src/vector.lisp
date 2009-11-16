(in-package :quat)

(defun vector-magnitude (v)
  (sqrt (reduce #'(lambda (sum x) (+ sum (* x x))) v :initial-value 0)))

(defun vector-normalized (v)
  (let ((mag (vector-magnitude v)))
    (map 'vector #'(lambda (x) (/ x mag)) v)))

(defun vector-dot (v1 v2)
  (reduce #'+ (map 'vector #'* v1 v2) :initial-value 0))

(defun vector-add (&rest vectors)
  (apply #'map 'vector #'+ vectors))

(defun vector-scale (s v)
  (map 'vector #'(lambda (x) (* x s)) v))

(defun centroid (&rest vectors)
  (vector-scale (/ 1 (length vectors)) (apply 'vector-add vectors)))
  
(defun equidistant-hyperplane (v1 v2)
  "Returns W and B as 2 values for the W.X = b where W and X are
vectors and b is a scalar."
  (let ((w (vector-add v2 (vector-scale -1 v1)))
	 (point-on-line (centroid v1 v2)))
    (values w (vector-dot w point-on-line))))
