(defpackage :cl-quaternion
    (:nicknames :quat :cl-quat :quaternion :hamilton)
  (:use "COMMON-LISP")
  (:export
   ;; construction
   #:quaternion
   #:make-quaternion
   ;; predicate
   #:quaternionp
   ;; common mathematical operations
   #:q*
   #:q+
   #:generic+
   #:generic*
   ;; normalization/length
   #:qmagnitude
   #:qmagnitude-squared
   #:qnormalized
   ;; rotations
   #:rotate-vector-with-quaternion
   #:rotate-vector-by-axis-angle
   #:quaternion-from-axis-angle
   ;; accessing the components
   #:bind-quaternion
   #:quaternion-ri
   #:quaternion-jk))

(in-package :cl-quaternion)

;; from the code published here:
;; http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/9acf0a2e4e498775/97ea371a5881f5bc?hl=en&ie=UTF-8&q=lisp+quaternion&pli=1

(defclass quaternion ()
  ((ri :type number :initarg :ri :initform 0 :reader quaternion-ri)
   (jk :type number :initarg :jk :initform 0 :reader quaternion-jk)))

(defun make-quaternion (&rest args)
  (apply 'make-instance 'quaternion args))

(defun quaternion (r &optional (i 0) (j 0) (k 0))
  "Returns a quaternion with R I J K as the components."
  (make-quaternion :ri (complex r i) :jk (complex j k)))

(defun quaternionp (obj)
  "Returns T if OBJ is a quaternion, NIL otherwise."
  (typep obj 'quaternion))

(defmacro bind-quaternion ((r i j k) quat &body body)
  "Binds the real, i, j, and k components of the given quaternion and then evaluates body.

R I J K -- not evaluated, variable names used to bind the quat components

quat -- evaluated, quaternion to bind

body -- forms evaluated after binding R I J K."
  (let ((q (gensym "quat"))
	(ri (gensym "ri"))
	(jk (gensym "jk")))
    `(multiple-value-bind (,r ,i ,j ,k)
	 (let* ((,q ,quat)
		(,ri (quaternion-ri ,q))
		(,jk (quaternion-jk ,q)))
	   (values
	     (realpart ,ri)
	     (imagpart ,ri)
	     (realpart ,jk)
	     (imagpart ,jk)))
       ,@body)))

(defmethod print-object ((self quaternion) stream)
  (bind-quaternion (r i j k)
      self
    (format stream "#Q(~A ~A ~A ~A)" r i j k))
  stream)

;; TODO: Add a read syntax for #Q(r i j k)

(defun q+ (&rest args)
  "Adds multipl quaternions."
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (ri   (quaternion-ri (car args))
	     (common-lisp:+ ri (quaternion-ri (car args))))
	   (jk   (quaternion-jk (car args))
	     (common-lisp:+ jk (quaternion-jk (car args)))))
	  ((null args) (make-quaternion :ri  ri :jk jk)))));;q+

(defun q* (&rest args)
  "Multiplies multiple quaternions"
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
           (ri   (quaternion-ri (car args))
                 (common-lisp:-
                  (common-lisp:* ri (quaternion-ri (car args)))
                  (common-lisp:* jk (conjugate
                                     (quaternion-jk (car args))))))
           (jk   (quaternion-jk (car args))
                 (common-lisp:+
                  (common-lisp:* ri (quaternion-jk (car args)))
                  (common-lisp:* jk (conjugate
                                     (quaternion-ri (car args)))))))
          ((null args) (make-quaternion :ri  ri :jk jk)))));;q*

(defun qmagnitude-squared (q)
  "Returns the magnitude of the quaternion squared (sum of squared components)."
  (bind-quaternion (r i j k)
      q
    (+ (* r r) (* i i) (* j j) (* k k))))

(defun qmagnitude (q)
  "Returns the magnitude of the quaternion."
  (sqrt (qmagnitude-squared q)))

(defun qconjugate (q)
  "Returns the conjugate of a quaternion."
  (bind-quaternion (r i j k)
      q
    (quaternion r (- i) (- j) (- k))))

(defun qnormalized (q)
  "Returns a version of the quaternion with unit length."
  (let ((mag (qmagnitude q)))
    (bind-quaternion (r i j k)
	q
      (quaternion (/ r mag) (/ i mag) (/ j mag) (/ k mag)))))

(defun quaternion-from-axis-angle (axis angle)
  "Given an axis vector and an angle (in radians), returns a 4-point vector. #(x y z w)"
  (let ((sin-result (sin (/ angle 2))))
    (quaternion (cos (/ angle 2))
		(* sin-result (elt axis 0))
		(* sin-result (elt axis 1))
		(* sin-result (elt axis 2)))))

(defun unit-vector (vector)
  "Given a vector returns the unit vector (each component is divided by its magnitude)."
  (let ((sum (reduce #'+ vector)))
    (map 'vector #'(lambda (x) (/ x sum)) vector)))

(defun rotate-vector-with-quaternion (vector quat)
  "Given a vector, rotates it using the given quaternion as an orientation."
  (let* ((q-vector (quaternion 0 (elt vector 0) (elt vector 1) (elt vector 2)))
	 (q-result (q* quat q-vector (qconjugate quat))))
    (bind-quaternion (r i j k)
	q-result
      (declare (ignore r))
      (vector i j k))))

(defun rotate-vector-by-axis-angle (vector axis angle)
  "Given a 3-point VECTOR and a UNIT vector axis and an ANGLE in radians,
returns a VECTOR rotated about AXIS by ANGLE in a counter-clockwise direction."
  (let ((q-rotation (quaternion-from-axis-angle axis angle))
	(q-vector (quaternion 0 (elt vector 0) (elt vector 1) (elt vector 2))))
    (let ((q-result (q* q-rotation q-vector (qconjugate q-rotation))))
      (vector (imagpart (quaternion-ri q-result))
	      (realpart (quaternion-jk q-result))
	      (imagpart (quaternion-jk q-result))))))

;; example usage:
;; CL-USER> (defun d2r (d)  (* pi (/ d 180.0)))
;; CL-USER> (rotate-vector-by-axis-angle (vector 1.0 0.0 0.0)  (vector 0.0 0.0 1.0)  (d2r 90))
;; #(2.220446049250313d-16 1.0d0 0.0d0)

;;; generic addition etc
(defun generic+ (&REST ARGS)
  "
DO:    A Generic addition with numbers or quaternions.
"
  (cond
    ((every #'numberp args)
     (apply #'cl:+ args))
    ((every #'quaternionp args)
     (apply (function q+) args))
    ((some (lambda (x) (not (or (numberp x) (quaternionp x)))) args)
     (ERROR "Incompatible types for '+': ~S"
            (MAPCAR (FUNCTION TYPE-OF) ARGS)))
    (t (apply (function q+)
              (mapcar
               (lambda (x) (cond
                        ((quaternionp x) x)
                        ((complexp x) (quaternion (realpart x) (imagpart x)))
                        (t (quaternion x))))  args)))));;+

(defun generic* (&REST ARGS)
  "
DO:    A Generic multiplication with numbers or quaternions.
"
  (COND
    ((EVERY (FUNCTION NUMBERP) ARGS)
     (APPLY (FUNCTION COMMON-LISP:*) ARGS))
    ((EVERY (FUNCTION QUATERNIONP) ARGS)
     (apply (function q*) args))
    ((some (lambda (x) (not (or (numberp x) (quaternionp x)))) args)
     (ERROR "Incompatible types for '*': ~S"
            (MAPCAR (FUNCTION TYPE-OF) ARGS)))
    (t (apply (function q*)
              (mapcar
               (lambda (x) (cond
                        ((quaternionp x) x)
                        ((complexp x) (quaternion (realpart x) (imagpart x)))
                        (t (quaternion x)))) args)))));;*

;;; experimental / nonfunctional

(defun qdot (q1 q2)
  "Dot product of multiple quaternions."
  (bind-quaternion (r1 i1 j1 k1)  q1
    (bind-quaternion (r2 i2 j2 k2)  q2
      (+ (* r1 r2) (* i1 i2) (* j1 j2) (* k1 k2)))))

(defun qslerp(q1 q2 t)
  "Assumes that q1 and q2 are normalized quaternions that represent rotations.  Computes
an inerpolated rotation between the two, where 0 < T < 1 and T is the degree to which
the interpolation should be at q2 vs. q1."
  nil)