# CL-Quaternion: Quaternions in Common Lisp.

# Usage

Using the package:

    CL-USER> (use-package :quaternion)
    T

Creating a quaternion from real and imaginary components.  The first argument is the real part,
and the rest are the imaginary components.

    CL-USER> (quaternion 10.0 3.0 0.0 0.0)
    #Q(10.0 3.0 0.0 0.0)

Quaternions can be normalized and magnitudes may be computed.

    CL-USER> (qnormalized *)
    #Q(0.95782626 0.28734788 0.0 0.0)
    CL-USER> (qmagnitude *)
    0.99999994

Quaternion addition and multiplication are supported.

    CL-USER> (q+ (quaternion 3 0 0 0) (quaternion 1 1 0 1))
    #Q(4 1 0 1)
    CL-USER> (q* (quaternion 3 0 0 0) (quaternion 1 1 0 1))
    #Q(3 3 0 3)

Unit quaternins may be used to represent rotations.  Functions are
provided for working with quaternions for this purpose.

    CL-USER> (quaternion-from-axis-angle (vector 0.0 0.0 1.0) (/ pi 2))
    #Q(0.7071067811865476d0 0.0d0 0.0d0 0.7071067811865475d0)
    CL-USER> (rotate-vector-with-quaternion (vector 0.0 1.0 0.0))
    ; Evaluation aborted.
    CL-USER> (rotate-vector-with-quaternion 
    #(-1.0d0 2.220446049250313d-16 0.0d0)
    CL-USER> (rotate-vector-by-axis-angle (vector 0.0 1.0 0.0)
					  (vector 0.0 0.0 1.0)
					  pi)
    #(-1.2246467991473532d-16 -1.0d0 0.0d0)
