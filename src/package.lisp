(mgl-pax:define-package :mgl-cube
  (:documentation "See MGL-CUBE:@CUBE-MANUAL.")
  (:use #:common-lisp #:mgl-pax))

(mgl-pax:define-package :mgl-mat
  (:documentation "See MGL-MAT:@MAT-MANUAL.")
  (:use #:common-lisp #:mgl-pax #:mgl-cube #:cl-cuda))
