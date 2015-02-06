(in-package :mgl-mat)

(defsection @mat-foreign (:title "Foreign arrays")
  "One facet of MAT objects is [FOREIGN-ARRAY][facet-name] which is
  backed by a memory area that can be pinned or is allocated in
  foreign memory depending on *FOREIGN-ARRAY-STRATEGY*."
  (foreign-array class)
  (*foreign-array-strategy* (variable "-see below-"))
  (foreign-array-strategy type)
  (pinning-supported-p function))

(defclass foreign-pool ()
  ((n-static-arrays :initform 0 :accessor n-static-arrays)
   (n-static-bytes-allocated :initform 0 :accessor n-static-bytes-allocated)
   (lock :initform (bordeaux-threads:make-recursive-lock) :reader lock)))

(defvar *foreign-pool* (make-instance 'foreign-pool))

(defmacro with-foreign-pool-locked ((pool) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock ,pool))
     ,@body))

(defun foreign-room (&key (stream *standard-output*) (verbose t))
  (if verbose
      (format stream "Foreign memory usage:~%~
                     static arrays: ~S (used bytes: ~:D)~%"
              (n-static-arrays *foreign-pool*)
              (n-static-bytes-allocated *foreign-pool*))
      (format stream "s: ~S (~:D)~%"
              (n-static-arrays *foreign-pool*)
              (n-static-bytes-allocated *foreign-pool*))))


;;; FOREIGN-ARRAY is to CFFI:FOREIGN-POINTER what CUDA-ARRAY is to
;;; CU-DEVICE-PTR.
(defclass foreign-array (offset-pointer)
  ((n-references :initform 1 :initarg :n-references :accessor n-references)
   (lock :initform (bordeaux-threads:make-recursive-lock) :accessor lock)
   ;; Somewhat out-of-place, this slot is to tie the shared
   ;; FOREIGN-ARRAY facet to a cuda pool so that unregistering of
   ;; memory can be done from arbitrary threads. When the
   ;; CUDA-HOST-ARRAY facet is created this is set to the pool.
   (cuda-pool :initform nil :accessor cuda-pool))
  (:documentation "[FOREIGN-ARRAY][class] wraps a foreign pointer (in
  the sense of CFFI:POINTERP). That is, both OFFSET-POINTER and
  BASE-POINTER return a foreign pointer. There are no other public
  operations that work with [FOREIGN-ARRAY][class] objects, their sole
  purpose is represent facets of MAT objects."))

(defmacro with-foreign-array-locked ((foreign-array) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock ,foreign-array))
     ,@body))

(defun alloc-static-vector (ctype length initial-element)
  (prog1
      (if initial-element
          (static-vectors:make-static-vector
           length :element-type (ctype->lisp ctype)
           :initial-element initial-element)
          (static-vectors:make-static-vector
           length :element-type (ctype->lisp ctype)))
    (with-foreign-array-locked (*foreign-pool*)
      (incf (n-static-arrays *foreign-pool*))
      (incf (n-static-bytes-allocated *foreign-pool*)
            (* length (ctype-size ctype))))))

(defun free-static-vector (vector)
  (let ((n-bytes (* (array-total-size vector)
                    (ctype-size (lisp->ctype (array-element-type vector))))))
    (static-vectors:free-static-vector vector)
    (with-foreign-array-locked (*foreign-pool*)
      (decf (n-static-arrays *foreign-pool*))
      (assert (not (minusp (n-static-arrays *foreign-pool*))))
      (decf (n-static-bytes-allocated *foreign-pool*) n-bytes)
      (assert (not (minusp (n-static-bytes-allocated *foreign-pool*)))))))

;;;; Foreign array strategy

(deftype foreign-array-strategy ()
  "One of :PINNED, :STATIC and :CUDA-HOST. See
  *FOREIGN-ARRAY-STRATEGY* for their semantics."
  '(member :pinned :static :cuda-host))

(defun pinning-supported-p ()
  "Return true iff the lisp implementation efficiently supports
pinning lisp arrays. Pinning ensures that the garbage collector
doesn't move the array in memory. Currently this is only supported on
SBCL gencgc platforms."
  #+(and sbcl gencgc) t
  #-(and sbcl gencgc) nil)

(defvar *foreign-array-strategy*
  (if (pinning-supported-p)
      :pinned
      :static)
  "One of :PINNED, :STATIC and :CUDA-HOST (see type
  FOREIGN-ARRAY-STRATEGY). This variable controls how foreign arrays
  are handled and it can be changed at any time.

  If it's :PINNED (only supported if (PINNING-SUPPORTED-P), then no
  separate storage is allocated for the foreign array. Instead, it
  aliases the lisp array (via the BACKING-ARRAY facet).

  If it's :STATIC, then the lisp backing arrays are allocated
  statically via the static-vectors library. On some implementations,
  explicit freeing of static vectors is necessary, this is taken care
  of by finalizers or can be controlled with WITH-FACET-BARRIER.
  DESTROY-CUBE and DESTROY-FACET may also be of help.

  :CUDA-HOST is the same as :STATIC, but any copies to/from the
  GPU (i.e. the [CUDA-ARRAY][facet-name] facet) will be done via the
  [CUDA-HOST-ARRAY][facet-name] facet whose memory pages will also be
  locked and registered with `cuMemHostRegister` which allows quicker
  and asynchronous copying to and from CUDA land.

  The default is :PINNED if available, because it's the most
  efficient. If pinning is not available, then it's :STATIC.")

(defun use-pinning-p ()
  (eq *foreign-array-strategy* :pinned))
