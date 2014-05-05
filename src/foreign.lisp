(in-package :mgl-mat)

(defsection @mat-foreign (:title "Foreign arrays")
  "One facet of MAT objects is [FOREIGN-ARRAY][facet-name] which is
  backed by a memory area that can be pinned or is allocated in
  foreign memory depending on *FOREIGN-ARRAY-STRATEGY*."
  (foreign-array class)
  (*foreign-array-strategy* (variable "-see below-"))
  (foreign-array-strategy type)
  (pinning-supported-p function))

;;; FOREIGN-ARRAY is to CFFI:FOREIGN-POINTER what CUDA-ARRAY is to
;;; CU-DEVICE-PTR.
(defclass foreign-array (offset-pointer) ()
  (:documentation "[FOREIGN-ARRAY][class] wraps a foreign pointer (in
  the sense of CFFI:POINTERP). That is, both OFFSET-POINTER and
  BASE-POINTER return a foreign pointer. There are no other public
  operations that work with [FOREIGN-ARRAY][class] objects, their sole
  purpose is represent facets of MAT objects."))

(defun alloc-foreign-array (type &key count)
  (make-instance 'foreign-array
                 :base-pointer (cffi:foreign-alloc type :count count)))

(defun free-foreign-array (foreign-array)
  (let ((base-pointer (base-pointer foreign-array)))
    (assert base-pointer)
    (setf (slot-value foreign-array 'base-pointer) nil)
    (cffi:foreign-free base-pointer)))

;;;; Foreign array strategy

(deftype foreign-array-strategy ()
  "One of :PIN-BACKING-ARRAY, :STATIC-BACKING-ARRAY, :DYNAMIC. See
  *FOREIGN-ARRAY-STRATEGY* for their semantics."
  '(member :pin-backing-array :static-backing-array :dynamic))

(defun pinning-supported-p ()
  "Return true iff the lisp implementation efficiently supports
pinning lisp arrays. Pinning ensures that the garbage collector
doesn't move the array in memory. Currently this is only supported on
SBCL gencgc platforms."
  #+(and sbcl gencgc) t
  #-(and sbcl gencgc) nil)

(defvar *foreign-array-strategy*
  (if (pinning-supported-p)
      :pin-backing-array
      :static-backing-array)
  "One of :PIN-BACKING-ARRAY, :STATIC-BACKING-ARRAY and :ALLOCATE (see
  type FOREIGN-ARRAY-STRATEGY). This variable controls how foreign
  arrays are handled and it can be changed at any time.

  If it's :PIN-BACKING-ARRAY (only supported if (PINNING-SUPPORTED-P),
  then no separate storage is allocated for the foreign array, instead
  it aliases the lisp array (via the BACKING-ARRAY facet).

  If it's :STATIC-BACKING-ARRAY, then the lisp backing arrays are
  allocated statically via the static-vectors library. On some
  implementations, explicit freeing of static vectors is necessary,
  this is taken care of by finalizers or can be controlled with
  WITH-FACET-BARRIER.

  If it's :DYNAMIC, then each time the foreign array is needed, it's
  allocated and freed dynamically.

  The default is :PIN-BACKING-ARRAY if available, because it's the most
  effecient. If pinning is not available, then
  it's :STATIC-BACKING-ARRAY.")

(defun use-pinning-p ()
  (eq *foreign-array-strategy* :pin-backing-array))
