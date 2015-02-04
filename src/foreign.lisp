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

(defun alloc-foreign-array (type &key count)
  (make-instance 'foreign-array
                 :base-pointer (cffi:foreign-alloc type :count count)
                 :n-bytes (* count (ctype-size type))))

(defun free-foreign-array (foreign-array)
  (with-foreign-array-locked (foreign-array)
    (assert (plusp (n-references foreign-array)) ()
            "Can't free already freed FOREIGN-ARRAY.")
    (decf (n-references foreign-array))
    (when (zerop (n-references foreign-array))
      (assert (null (cuda-pool foreign-array)) ()
              "Can't free foreign array while it's registered in CUDA.")
      (let ((base-pointer (base-pointer foreign-array)))
        (assert base-pointer)
        (setf (slot-value foreign-array 'base-pointer) nil)
        (cffi:foreign-free base-pointer)))))

;;;; Foreign array strategy

(deftype foreign-array-strategy ()
  "One of :PINNED, :STATIC, :CUDA-HOST and :DYNAMIC. See
  *FOREIGN-ARRAY-STRATEGY* for their semantics."
  '(member :pinned :static :cuda-host :dynamic))

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
  "One of :PINNED, :STATIC and :DYNAMIC (see type
  FOREIGN-ARRAY-STRATEGY). This variable controls how foreign arrays
  are handled and it can be changed at any time.

  If it's :PINNED (only supported if (PINNING-SUPPORTED-P), then no
  separate storage is allocated for the foreign array. Instead, it
  aliases the lisp array (via the BACKING-ARRAY facet).

  If it's :STATIC, then the lisp backing arrays are allocated
  statically via the static-vectors library. On some implementations,
  explicit freeing of static vectors is necessary, this is taken care
  of by finalizers or can be controlled with WITH-FACET-BARRIER.

  :CUDA-HOST is the same as :STATIC, but any copies to/from the
  GPU (i.e. the CUDA-ARRAY facet) will be done via the CUDA-HOST-ARRAY
  facet whose memory pages are locked in will also be locked and
  registered with cuMemHostRegister which allows quicker and
  asynchronous copying to and from CUDA land.

  If it's :DYNAMIC, then each time the foreign array is needed, it's
  allocated and freed dynamically.

  The default is :PINNED if available, because it's the most
  efficient. If pinning is not available, then it's :STATIC.")

(defun use-pinning-p ()
  (eq *foreign-array-strategy* :pinned))
