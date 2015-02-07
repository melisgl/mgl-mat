(in-package :mgl-mat)

(defsection @mat-cuda (:title "CUDA")
  (cuda-available-p function)
  (with-cuda* macro)
  (call-with-cuda function)
  (*cuda-enabled* variable)
  (cuda-enabled (accessor mat))
  (*default-mat-cuda-enabled* variable)
  (*n-memcpy-host-to-device* variable)
  (*n-memcpy-device-to-host* variable)
  (*cuda-default-device-id* variable)
  (*cuda-default-random-seed* variable)
  (*cuda-default-n-random-states* variable)
  (@mat-cuda-memory-management section))

(defsection @mat-cuda-extensions (:title "CUDA Extensions")
  (use-cuda-p function)
  (choose-1d-block-and-grid function)
  (choose-2d-block-and-grid function)
  (choose-3d-block-and-grid function)
  (define-cuda-kernel macro)
  (@mat-cublas section)
  (@mat-curand section))

(defkernelmacro when (test &body body)
  `(if ,test
       (progn ,@body)))

(defkernelmacro 1+ (form)
  `(+ 1 ,form))

(defkernelmacro incf (place &optional (delta 1))
  `(set ,place (+ ,place ,delta)))

(defkernelmacro setf (place value)
  `(set ,place ,value))

(defvar *cuda-enabled* t
  "Set or bind this to false to disable all use of cuda. If this is
  done from within WITH-CUDA*, then cuda becomes temporarily disabled.
  If this is done from outside WITH-CUDA*, then it changes the default
  values of the ENABLED argument of any future [WITH-CUDA*][]s which
  turns off cuda initialization entirely.")

(defun use-cuda-p (&rest mats)
  "Return true if cuda is enabled (*CUDA-ENABLED*), it's initialized
  and all MATS have [CUDA-ENABLED][(accessor mat)]. Operations of
  matrices use this to decide whether to go for the CUDA
  implementation or BLAS/Lisp. It's provided for implementing new
  operations."
  (declare (optimize speed)
           (dynamic-extent mats))
  (and *cuda-enabled* (boundp '*cuda-context*)
       (every #'cuda-enabled mats)))

;;; This is effectively a constant across all cuda cards.
(defvar *cuda-warp-size* 32)

;;; FIXME: This should be bound by WITH-CUDA* to the actual value.
(defvar *cuda-n-streaming-multiprocessors* 14)

;;; A higher value means more thread start overhead, a low value means
;;; possible underutilization. Usually a multiple of
;;; *CUDA-N-STREAMING-MULTIPROCESSORS*.
(defvar *cuda-max-n-blocks* (* 8 *cuda-n-streaming-multiprocessors*))

(defun choose-1d-block-and-grid (n max-n-warps-per-block)
  "Return two values, one suitable as the :BLOCK-DIM, the other as
  the :GRID-DIM argument for a cuda kernel call where both are
  one-dimensional (only the first element may be different from 1).

  The number of threads in a block is a multiple of *CUDA-WARP-SIZE*.
  The number of blocks is between 1 and and *CUDA-MAX-N-BLOCKS*. This
  means that the kernel must be able handle any number of elements in
  each thread. For example, a strided kernel that adds a constant to
  each element of a length N vector looks like this:

  ```
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref x i) (+ (aref x i) alpha))))
  ```

  It is often the most efficient to have MAX-N-WARPS-PER-BLOCK around
  4. Note that the maximum number of threads per block is limited by
  hardware (512 for compute capability < 2.0, 1024 for later
  versions), so *CUDA-MAX-N-BLOCKS* times MAX-N-WARPS-PER-BLOCK must
  not exceed that limit."
  (let* ((n-warps (ceiling n *cuda-warp-size*))
         (n-warps-per-block (clip n-warps :min 1 :max max-n-warps-per-block))
         (n-threads-per-block (* *cuda-warp-size* n-warps-per-block))
         (n-blocks (clip (floor n-warps n-warps-per-block)
                         :min 1 :max *cuda-max-n-blocks*)))
    (values (list n-threads-per-block 1 1)
            (list n-blocks 1 1))))

(defun choose-2d-block-and-grid (dimensions max-n-warps-per-block)
  "Return two values, one suitable as the :BLOCK-DIM, the other as
  the :GRID-DIM argument for a cuda kernel call where both are
  two-dimensional (only the first two elements may be different from
  1).

  The number of threads in a block is a multiple of *CUDA-WARP-SIZE*.
  The number of blocks is between 1 and and *CUDA-MAX-N-BLOCKS*.
  Currently - but this may change - the BLOCK-DIM-X is always
  *CUDA-WARP-SIZE* and GRID-DIM-X is always 1.

  This means that the kernel must be able handle any number of
  elements in each thread. For example, a strided kernel that adds a
  constant to each element of a HEIGHT*WIDTH matrix looks like this:

  ```
  (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
        (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
        (stride-x (* block-dim-x grid-dim-x))
        (stride-y (* block-dim-y grid-dim-y)))
    (do ((row id-y (+ row stride-y)))
        ((>= row height))
      (let ((i (* row width)))
        (do ((column id-x (+ column stride-x)))
            ((>= column width))
          (set (aref x i) (+ (aref x i) alpha))
          (incf i stride-x)))))
  ```"
  (destructuring-bind (height width) dimensions
    (let* ((n-warps (ceiling (* height (round-up width *cuda-warp-size*))
                             *cuda-warp-size*))
           (n-warps-per-block (clip n-warps :min 1 :max max-n-warps-per-block))
           (n-blocks (clip (floor n-warps n-warps-per-block)
                           :min 1 :max *cuda-max-n-blocks*)))
      (values (list *cuda-warp-size* n-warps-per-block 1)
              (list 1 n-blocks 1)))))

(defun choose-3d-block-and-grid (dimensions max-n-warps-per-block)
  "Return two values, one suitable as the :BLOCK-DIM, the other as
  the :GRID-DIM argument for a cuda kernel call where both are
  two-dimensional (only the first two elements may be different from
  1).

  The number of threads in a block is a multiple of *CUDA-WARP-SIZE*.
  The number of blocks is between 1 and and *CUDA-MAX-N-BLOCKS*.
  Currently - but this may change - the BLOCK-DIM-X is always
  *CUDA-WARP-SIZE* and GRID-DIM-X is always 1.

  This means that the kernel must be able handle any number of
  elements in each thread. For example, a strided kernel that adds a
  constant to each element of a THICKNESS * HEIGHT * WIDTH 3d array
  looks like this:

  ```
  (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
        (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
        (id-z (+ (* block-dim-z block-idx-z) thread-idx-z))
        (stride-x (* block-dim-x grid-dim-x))
        (stride-y (* block-dim-y grid-dim-y))
        (stride-z (* block-dim-z grid-dim-z)))
    (do ((plane id-z (+ plane stride-z)))
        ((>= plane thickness))
      (do ((row id-y (+ row stride-y)))
          ((>= row height))
        (let ((i (* (+ (* plane height) row)
                    width)))
          (do ((column id-x (+ column stride-x)))
              ((>= column width))
            (set (aref x i) (+ (aref x i) alpha))
            (incf i stride-x))))))
  ```"
  (destructuring-bind (thickness height width) dimensions
    (let* ((n-warps (ceiling (* thickness height
                                (round-up width *cuda-warp-size*))
                             *cuda-warp-size*))
           (n-warps-per-block (clip n-warps :min 1 :max max-n-warps-per-block))
           (n-blocks (clip (floor n-warps n-warps-per-block)
                           :min 1 :max *cuda-max-n-blocks*)))
      (values (list *cuda-warp-size* n-warps-per-block 1)
              (list 1 n-blocks 1)))))


(defsection @mat-cuda-memory-management (:title "CUDA Memory Management")
  "The GPU (called _device_ in CUDA terminology) has its own memory
  and it can only perform computation on data in this _device memory_
  so there is some copying involved to and from main memory. Efficient
  algorithms often allocate device memory up front and minimize the
  amount of copying that has to be done by computing as much as
  possible on the GPU.

  \\MGL-MAT reduces the cost of device of memory allocations by
  maintaining a cache of currently unused allocations from which it
  first tries to satisfy allocation requests. The total size of all
  the allocated device memory regions (be they in use or currently
  unused but cached) is never more than N-POOL-BYTES as specified in
  WITH-CUDA*. N-POOL-BYTES being NIL means no limit."
  (cuda-out-of-memory condition)
  (cuda-room function)
  "That's it about reducing the cost allocations. The other important
  performance consideration, minimizing the amount copying done, is
  very hard to do if the data doesn't fit in device memory which is
  often a very limited resource. In this case the next best thing is
  to do the copying concurrently with computation."
  (with-syncing-cuda-facets macro)
  (*syncing-cuda-facets-safe-p* variable)
  "Also note that often the easiest thing to do is to prevent the use
  of CUDA (and consequently the creation of [CUDA-ARRAY][facet-name]
  facets, and allocations). This can be done either by binding
  *CUDA-ENABLED* to NIL or by setting CUDA-ENABLED to NIL on specific
  matrices.")

(defmacro with-syncing-cuda-facets ((mats-to-cuda mats-to-cuda-host &key
                                     (safep '*syncing-cuda-facets-safe-p*))
                                    &body body)
  "Update CUDA facets in a possibly asynchronous way while BODY
  executes. Behind the scenes, a separate CUDA stream is used to copy
  between registered host memory and device memory. When
  WITH-SYNCING-CUDA-FACETS finishes either by returning normally or by
  a performing a non-local-exit the following are true:

  - All `MAT`s in MATS-TO-CUDA have an up-to-date
    [CUDA-ARRAY][facet-name] facet.

  - All `MAT`s in MATS-TO-CUDA-HOST have an up-to-date
    [CUDA-HOST-ARRAY][facet-name] facet and no
    [CUDA-ARRAY][facet-name].

  It is an error if the same matrix appears in both MATS-TO-CUDA and
  MATS-TO-CUDA-HOST, but the same matrix may appear any number of
  times in one of them.

  If SAFEP is true, then the all matrices in either of the two lists
  are effectively locked for output until WITH-SYNCING-CUDA-FACETS
  finishes. With SAFE NIL, unsafe accesses to facets of these matrices
  are not detected, but the whole operation has a bit less overhead."
  (alexandria:once-only (safep)
    (alexandria:with-gensyms (token)
      `(flet ((foo ()
                ,@body))
         (if (use-cuda-p)
             (let ((,token (start-syncing-cuda-facets
                            ,mats-to-cuda ,mats-to-cuda-host ,safep)))
               (unwind-protect
                    (foo)
                 (finish-syncing-cuda-facets ,token ,safep)))
             (foo))))))

(defvar *syncing-cuda-facets-safe-p* t
  "The default value of the SAFEP argument of
  WITH-SYNCING-CUDA-FACETS.")


;;;; Implementation of WITH-SYNCING-CUDA-FACETS

(defvar *cuda-copy-stream*)

(defclass sync-token ()
  ((ensures :initarg :ensures :reader ensures)
   (destroys :initarg :destroys :reader destroys)))

(defun fake-writer (facet fake-thread)
  (assert (eq (mgl-cube:facet-direction facet) :input))
  (setf (mgl-cube:facet-direction facet) :output)
  (incf (mgl-cube:facet-n-watchers facet))
  (push fake-thread (mgl-cube:facet-watcher-threads facet)))

(defun remove-fake-writer (facet fake-thread)
  (assert (eq (mgl-cube:facet-direction facet) :output))
  (setf (mgl-cube:facet-direction facet) :input)
  (decf (mgl-cube:facet-n-watchers facet))
  (assert (equal fake-thread (pop (mgl-cube:facet-watcher-threads facet)))))

(defun start-syncing-cuda-facets (mats-to-cuda mats-to-cuda-host safep)
  (when (or mats-to-cuda mats-to-cuda-host)
    ;; Wait for any computation on our normal stream to finish so that
    ;; we copy the latest.
    (cl-cuda.driver-api:cu-stream-synchronize *cuda-stream*)
    (let* ((*foreign-array-strategy* :cuda-host)
           (*cuda-stream* *cuda-copy-stream*)
           (ensures-seen (make-hash-table))
           (destroys-seen (make-hash-table))
           (fake-thread (when safep
                          (cons 'with-syncing-cuda-facets
                                (bordeaux-threads:current-thread)))))
      (loop while (or mats-to-cuda mats-to-cuda-host)
            do (when mats-to-cuda
                 (let ((mat (pop mats-to-cuda)))
                   (assert (not (gethash mat destroys-seen)))
                   (unless (gethash mat ensures-seen)
                     (with-facet (a (mat 'cuda-array :direction :input))
                       (declare (ignore a))
                       ;; Add a watcher inside WITH-FACET so that
                       ;; there is no race with other threads.
                       (when safep
                         (fake-writer (find-facet mat 'cuda-array)
                                      fake-thread)))
                     (setf (gethash mat ensures-seen) t))))
               (when mats-to-cuda-host
                 (let ((mat (pop mats-to-cuda-host)))
                   (assert (not (gethash mat ensures-seen)))
                   (unless (gethash mat destroys-seen)
                     (with-facet (a (mat 'cuda-host-array :direction :input))
                       (declare (ignore a))
                       (when safep
                         (fake-writer (find-facet mat 'cuda-host-array)
                                      fake-thread)))
                     (setf (gethash mat destroys-seen) t)))))
      (make-instance 'sync-token :ensures ensures-seen
                     :destroys destroys-seen))))

(defun finish-syncing-cuda-facets (sync-token safep)
  "Wait until all the copying started by START-SYNCING-CUDA-FACETS is
  done, then remove the CUDA-ARRAY facets of the CUDA-ARRAY facets
  from all matrices in KILLS that was passed to
  START-SYNCING-CUDA-FACETS."
  (when sync-token
    (cl-cuda.driver-api:cu-stream-synchronize *cuda-copy-stream*)
    (let ((fake-thread (when safep
                         (cons 'with-syncing-cuda-facets
                               (bordeaux-threads:current-thread)))))
      (when safep
        (maphash (lambda (mat value)
                   (declare (ignore value))
                   (let ((facet (find-facet mat 'cuda-array)))
                     (remove-fake-writer facet fake-thread)
                     (assert (facet-up-to-date-p* mat 'cuda-array facet))))
                 (ensures sync-token)))
      (maphash (lambda (mat value)
                 (declare (ignore value))
                 (when safep
                   (let ((facet (find-facet mat 'cuda-host-array)))
                     (remove-fake-writer facet fake-thread)
                     (assert (facet-up-to-date-p* mat 'cuda-host-array facet))))
                 (destroy-facet mat 'cuda-array))
               (destroys sync-token)))))


;;;; Memory allocation on the GPU
;;;;
;;;; In a nutshell, all allocations must be performed within
;;;; WITH-CUDA-POOL with ALLOC-CUDA-VECTOR. In return, FREE-CUDA-VECTOR
;;;; can be legally called from all threads which is a big no-no with
;;;; cuMemFree. This allows finalizers to work although the freeing is
;;;; deferred until the next call to ALLOC-CUDA-VECTOR.
;;;;
;;;; Similarly, all host memory must be registered within
;;;; WITH-CUDA-POOL with REGISTER-CUDA-HOST-ARRAY (instead of
;;;; CU-MEM-HOST-REGISTER) and in return
;;;; UNREGISTER-AND-FREE-CUDA-HOST-ARRAY can be called from any
;;;; thread.
;;;;
;;;; This is all internal except for CUDA-OUT-OF-MEMORY condition.

(defvar *cuda-pool* nil)

(defmacro with-cuda-pool-locked ((pool) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lock ,pool))
     ,@body))

(defmacro with-cuda-pool ((&key n-bytes) &body body)
  `(progn
     (assert (null *cuda-pool*) () "WITH-CUDA-POOL cannot be nested.")
     (let ((*cuda-pool*
             (make-instance 'cuda-pool
                            :n-bytes-free (or ,n-bytes most-positive-fixnum))))
       (unwind-protect (locally ,@body)
         ;; The WITH-FACET-BARRIER in CALL-WITH-CUDA destroyed the
         ;; CUDA-VECTOR facets of live VEC objects. But if
         ;; finalization has started for a garbage VEC then we must
         ;; wait for it to finish, hence the loop.
         (loop until (with-cuda-pool-locked (*cuda-pool*)
                       (and (zerop (n-bytes-host-array-registered *cuda-pool*))
                            (zerop (n-bytes-allocated *cuda-pool*))))
               do (process-pool *cuda-pool*)
                  (sleep 0.01))
         ;; free all
         (free-some-reusables *cuda-pool* (n-bytes-reusable *cuda-pool*))
         (with-cuda-pool-locked (*cuda-pool*)
           (assert (zerop (n-bytes-allocated *cuda-pool*)))
           (assert (zerop (n-bytes-reusable *cuda-pool*)))
           (assert (zerop (n-bytes-host-array-registered *cuda-pool*))))))))

(defun alloc-cuda-vector (n-bytes)
  (assert *cuda-pool* () "No cuda memory pool. Use WITH-CUDA*.")
  (process-pool *cuda-pool*)
  (let ((pointer (alloc-cuda-vector-with-recovery
                  *cuda-pool* n-bytes (list #'free-some-reusables
                                            #'try-to-free-cuda-memory-1
                                            #'try-to-free-cuda-memory-2
                                            #'try-to-free-cuda-memory-3
                                            #'try-to-free-cuda-memory-3
                                            #'try-to-free-cuda-memory-3))))
    (make-instance 'cuda-vector :base-pointer pointer :n-bytes n-bytes)))

(defun free-cuda-vector (cuda-vector)
  (let ((base-pointer (base-pointer cuda-vector)))
    (assert base-pointer () "Double free detected on cuda array.")
    (setf (slot-value cuda-vector 'base-pointer) nil)
    (return-to-pool (cuda-pool cuda-vector) base-pointer
                    (pointer-n-bytes cuda-vector))))

(defun register-cuda-host-array (pointer n-bytes)
  (assert *cuda-pool* () "No cuda memory pool. Use WITH-CUDA*.")
  (process-pool *cuda-pool*)
  (with-cuda-pool-locked (*cuda-pool*)
    (cl-cuda.driver-api:cu-mem-host-register pointer n-bytes 0)
    (incf (n-bytes-host-array-registered *cuda-pool*) n-bytes))
  (make-instance 'cuda-host-array :base-pointer pointer :n-bytes n-bytes))

(defun unregister-cuda-host-array (cuda-host-array callback)
  (assert (cuda-pool cuda-host-array) () "Double unregister detected.")
  (cond ((eq (cuda-pool cuda-host-array) *cuda-pool*)
         (let ((base-pointer (base-pointer cuda-host-array)))
           (assert base-pointer () "Can't unregister freed array.")
           (unregister-cuda-host-array-now cuda-host-array callback)))
        (t
         (add-host-array-to-be-unregistered (cuda-pool cuda-host-array)
                                            cuda-host-array callback))))

(defun unregister-cuda-host-array-now (cuda-host-array callback)
  (assert (eq *cuda-pool* (cuda-pool cuda-host-array)))
  (cl-cuda.driver-api:cu-mem-host-unregister (base-pointer cuda-host-array))
  (with-cuda-pool-locked (*cuda-pool*)
    (decf (n-bytes-host-array-registered *cuda-pool*)
          (pointer-n-bytes cuda-host-array))
    (assert (not (minusp (n-bytes-host-array-registered *cuda-pool*)))))
  (setf (cuda-pool cuda-host-array) nil)
  (when callback
    (funcall callback)))

(define-condition cuda-out-of-memory (storage-condition)
  ((n-bytes :initarg :n-bytes :reader n-bytes))
  (:report (lambda (condition stream)
             (format stream "Could not allocate ~S bytes on the cuda device."
                     (n-bytes condition))))
  (:documentation "If an allocation request cannot be
  satisfied (either because of N-POOL-BYTES or physical device memory
  limits being reached), then CUDA-OUT-OF-MEMORY is signalled."))

;;;; Implementation of CUDA pool

(defclass cuda-pool ()
  ((n-bytes-allocated :initform 0 :accessor n-bytes-allocated)
   (n-bytes-reusable :initform 0 :accessor n-bytes-reusable)
   (n-bytes-free :initarg :n-bytes-free :accessor n-bytes-free)
   (reusables :initform (make-hash-table) :accessor reusables)
   (lock :initform (bordeaux-threads:make-recursive-lock) :reader lock)
   (host-arrays-to-be-unregistered
    :initform ()
    :accessor host-arrays-to-be-unregistered)
   (n-bytes-host-array-registered
    :initform 0
    :accessor n-bytes-host-array-registered)))

(defun process-pool (cuda-pool)
  (maybe-unregister-pointers cuda-pool))

(defun maybe-unregister-pointers (cuda-pool)
  (when (host-arrays-to-be-unregistered cuda-pool)
    (loop
      (let ((host-arrays-to-be-unregistered
              (host-arrays-to-be-unregistered cuda-pool)))
        (when (mgl-cube::compare-and-swap
               (slot-value cuda-pool 'host-arrays-to-be-unregistered)
               host-arrays-to-be-unregistered ())
          (dolist (cuda-host-array-and-callback host-arrays-to-be-unregistered)
            (destructuring-bind (cuda-host-array . callback)
                cuda-host-array-and-callback
              (unregister-cuda-host-array-now cuda-host-array callback)))
          (return))))))

(defun add-host-array-to-be-unregistered (cuda-pool cuda-host-array callback)
  (loop
    (let* ((old (host-arrays-to-be-unregistered cuda-pool))
           (new (cons (cons cuda-host-array callback) old)))
      (when (mgl-cube::compare-and-swap
             (slot-value cuda-pool 'host-arrays-to-be-unregistered) old new)
        (return)))))

(defclass cuda-vector (offset-pointer)
  ((cuda-pool :initform *cuda-pool* :reader cuda-pool)))

(defclass cuda-array (offset-pointer)
  ())

(defclass cuda-host-array (offset-pointer)
  ((cuda-pool :initform *cuda-pool* :accessor cuda-pool)))

(defun try-to-free-cuda-memory-1 (pool n-bytes)
  (declare (ignore n-bytes))
  (assert (eq pool *cuda-pool*))
  ;; Force finalizations.
  (tg:gc)
  (process-pool pool))

(defun try-to-free-cuda-memory-2 (pool n-bytes)
  (declare (ignore n-bytes))
  (assert (eq pool *cuda-pool*))
  ;; Force finalizations with a global gc.
  (tg:gc :full t)
  (process-pool pool))

(defun try-to-free-cuda-memory-3 (pool n-bytes)
  (declare (ignore n-bytes))
  (assert (eq pool *cuda-pool*))
  ;; FIXME: Wait for finalizers to run. No guarantee that they
  ;; actually run. Even less guarantee that other pools free their
  ;; memory.
  (sleep 1)
  (process-pool pool))

(defun alloc-cuda-vector-with-recovery (pool n-bytes recovery-fns)
  (let ((remaining-recovery-fns recovery-fns))
    (loop
      (catch 'again
        (flet ((handle-it (condition)
                 (declare (ignore condition))
                 (cond (remaining-recovery-fns
                        (funcall (pop remaining-recovery-fns) pool n-bytes))
                       (t
                        (restart-case
                            (error 'cuda-out-of-memory :n-bytes n-bytes)
                          (retry ()
                            :report "Retry the allocation."))
                        (setq remaining-recovery-fns recovery-fns)))
                 (throw 'again nil)))
          (handler-bind ((cuda-out-of-memory #'handle-it)
                         (error (lambda (e)
                                  (when (search "CUDA_ERROR_OUT_OF_MEMORY"
                                                (princ-to-string e))
                                    (handle-it e)))))
            (return
              (or
               ;; try to find an allocation of exactly N-BYTES that
               ;; was returned to the pool
               (reallocate-from-pool pool n-bytes)
               ;; try to grow the pool
               (allocate-to-pool pool n-bytes)
               (error 'cuda-out-of-memory :n-bytes n-bytes)))))))))

;;; free -> allocated
(defun allocate-to-pool (pool n-bytes)
  (with-cuda-pool-locked (pool)
    (when (<= n-bytes (n-bytes-free pool))
      (let ((pointer (cffi:with-foreign-object
                         (device-ptr-ptr 'cl-cuda.driver-api:cu-device-ptr)
                       (cl-cuda.driver-api:cu-mem-alloc device-ptr-ptr n-bytes)
                       (cffi:mem-ref device-ptr-ptr
                                     'cl-cuda.driver-api:cu-device-ptr))))
        (decf (n-bytes-free pool) n-bytes)
        (incf (n-bytes-allocated pool) n-bytes)
        pointer))))

;;; allocated -> reusable
(defun return-to-pool (pool pointer n-bytes)
  (with-cuda-pool-locked (pool)
    (push pointer (gethash n-bytes (reusables pool)))
    (decf (n-bytes-allocated pool) n-bytes)
    (incf (n-bytes-reusable pool) n-bytes)))

;;; reusable -> allocated
(defun reallocate-from-pool (pool n-bytes)
  (with-cuda-pool-locked (pool)
    (when (<= n-bytes (n-bytes-reusable pool))
      (let ((reusable (pop (gethash n-bytes (reusables pool)))))
        (when reusable
          (decf (n-bytes-reusable pool) n-bytes)
          (incf (n-bytes-allocated pool) n-bytes)
          reusable)))))

;;; reusable -> free
(defun free-some-reusables (pool n-bytes-to-free)
  (with-cuda-pool-locked (pool)
    (let ((n-bytes-freed 0)
          (reusables (reusables pool)))
      (maphash (lambda (n-bytes pointers)
                 (let ((left (- n-bytes-to-free n-bytes-freed)))
                   (unless (plusp left)
                     (return-from free-some-reusables n-bytes-freed))
                   (let ((n (length pointers))
                         (n-required (ceiling left n-bytes)))
                     (cond ((<= n n-required)
                            (remhash n-bytes reusables)
                            (incf n-bytes-freed (* n n-bytes)))
                           (t
                            (setf (gethash n-bytes reusables)
                                  (nthcdr n-required pointers))
                            (incf n-bytes-freed (* n-required n-bytes)))))))
               (reusables pool))
      (decf (n-bytes-reusable pool) n-bytes-freed)
      (incf (n-bytes-free pool) n-bytes-freed)
      n-bytes-freed)))
