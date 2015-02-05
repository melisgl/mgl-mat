(in-package :mgl-mat)

(defsection @mat-cuda (:title "CUDA")
  (cuda-available-p function)
  (with-cuda* macro)
  (call-with-cuda function)
  (*cuda-enabled* variable)
  (cuda-enabled (accessor mat))
  (use-cuda-p function)
  (*default-mat-cuda-enabled* variable)
  (*n-memcpy-host-to-device* variable)
  (*n-memcpy-device-to-host* variable)
  (choose-1d-block-and-grid function)
  (choose-2d-block-and-grid function)
  (choose-3d-block-and-grid function)
  (cuda-out-of-memory condition)
  (*cuda-default-device-id* variable)
  (*cuda-default-random-seed* variable)
  (*cuda-default-n-random-states* variable)
  (@mat-cublas section)
  (@mat-curand section)
  (@mat-cuda-memory-management section))

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
  ""
  (cuda-room function))

(defmacro with-syncing-cuda-facets ((ensures destroys) &body body)
  (alexandria:with-gensyms (token)
    `(flet ((foo ()
              ,@body))
       (if (use-cuda-p)
           (let ((,token (start-syncing-cuda-facets ,ensures ,destroys)))
             (unwind-protect
                  (foo)
               (finish-syncing-cuda-facets ,token)))
           (foo)))))

(defvar *cuda-copy-stream*)

(defclass sync-token ()
  ((ensures :initarg :ensures :reader ensures)
   (destroys :initarg :destroys :reader destroys)))

(defvar *check-async-copy-p* t)

(defun fake-writer (view)
  (assert (eq (mgl-cube::view-direction view) :input))
  (setf (mgl-cube::view-direction view) :output)
  (incf (mgl-cube::view-n-watchers view))
  (push :async (mgl-cube::view-watcher-threads view)))

(defun remove-fake-writer (view)
  (assert (eq (mgl-cube::view-direction view) :output))
  (setf (mgl-cube::view-direction view) :input)
  (decf (mgl-cube::view-n-watchers view))
  (assert (eq :async (pop (mgl-cube::view-watcher-threads view)))))

(defun start-syncing-cuda-facets (ensures destroys)
  "Ensure that all matrices in ENSURES have a CUDA-ARRAY facet and
  start copying data to them to ensure that they are up-to-date. Also,
  ensure that matrices in DESTROYS have up-to-date facets other than
  CUDA-ARRAY so that FINISH-SYNCING-CUDA-FACETS can remove these
  facets. Returns an opaque object to be passed to
  FINISH-SYNCING-CUDA-FACETS. Note that the matrices in ENSURES and
  KILLS must not be accessed before FINISH-SYNCING-CUDA-FACETS
  returns.

  Copying is performed in a separate CUDA stream, so that it can
  overlap with computation."
  (when (or ensures destroys)
    (cl-cuda.driver-api:cu-stream-synchronize *cuda-stream*)
    (let ((*foreign-array-strategy* :cuda-host)
          (cl-cuda:*cuda-stream* *cuda-copy-stream*)
          (ensures-seen (make-hash-table))
          (destroys-seen (make-hash-table))
          (checkp *check-async-copy-p*))
      (loop while (or ensures destroys)
            do (when ensures
                 (let ((mat (pop ensures)))
                   (assert (not (gethash mat destroys-seen)))
                   (unless (gethash mat ensures-seen)
                     (with-facet (a (mat 'cuda-array :direction :input))
                       (declare (ignore a)))
                     (when checkp
                       (fake-writer (find-view mat 'cuda-array)))
                     (setf (gethash mat ensures-seen) t))))
               (when destroys
                 (let ((mat (pop destroys)))
                   (assert (not (gethash mat ensures-seen)))
                   (unless (gethash mat destroys-seen)
                     (with-facet (a (mat 'cuda-host-array :direction :input))
                       (declare (ignore a)))
                     (when checkp
                       (fake-writer (find-view mat 'cuda-host-array)))
                     (setf (gethash mat destroys-seen) t)))))
      (make-instance 'sync-token :ensures ensures-seen :destroys destroys-seen))))

(defun finish-syncing-cuda-facets (sync-token)
  "Wait until all the copying started by START-SYNCING-CUDA-FACETS is
  done, then remove the CUDA-ARRAY facets of the CUDA-ARRAY facets
  from all matrices in KILLS that was passed to
  START-SYNCING-CUDA-FACETS."
  (when sync-token
    (cl-cuda.driver-api:cu-stream-synchronize *cuda-copy-stream*)
    (let ((checkp *check-async-copy-p*))
      (when checkp
        (maphash (lambda (mat value)
                   (declare (ignore value))
                   (remove-fake-writer (find-view mat 'cuda-array))
                   (assert (up-to-date-p* mat 'cuda-array
                                          (find-view mat 'cuda-array))))
                 (ensures sync-token)))
      (maphash (lambda (mat value)
                 (declare (ignore value))
                 (when checkp
                   (remove-fake-writer (find-view mat 'cuda-host-array))
                   (assert (up-to-date-p* mat 'cuda-host-array
                                          (find-view mat 'cuda-host-array))))
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
     (assert (null *cuda-pool*))
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
                     (n-bytes condition)))))

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

;;; reusable -> allocated
(defun reallocate-from-pool (pool n-bytes)
  (with-cuda-pool-locked (pool)
    (when (<= n-bytes (n-bytes-reusable pool))
      (let ((reusable (pop (gethash n-bytes (reusables pool)))))
        (when reusable
          (decf (n-bytes-reusable pool) n-bytes)
          (incf (n-bytes-allocated pool) n-bytes)
          reusable)))))

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

;;; allocated -> reusable
(defun return-to-pool (pool pointer n-bytes)
  (with-cuda-pool-locked (pool)
    (push pointer (gethash n-bytes (reusables pool)))
    (decf (n-bytes-allocated pool) n-bytes)
    (incf (n-bytes-reusable pool) n-bytes)))
