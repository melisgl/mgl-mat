(in-package :mgl-mat)

(defsection @mat-cuda (:title "CUDA")
  (cuda-available-p function)
  (with-cuda* macro)
  (call-with-cuda function)
  (*cuda-enabled* variable)
  (use-cuda-p function)
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

(defun use-cuda-p ()
  "Return true if cuda is enabled (*CUDA-ENABLED*) and it's
initialized. MAT operations use this to decide whether to go for the
cuda implementation or BLAS/Lisp. It's provided for implementing new
operations."
  (and *cuda-enabled* *cuda-context*))

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

  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref x i) (+ (aref x i) alpha))))

It is often the most efficient to have MAX-N-WARPS-PER-BLOCK is around
4. Note that the maximum number of threads per block is limited by
hardware (512 for compute capability < 2.0, 1024 for later versions),
so *CUDA-MAX-N-BLOCKS* times MAX-N-WARPS-PER-BLOCK must not exceed
that limit."
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
two-dimensional (only the first two elements may be different from 1).

The number of threads in a block is a multiple of *CUDA-WARP-SIZE*.
The number of blocks is between 1 and and *CUDA-MAX-N-BLOCKS*.
Currently - but this may change - the BLOCK-DIM-X is always
*CUDA-WARP-SIZE* and GRID-DIM-X is always 1.

This means that the kernel must be able handle any number of elements
in each thread. For example, a strided kernel that adds a constant to
each element of a HEIGHT*WIDTH matrix looks like this:

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
          (incf i stride-x)))))"
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
two-dimensional (only the first two elements may be different from 1).

The number of threads in a block is a multiple of *CUDA-WARP-SIZE*.
The number of blocks is between 1 and and *CUDA-MAX-N-BLOCKS*.
Currently - but this may change - the BLOCK-DIM-X is always
*CUDA-WARP-SIZE* and GRID-DIM-X is always 1.

This means that the kernel must be able handle any number of elements
in each thread. For example, a strided kernel that adds a constant to
each element of a THICKNESS*HEIGHT*WIDTH 3d array looks like this:

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
            (incf i stride-x))))))"
  (destructuring-bind (thickness height width) dimensions
    (let* ((n-warps (ceiling (* thickness height (round-up width *cuda-warp-size*))
                             *cuda-warp-size*))
           (n-warps-per-block (clip n-warps :min 1 :max max-n-warps-per-block))
           (n-blocks (clip (floor n-warps n-warps-per-block)
                           :min 1 :max *cuda-max-n-blocks*)))
      (values (list *cuda-warp-size* n-warps-per-block 1)
              (list 1 n-blocks 1)))))


;;;; Memory allocation on the GPU
;;;;
;;;; In a nutshell, all allocations must be within a WITH-CUDA-POOL
;;;; with ALLOC-CUDA-ARRAY. In return, FREE-CUDA-ARRAY can be legally
;;;; called from all threads which is a big no-no with cuMemFree. This
;;;; allows finalizers to work although the freeing is deferred until
;;;; the next call to ALLOC-CUDA-ARRAY.
;;;;
;;;; This is all internal except for CUDA-OUT-OF-MEMORY condition.

(defvar *cuda-pool* nil)

(defclass cuda-pool ()
  ((arrays-to-be-freed :initform () :accessor arrays-to-be-freed)))

(defun maybe-free-pointers (cuda-pool)
  (when (arrays-to-be-freed cuda-pool)
    (loop
      (let ((arrays-to-be-freed (arrays-to-be-freed cuda-pool)))
        (when (mgl-cube::compare-and-swap
               (slot-value cuda-pool 'arrays-to-be-freed) arrays-to-be-freed ())
          (dolist (cuda-array arrays-to-be-freed)
            (free-cuda-array cuda-array))
          (return))))))

(defun add-array-to-be-freed (cuda-pool cuda-array)
  (loop
    (let* ((old (arrays-to-be-freed cuda-pool))
           (new (cons cuda-array old)))
      (when (mgl-cube::compare-and-swap
             (slot-value cuda-pool 'arrays-to-be-freed) old new)
        (return)))))

(defmacro with-cuda-pool (() &body body)
  `(progn
     (assert (null *cuda-pool*))
     (let ((*cuda-pool* (make-instance 'cuda-pool)))
       (unwind-protect (locally ,@body)
         (maybe-free-pointers *cuda-pool*)))))

(defclass cuda-array (offset-pointer)
  ((pool :initarg :pool :reader cuda-array-pool)))

(defun try-to-free-cuda-memory-1 ()
  ;; Force finalizations.
  (tg:gc)
  (maybe-free-pointers *cuda-pool*))

(defun try-to-free-cuda-memory-2 ()
  ;; Force finalizations with a global gc.
  (tg:gc :full t)
  (maybe-free-pointers *cuda-pool*))

(defun try-to-free-cuda-memory-3 ()
  ;; FIXME: Wait for finalizers to run. No guarantee that they
  ;; actually run. Even less guarantee that other pools free their
  ;; memory.
  (sleep 3)
  (maybe-free-pointers *cuda-pool*))

(define-condition cuda-out-of-memory (storage-condition)
  ((n-bytes :initarg :n-bytes :reader n-bytes))
  (:report (lambda (condition stream)
             (format stream "Could not allocate ~S bytes on the cuda device."
                     (n-bytes condition)))))

(defun alloc-cuda-array-with-recovery (device-ptr-ptr n-bytes recovery-fns)
  (let ((remaining-recovery-fns recovery-fns))
    (loop
      (catch 'again
        (handler-bind
            ((error
               (lambda (e)
                 (when (search "CUDA_ERROR_OUT_OF_MEMORY" (princ-to-string e))
                   (cond (remaining-recovery-fns
                          (funcall (pop remaining-recovery-fns)))
                         (t
                          (restart-case
                              (error 'cuda-out-of-memory :n-bytes n-bytes)
                            (retry ()
                              :report "Retry the allocation."))
                          (setq remaining-recovery-fns recovery-fns)))
                   (throw 'again nil)))))
          (cl-cuda.driver-api:cu-mem-alloc device-ptr-ptr n-bytes)
          (return))))))

(defun alloc-cuda-array (n-bytes)
  (assert *cuda-pool* () "No cuda memory pool. Use WITH-CUDA*.")
  (maybe-free-pointers *cuda-pool*)
  (cffi:with-foreign-object (device-ptr-ptr 'cl-cuda.driver-api:cu-device-ptr)
    (alloc-cuda-array-with-recovery device-ptr-ptr n-bytes
                                    (list #'try-to-free-cuda-memory-1
                                          #'try-to-free-cuda-memory-2
                                          #'try-to-free-cuda-memory-3))
    (let* ((pointer (cffi:mem-ref device-ptr-ptr
                                  'cl-cuda.driver-api:cu-device-ptr))
           (cuda-array (make-instance 'cuda-array :base-pointer pointer
                                      :pool *cuda-pool*)))
      cuda-array)))

(defun free-cuda-array (cuda-array)
  (when (base-pointer cuda-array)
    (cond ((eq (cuda-array-pool cuda-array) *cuda-pool*)
           (let ((base-pointer (base-pointer cuda-array)))
             (assert base-pointer () "Double free detected on cuda array.")
             (setf (slot-value cuda-array 'base-pointer) nil)
             (cl-cuda.driver-api:cu-mem-free base-pointer)))
          (t
           (add-array-to-be-freed (cuda-array-pool cuda-array) cuda-array)))
    t))
