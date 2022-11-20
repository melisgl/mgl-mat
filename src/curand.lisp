(in-package :mgl-mat)

(defsection @mat-curand (:title "CURAND")
  "This the low level CURAND API. You probably want @MAT-RANDOM
  instead."
  (with-curand-state macro)
  (*curand-state* variable)
  (curand-xorwow-state class)
  (n-states (reader curand-xorwow-state))
  (states (reader curand-xorwow-state)))

(defvar *curand-state*)

(defgeneric copy-curand-state (state))
(defgeneric destroy-curand-state (state))

(defmacro with-curand-state ((state) &body body)
  `(let ((*curand-state* ,state))
     (unwind-protect
          (locally ,@body)
       (destroy-curand-state *curand-state*))))

(defgeneric curand-uniform (state mat))

(defgeneric curand-normal (state mat))

(defclass curand-state ()
  ())


;;;; XORWOW

(defclass curand-xorwow-state (curand-state)
  ((n-states :initarg :n-states :reader n-states)
   (states :initarg :states :reader states)))

(defun sanitize-n-random-states (n-states)
  (cond ((< n-states *cuda-warp-size*)
         (warn "Must have at least ~S random states." *cuda-warp-size*)
         *cuda-warp-size*)
        ((zerop (mod n-states *cuda-warp-size*))
         n-states)
        (t
         (warn "Rounding number of random states (~S) to a multiple of warp ~
                size (~S)."
               n-states *cuda-warp-size*)
         (* (ceiling n-states *cuda-warp-size*)
            *cuda-warp-size*))))

(defun make-xorwow-state/simple (seed n-states)
  (let* ((n-states (sanitize-n-random-states n-states))
         (states (alloc-cuda-vector
                  (* n-states
                     (cffi:foreign-type-size
                      '(:struct cl-cuda.lang.data::curand-state-xorwow))))))
    (curand-init-xorwow-simple seed (offset-pointer states) n-states
                               :grid-dim (list (ceiling n-states 256) 1 1)
                               :block-dim (list 256 1 1))
    (make-instance 'curand-xorwow-state
                   :n-states n-states
                   :states states)))

(defkernel curand-init-xorwow-simple
    (void ((seed int) (states curand-state-xorwow*) (n int)))
  (let ((id (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< id n)
      (curand-init-xorwow seed id 0 (pointer (aref states id))))))

(define-cuda-kernel (curand-uniform-xorwow)
    (void ((states curand-state-xorwow*) (x :mat :output) (n int)
           (n-states int)))
  (let ((idx (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< idx n-states)
      (let ((state (aref states idx))
            (stride (* block-dim-x grid-dim-x)))
        (do ((i idx (+ i stride)))
            ((>= i n))
          (set (aref x i) (curand-uniform-float-xorwow (pointer state))))
        (set (aref states idx) state)))))

(defmethod curand-uniform ((state curand-xorwow-state) mat)
  (let ((n (mat-size mat))
        (n-states (n-states state)))
    (multiple-value-bind (block-dim grid-dim)
        (choose-1d-block-and-grid (min n n-states) 4)
      (curand-uniform-xorwow (offset-pointer (states state)) mat n n-states
                             :grid-dim grid-dim :block-dim block-dim))))

(define-cuda-kernel (curand-normal-xorwow)
    (void ((states curand-state-xorwow*) (x :mat :output) (n int)
           (n-states int)))
  (let ((idx (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< idx n-states)
      (let ((state (aref states idx))
            (stride (* block-dim-x grid-dim-x)))
        (do ((i idx (+ i stride)))
            ((>= i n))
          (set (aref x i) (curand-normal-float-xorwow (pointer state))))
        (set (aref states idx) state)))))

(defmethod curand-normal ((state curand-xorwow-state) mat)
  (let ((n (mat-size mat))
        (n-states (n-states state)))
    (multiple-value-bind (block-dim grid-dim)
        (choose-1d-block-and-grid (min n n-states) 4)
      (curand-normal-xorwow (offset-pointer (states state)) mat n n-states
                              :grid-dim grid-dim :block-dim block-dim))))

(defkernel copy-xorwow-states
    (void ((from curand-state-xorwow*) (n int) (to curand-state-xorwow*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n)
      (set (aref to i) (aref from i)))))

(defmethod copy-curand-state ((state curand-xorwow-state))
  (let* ((n-states (n-states state))
         (states (alloc-cuda-vector
                  (* n-states
                     (cffi:foreign-type-size
                      '(:struct cl-cuda.lang.data::curand-state-xorwow))))))
    (copy-xorwow-states (offset-pointer (states state)) n-states
                        (offset-pointer states)
                        :grid-dim (list (ceiling n-states 256) 1 1)
                        :block-dim (list 256 1 1))
    (make-instance 'curand-xorwow-state
                   :n-states n-states
                   :states states)))

(defmethod destroy-curand-state ((state curand-xorwow-state))
  (free-cuda-vector (states state))
  (setf (slot-value state 'states) nil))
