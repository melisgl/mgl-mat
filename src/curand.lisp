(in-package :mgl-mat)

(defsection @mat-curand (:title "CURAND")
  "This the low level CURAND API."
  (with-curand-state macro)
  (*curand-state* variable)
  (curand-xorwow-state class)
  (n-states (reader curand-xorwow-state))
  (states (reader curand-xorwow-state)))

(defvar *curand-state*)

(defgeneric destroy-random-state (state))

(defmacro with-curand-state ((state) &body body)
  `(let ((*curand-state* ,state))
     (unwind-protect
          (locally ,@body)
       (destroy-random-state *curand-state*))))

(defgeneric curand-uniform (state mat))


;;;; XORWOW

(defclass curand-xorwow-state ()
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
         (states (alloc-cuda-array
                  (* n-states
                     (cffi:foreign-type-size
                      '(:struct curand-state-xorwow))))))
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

(defkernel copy-xorwow-states
    (void ((from curand-state-xorwow*) (n int) (to curand-state-xorwow*)))
  (let ((i (+ (* block-dim-x block-idx-x) thread-idx-x)))
    (when (< i n)
      (set (aref to i) (aref from i)))))

(defmethod copy-random-state ((state curand-xorwow-state))
  (let* ((n-states (n-states state))
         (states (alloc-cuda-array
                  (* n-states
                     (cffi:foreign-type-size
                      '(:struct curand-state-xorwow))))))
    (copy-xorwow-states (offset-pointer (states state)) n-states
                        (offset-pointer states)
                        :grid-dim (list (ceiling n-states 256) 1 1)
                        :block-dim (list 256 1 1))
    (make-instance 'curand-xorwow-state
                   :n-states n-states
                   :states states)))

(defmethod destroy-random-state ((state curand-xorwow-state))
  (free-cuda-array (states state))
  (setf (slot-value state 'states) nil))
