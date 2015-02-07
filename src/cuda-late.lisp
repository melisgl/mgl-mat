(in-package :mgl-mat)

(defvar *cuda-default-device-id* 0
  "The default value of WITH-CUDA*'s :DEVICE-ID argument.")

(defvar *cuda-default-random-seed* 1234
  "The default value of WITH-CUDA*'s :RANDOM-SEED argument.")

(defvar *cuda-default-n-random-states* 4096
  "The default value of WITH-CUDA*'s :N-RANDOM-STATES argument.")

(defvar *n-memcpy-host-to-device* 0
  "Incremented each time a host to device copy is performed. Bound to
  0 by WITH-CUDA*. Useful for tracking down performance problems.")

(defvar *n-memcpy-device-to-host* 0
  "Incremented each time a device to host copy is performed. Bound to
  0 by WITH-CUDA*. Useful for tracking down performance problems.")

(defun cuda-room (&key (stream *standard-output*) (verbose t))
  "When CUDA is in use (see USE-CUDA-P), print a summary of memory
  usage in the current CUDA context to STREAM. If VERBOSE, make the
  output human easily readable, else try to present it in a very
  concise way. Sample output with VERBOSE:

  ```
  CUDA memory usage:
  device arrays: 450 (used bytes: 3,386,295,808, pooled bytes: 1,816,657,920)
  host arrays: 14640 (used bytes: 17,380,147,200)
  host->device copies: 154,102,488, device->host copies: 117,136,434
  ```

  The same data presented with VERBOSE false:

  ```
  d: 450 (3,386,295,808 + 1,816,657,920), h: 14640 (17,380,147,200)
  h->d: 154,102,488, d->h: 117,136,434
  ```"
  (when (use-cuda-p)
    (if verbose
        (format stream
                "CUDA memory usage:~%~
                device arrays: ~S (used bytes: ~:D, pooled bytes: ~:D)~%~
                host arrays: ~S (used bytes: ~:D)~%~
                host->device copies: ~:D, device->host copies: ~:D~%"
                (count-barred-facets 'cuda-array :type 'mat)
                (n-bytes-allocated *cuda-pool*)
                (n-bytes-reusable *cuda-pool*)
                (count-barred-facets 'cuda-host-array :type 'mat)
                (n-bytes-host-array-registered *cuda-pool*)
                *n-memcpy-host-to-device*
                *n-memcpy-device-to-host*)
        (format stream "d: ~S (~:D + ~:D), h: ~S (~:D)~%~
                       h->d: ~:D, d->h: ~:D~%"
                (count-barred-facets 'cuda-array :type 'mat)
                (n-bytes-allocated *cuda-pool*)
                (n-bytes-reusable *cuda-pool*)
                (count-barred-facets 'cuda-host-array :type 'mat)
                (n-bytes-host-array-registered *cuda-pool*)
                *n-memcpy-host-to-device*
                *n-memcpy-device-to-host*))))

(defun remove-arch-nvcc-option (options)
  (remove-if (lambda (option)
               (zerop (search "-arch=" option)))
             options))

(defmacro with-cuda-stream ((stream) &body body)
  (alexandria:with-gensyms (stream-pointer)
    `(cffi:with-foreign-objects
         ((,stream-pointer 'cl-cuda.driver-api:cu-stream))
       (cl-cuda.driver-api:cu-stream-create ,stream-pointer 0)
       (let ((,stream (cffi:mem-ref ,stream-pointer
                                    'cl-cuda.driver-api:cu-stream)))
         (unwind-protect
              (locally ,@body)
           (cl-cuda.driver-api:cu-stream-destroy ,stream))))))

(defun call-with-cuda (fn &key
                       ((:enabled *cuda-enabled*) *cuda-enabled*)
                       (device-id *cuda-default-device-id*)
                       (random-seed *cuda-default-random-seed*)
                       (n-random-states *cuda-default-n-random-states*)
                       (override-arch-p t)
                       n-pool-bytes)
  "Like WITH-CUDA*, but takes a no argument function instead of the
  macro's BODY."
  (cond ((boundp '*cuda-context*)
         ;; FIXME: is this sane semantics?
         (with-facet-barrier ('mat '(array) '(cuda-host-array cuda-array))
           (funcall fn)))
        ((and *cuda-enabled* (cuda-available-p))
         (let ((*show-messages* nil)
               (*n-memcpy-host-to-device* 0)
               (*n-memcpy-device-to-host* 0))
           (with-cuda (device-id)
             (multiple-value-bind (major minor)
                 (cl-cuda:device-compute-capability device-id)
               (let ((cl-cuda:*nvcc-options*
                       (if override-arch-p
                           (cons (format nil "-arch=sm_~D~D" major minor)
                                 (remove-arch-nvcc-option
                                  cl-cuda:*nvcc-options*))
                           cl-cuda:*nvcc-options*)))
                 (with-cuda-pool (:n-bytes n-pool-bytes)
                   (with-cuda-stream (*cuda-stream*)
                     (with-cuda-stream (*cuda-copy-stream*)
                       (with-facet-barrier ('vec '(lisp-vector) '(cuda-vector))
                         (with-facet-barrier ('mat '(array) '(cuda-array
                                                              cuda-host-array))
                           (with-cublas-handle ()
                             (with-curand-state
                                 ((if random-seed
                                      (make-xorwow-state/simple
                                       random-seed n-random-states)
                                      *curand-state*))
                               (funcall fn)))))))))))))
        (t
         (funcall fn))))

(defun cuda-available-p (&key (device-id 0))
  "Check a cuda context is already in initialized in the current
  thread or a device with DEVICE-ID is available."
  (let ((*show-messages* nil))
    (or (boundp '*cuda-context*)
        (not (not (ignore-errors
                   (init-cuda)
                   (get-cuda-device device-id)))))))

(defmacro with-cuda* ((&key (enabled '*cuda-enabled*)
                       (device-id *cuda-default-device-id*)
                       (random-seed *cuda-default-random-seed*)
                       (n-random-states *cuda-default-n-random-states*)
                       (override-arch-p t)
                       n-pool-bytes)
                      &body body)
  "Initializes CUDA with with all bells and whistles before BODY and
  deinitializes it after. Simply wrapping WITH-CUDA* around a piece
  code is enough to make use of the first available CUDA device or
  fall back on blas and lisp kernels if there is none.

  If CUDA is already initialized, then it sets up a facet barrier
  which destroys CUDA-ARRAY and CUDA-HOST-ARRAY facets after ensuring
  that the ARRAY facet is up-to-date.

  Else, if CUDA is available and ENABLED, then in addition to the
  facet barrier, a CUDA context is set up, *N-MEMCPY-HOST-TO-DEVICE*,
  *N-MEMCPY-DEVICE-TO-HOST* are bound to zero, the highest possible
  -arch option for the device is added to *CL-CUDA:NVCC-OPTIONS* (if
  OVERRIDE-ARCH-P), a cublas handle created, and *CURAND-STATE* is
  bound to a CURAND-XORWOW-STATE with N-RANDOM-STATES, seeded with
  RANDOM-SEED, and allocation of device memory is limited to
  N-POOL-BYTES (NIL means no limit, see @MAT-CUDA-MEMORY-MANAGEMENT).

  Else - that is, if CUDA is not available, BODY is simply executed."
  `(call-with-cuda (lambda () ,@body) :enabled ,enabled
                   :device-id ,device-id :random-seed ,random-seed
                   :n-random-states ,n-random-states
                   :override-arch-p ,override-arch-p
                   :n-pool-bytes ,n-pool-bytes))
