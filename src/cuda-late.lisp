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

(defun remove-arch-nvcc-option (options)
  (remove-if (lambda (option)
               (zerop (search "-arch=" option)))
             options))

(defun call-with-cuda (fn &key
                       ((:enabled *cuda-enabled*) *cuda-enabled*)
                       (device-id *cuda-default-device-id*)
                       (random-seed *cuda-default-random-seed*)
                       (n-random-states *cuda-default-n-random-states*)
                       (override-arch-p t))
  "Like WITH-CUDA*, but takes a no argument function instead of the
macro's BODY."
  (cond ((boundp '*cuda-context*)
         (with-facet-barrier ('mat '(array) '(cuda-array))
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
                 (with-cuda-pool ()
                   (with-facet-barrier ('mat '(array) '(cuda-array))
                     (with-cublas-handle ()
                       (with-curand-state ((if random-seed
                                               (make-xorwow-state/simple
                                                random-seed n-random-states)
                                               *curand-state*))
                         (funcall fn))))))))))
        (t
         (funcall fn))))

(defun cuda-available-p (&key (device-id 0))
  "Check a cuda context is already in initialized in the current
  thread or a device with DEVICE-ID is available."
  (let ((*show-messages* nil))
    (or (boundp '*cuda-context*)
        (ignore-errors
         (init-cuda)
         (get-cuda-device device-id)))))

(defmacro with-cuda* ((&key (enabled '*cuda-enabled*)
                       (device-id *cuda-default-device-id*)
                       (random-seed *cuda-default-random-seed*)
                       (n-random-states *cuda-default-n-random-states*)
                       (override-arch-p t))
                      &body body)
  "Initializes cuda with with all bells and whistles before BODY and
  deinitializes it after. Simply wrapping WITH-CUDA* around a piece
  code is enough to make use of the first available cuda device or
  fall back on blas and lisp kernels if there is none.

  If cuda is already initialized, then it sets up a facet barrier
  which destroys CUDA-ARRAY facets after ensuring that the ARRAY facet
  is up-to-date.

  Else, if cuda is available and ENABLED, then in addition to the
  facet barrier, a cuda context is set up, *N-MEMCPY-HOST-TO-DEVICE*,
  *N-MEMCPY-DEVICE-TO-HOST* are bound to zero, the highest possible
  -arch option for the device is added to *CL-CUDA:NVCC-OPTIONS* (if
  OVERRIDE-ARCH-P), a cublas handle created, and *CURAND-STATE* is
  bound to a CURAND-XORWOW-STATE with N-RANDOM-STATES, seeded with
  RANDOM-SEED.

  Else - that is, if cuda not available -, BODY is simply executed."
  `(call-with-cuda (lambda () ,@body) :enabled ,enabled
                   :device-id ,device-id :random-seed ,random-seed
                   :n-random-states ,n-random-states
                   :override-arch-p ,override-arch-p))
