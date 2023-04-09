;;;; -*- mode: Lisp -*-

;;; The CUDA SDK may not be installed, we need CUDA-GROVEL-FILE from
;;; CL-CUDA to grovel safely.
(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cl-cuda))

;;; This should really be in CFFI-GROVEL.
(defmethod asdf:perform :around ((op cffi-grovel::process-op)
                                 (file cffi-grovel::cc-flags-mixin))
  (declare (ignorable op))
  (let ((cffi-grovel::*cc-flags*
          (append (cffi-grovel::ensure-list (cffi-grovel::cc-flags-of file))
                  cffi-grovel::*cc-flags*)))
    (call-next-method)))

(asdf:defsystem #:mgl-mat
  :licence "MIT, see COPYING."
  :version "0.1.0"
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-mat"
  :bug-tracker "https://github.com/melisgl/mgl-mat/issues"
  :source-control (:git "https://github.com/melisgl/mgl-mat.git")
  :description "MAT is library for working with multi-dimensional
  arrays which supports efficient interfacing to foreign and CUDA
  code. BLAS and CUBLAS bindings are available."
  :depends-on (#:alexandria #:bordeaux-threads #:cffi #:cffi-grovel #:cl-cuda
                            #:cl-num-utils #:flexi-streams #:ieee-floats #:lla
                            #:mgl-pax #:static-vectors #:trivial-garbage)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "cube")
                             (:file "util")
                             (:file "blas")
                             (:file "blas-functions")
                             ;; When everyone has the new cl-cuda,
                             ;; this should be just
                             ;; CFFI-GROVEL:GROVEL-FILE. It is
                             ;; redundant with the :IF-FEATURE below.
                             (cl-cuda-asd::cuda-grovel-file
                              "cublas-grovel"
                              ;; Work around cublas_v2.h not found on
                              ;; OS X issue.
                              ;;
                              ;; https://github.com/melisgl/mgl-mat/issues/1
                              :cc-flags ("-I" "/usr/local/cuda/include/")
                              :if-feature :cuda-sdk)
                             (:file "cublas")
                             (:file "cublas-functions")
                             (:file "foreign")
                             (:file "cuda-early")
                             (:file "cuda-kernel")
                             (:file "lisp-kernel")
                             (:file "curand")
                             (:file "cuda-late")
                             (:file "vec")
                             (:file "mat")
                             (:file "convolve")
                             (:file "max-pool")
                             (:file "doc"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-mat/test"))))

(asdf:defsystem mgl-mat/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :description "Test system for MGL-MAT."
  :depends-on (#:mgl-mat #:cl-fad)
  :components ((:module "test"
                :serial t
                :components ((:file "test-mat"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-mat '#:test)))
