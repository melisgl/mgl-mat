;;;; -*- mode: Lisp -*-

;;; The CUDA SDK may not be installed, we need CUDA-GROVEL-FILE from
;;; CL-CUDA to grovel safely.
(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cl-cuda))

(asdf:defsystem #:mgl-mat
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "MAT is library for working with multi-dimensional
  arrays which supports efficient interfacing to foreign and CUDA
  code. BLAS and CUBLAS bindings are available."
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-mat-test")))
  :depends-on (#:alexandria #:bordeaux-threads #:cffi #:cffi-grovel #:cl-cuda
                            #:ieee-floats #:lla #:mgl-pax #:static-vectors
                            #:trivial-garbage)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "cube")
                             (:file "util")
                             (:file "blas")
                             (:file "blas-functions")
                             (cl-cuda-asd::cuda-grovel-file "cublas-grovel")
                             (:file "cublas")
                             (:file "cublas-functions")
                             (:file "foreign")
                             (:file "cuda-early")
                             (:file "cuda-kernel")
                             (:file "lisp-kernel")
                             (:file "curand")
                             (:file "cuda-late")
                             (:file "mat")
                             (:file "convolve")
                             (:file "max-pool")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:mgl-mat))))
  (asdf:oos 'asdf:load-op '#:mgl-mat-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:mgl-mat))))
