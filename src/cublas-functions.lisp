(in-package :mgl-mat)

;;;; Level 1

(define-cublas-function (asum)
  ((n :int) (x :mat :input) (incx :int) (y-ptr (:pointer :float) :output)))

(define-cublas-function (axpy)
  ((n :int) (alpha (:pointer :float) :input)
   (x :mat :input) (incx :int) (y :mat :io) (incy :int)))

(define-cublas-function (copy)
  ((n :int) (x :mat :input) (incx :int) (y :mat :input) (incy :int)))

(define-cublas-function (dot)
  ((n :int) (x :mat :input) (incx :int) (y :mat :input) (incy :int)
   (z-ptr (:pointer :float) :output)))

(define-cublas-function (nrm2)
  ((n :int) (x :mat :input) (incx :int) (y-ptr (:pointer :float) :output)))

(define-cublas-function (scal)
  ((n :int) (alpha (:pointer :float) :input) (x :mat :io) (incx :int)))


;;;; Level 2



;;;; Level 3

(define-cublas-function (gemm)
  ((transa cublas-operation)
   (transb cublas-operation)
   (m :int) (n :int) (k :int)
   (alpha (:pointer :float))
   (a :mat :input)
   (lda :int)
   (b :mat :input)
   (ldb :int)
   (beta (:pointer :float))
   (c :mat :io)
   (ldc :int)))
