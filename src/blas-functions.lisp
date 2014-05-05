(in-package :mgl-mat)

;;;; Level 1

(define-blas-function (asum)
    (:float ((n :int) (x :mat :input) (incx :int))))

(define-blas-function (axpy)
    (:void ((n :int) (alpha :float) (x :mat :input) (incx :int)
            (y :mat :io) (incy :int))))

(define-blas-function (copy)
    (:void ((n :int) (x :mat :input) (incx :int) (y :mat :output) (incy :int))))

(define-blas-function (dot)
    (:float ((n :int) (x :mat :input) (incx :int)
             (y :mat :input) (incy :int))))

(define-blas-function (nrm2)
    (:float ((n :int) (x :mat :input) (incx :int))))

(define-blas-function (scal)
    (:void ((n :int) (alpha :float) (x :mat :io) (incx :int))))



;;;; Level 2



;;;; Level 3

(define-blas-function (gemm)
    (:void ((transpose-a :char) (transpose-b :char) (n :int) (m :int) (k :int)
            (alpha :float) (a :mat :input) (lda :int)
            (b :mat :input) (ldb :int)
            (beta :float) (c :mat :io) (ldc :int))))
