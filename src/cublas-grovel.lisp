#+darwin (include "cuda/cublas_v2.h")
#-darwin (include "cublas_v2.h")

(in-package :mgl-mat)

(cenum cublas-status
       ((:cublas-status-success "CUBLAS_STATUS_SUCCESS"))
       ((:cublas-status-not-initialized "CUBLAS_STATUS_NOT_INITIALIZED"))
       ((:cublas-status-alloc-failed "CUBLAS_STATUS_ALLOC_FAILED"))
       ((:cublas-status-invalid-value "CUBLAS_STATUS_INVALID_VALUE"))
       ((:cublas-status-arch-mismatch "CUBLAS_STATUS_ARCH_MISMATCH"))
       ((:cublas-status-mapping-error "CUBLAS_STATUS_MAPPING_ERROR"))
       ((:cublas-status-execution-failed "CUBLAS_STATUS_EXECUTION_FAILED"))
       ((:cublas-status-internal-error "CUBLAS_STATUS_INTERNAL_ERROR")))

(cenum cublas-operation
       ((:cublas-op-n "CUBLAS_OP_N"))
       ((:cublas-op-t "CUBLAS_OP_T"))
       ((:cublas-op-c "CUBLAS_OP_C")))
