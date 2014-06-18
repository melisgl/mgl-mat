;;;; CUBLAS support.
;;;;
;;;; Similarly to DEFINE-BLAS-FUNCTION in blas.lisp, the main thing
;;;; here is DEFINE-CUBLAS-FUNCTION. In addition to that, there is
;;;; just a tiny bit of code for initialization of cublas handles and
;;;; error handling.
;;;;
;;;; All CUBLAS functions take a HANDLE keyword argument that defaults
;;;; to *CUBLAS-HANDLE*. WITH-CUBLAS-HANDLE binds *CUBLAS-HANDLE*, all
;;;; calls to CUBLAS functions must be done from within
;;;; WITH-CUBLAS-HANDLE.

(in-package :mgl-mat)

(defsection @mat-cublas (:title "CUBLAS")
  "WITH-CUDA should take of everything. No need to use these at all
  unless you have a very good reason to bypass it."
  (cublas-error condition)
  (cublas-error-function-name (reader cublas-error))
  (cublas-error-status (reader cublas-error))
  (*cublas-handle* variable)
  (cublas-create function)
  (cublas-destroy function)
  (with-cublas-handle macro)
  (cublas-get-version function))

(cffi:define-foreign-library libcublas
  (t (:default "libcublas")))

;;; Let loading succeed even if the foreign library is not available.
(ignore-errors
 (cffi:use-foreign-library libcublas))

(defmacro define-auxiliary-cublas-function ((cname name) &body args)
  (let ((%name (intern (format nil "%~A" (symbol-name name))))
        (arg-names (mapcar #'first args)))
    `(progn
       (cffi:defcfun (,cname ,%name) cublas-status
         (handle cublas-handle)
         ,@args)
       (defun ,name (,@arg-names &key (handle *cublas-handle*))
         (let ((status (,%name handle ,@arg-names)))
           (unless (eq status :cublas-status-success)
             (error 'cublas-error :function-name ,cname :status status)))))))


;;;; Conditions

(cffi:defctype cublas-handle :pointer)

(define-condition cublas-error (error)
  ((function-name :initarg :function-name :reader cublas-error-function-name)
   (status :initarg :status :reader cublas-error-status))
  (:report (lambda (condition stream)
             (format stream "The call to ~A failed with ~A."
                     (cublas-error-function-name condition)
                     (cublas-error-status condition)))))


;;;; Init

(cffi:defcfun ("cublasCreate_v2" cublas-create) cublas-status
  (handle (:pointer cublas-handle)))

(defvar *cublas-handle*)

(define-auxiliary-cublas-function ("cublasDestroy_v2" cublas-destroy))

(defmacro with-cublas-handle (() &body body)
  `(cffi:with-foreign-object (handle-pointer 'cublas-handle)
     (assert (eq :cublas-status-success (cublas-create handle-pointer)))
     (let ((*cublas-handle* (cffi:mem-aref handle-pointer 'cublas-handle 0)))
       (unwind-protect
            (progn ,@body)
         (cublas-destroy)))))

(define-auxiliary-cublas-function ("cublasGetVersion_v2" cublas-get-version)
  (version (:pointer :int)))


;;;; DEFINE-CUBLAS-FUNCTION and supporting cast. All of this is
;;;; internal.

(defun cublas-function-name (name ctype)
  (let ((*package* (find-package :mgl-mat)))
    (read-from-string (format nil "cublas-~A~A" (ctype-blas-prefix ctype)
                              name))))

(defmacro cublas-function-name* (name ctype)
  `(ecase ,ctype
     ,@(loop for ctype in *supported-ctypes*
             collect `((,ctype)
                       ',(cublas-function-name name ctype)))))

(defmacro call-cublas-function (name (&rest params) handle)
  (let* ((*mat-param-type* 'cl-cuda::cu-device-ptr)
         (mat-params (remove-if-not #'mat-param-p params)))
    (alexandria:with-unique-names (ctype)
      `(let ((,ctype (common-mat-ctype ,@(mapcar #'param-name mat-params))))
         (funcall (cublas-function-name* ,name ,ctype)
                  ,@(mapcar (lambda (param)
                              (let ((name (param-name param)))
                                (if (equal (param-type param)
                                           '(:pointer :float))
                                    `(coerce-to-ctype ,name :ctype ,ctype)
                                    name)))
                            params)
                  :handle ,handle)))))

(defun cublas-foreign-function-name (name ctype)
  (format nil "cublas~A~A_v2" (string-upcase (ctype-blas-prefix ctype))
          (string-downcase (symbol-name name))))

(defun cublas-funcall-form (name ctype params args)
  (let ((cname (cublas-foreign-function-name name ctype)))
    `(let ((status (cffi:foreign-funcall
                    ,cname
                    cublas-handle handle
                    ,@(loop for param in params
                            for arg in args
                            append (list (convert-param-types
                                          (param-type param)
                                          ctype)
                                         arg))
                    cublas-status)))
       (unless (eq status :cublas-status-success)
         (error 'cublas-error :function-name ,cname :status status)))))

(defun cublas-call-form* (params args fn)
  (if (endp params)
      (funcall fn (reverse args))
      (let* ((param (first params))
             (name (param-name param))
             (ctype (param-type param))
             (direction (param-direction param)))
        (if (mat-param-p param)
            (let ((arg (gensym (symbol-name name))))
              `(with-facets ((,arg (,name 'cuda-array :direction ,direction)))
                 (let ((,arg (offset-pointer ,arg)))
                   ,(cublas-call-form* (rest params) (cons arg args) fn))))
            (if (and (listp ctype)
                     (eq (first ctype) :pointer))
                (let ((pointer-ctype (second ctype))
                      (arg (gensym (symbol-name name))))
                  `(cffi:with-foreign-object (,arg ,pointer-ctype)
                     ,@(when (member direction '(:input :io))
                         `((setf (cffi:mem-ref ,arg ,pointer-ctype) ,name)))
                     ,(cublas-call-form* (rest params) (cons arg args) fn)
                     ,@(when (member direction '(:io :output))
                         `((cffi:mem-ref ,arg ,pointer-ctype)))))
                (cublas-call-form* (rest params) (cons name args) fn))))))

(defun cublas-call-form (cublas-name ctype params)
  (cublas-call-form* params ()
                     (lambda (args)
                       (cublas-funcall-form cublas-name ctype
                                            params args))))

(defmacro define-cublas-function ((name &key (ctypes '(:float :double)))
                                  (&rest params))
  (let* ((*mat-param-type* 'cl-cuda::cu-device-ptr)
         (in-params (remove-if #'non-mat-output-param-p params))
         (lisp-parameters
           (append (mapcar #'param-name in-params)
                   '(&key (handle *cublas-handle*)))))
    `(progn
       ,@(loop for ctype in ctypes
               collect `(defun ,(cublas-function-name name ctype)
                          ,lisp-parameters
                          ,(let ((params (convert-param-types params ctype)))
                             (cublas-call-form name ctype params))))
       (defun ,(cublas-function-name name nil) (,@lisp-parameters)
         (call-cublas-function ,name ,in-params handle)))))
