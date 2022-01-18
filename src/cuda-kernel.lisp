;;;; DEFINE-CUDA-KERNEL and supporting utilities

(in-package :mgl-mat)

(defun param-to-cuda (param)
    (list (param-name param) (param-type param)))

(defun cuda-kernel-name (name ctype)
  (if ctype
      (append-to-symbol name (format nil "-~A" (ctype-blas-prefix ctype)))
      name))

(defun convert-to-cuda-code (object ctype)
  (cond ((eq object 'float)
         (ecase ctype
           ((:float) 'float)
           ((:double) 'double)))
        ((eq object 'float*)
         (ecase ctype
           ((:float) 'float*)
           ((:double) 'double*)))
        ((eq object 'cl-cuda:curand-uniform-float-xorwow)
         (ecase ctype
           ((:float) 'cl-cuda:curand-uniform-float-xorwow)
           ((:double) 'cl-cuda:curand-uniform-double-xorwow)))
        ((eq object 'cl-cuda:curand-normal-float-xorwow)
         (ecase ctype
           ((:float) 'cl-cuda:curand-normal-float-xorwow)
           ((:double) 'cl-cuda:curand-normal-double-xorwow)))
        ((typep object 'single-float)
         (ecase ctype
           ((:float) object)
           ((:double) (float object 0d0))))
        (t
         object)))

(defun cuda-code (form ctype)
  (map-tree (lambda (object)
              (convert-to-cuda-code object ctype))
            form))

(defmacro call-cuda-kernel (name (&rest params) ctypes &key grid-dim block-dim)
  (let* ((mat-params (remove-if-not #'mat-param-p params))
         (facet-vars (facet-vars mat-params)))
    `(let ((ctype (common-mat-ctype ,@(mapcar #'param-name mat-params))))
       (with-facets (,@(loop for param in mat-params
                             for facet-var in facet-vars
                             collect `(,facet-var
                                       (,(param-name param) 'cuda-array
                                        :direction ,(param-direction param)))))
         (funcall (ecase ctype
                    ,@(loop for ctype in ctypes
                            collect `((,ctype)
                                      ',(cuda-kernel-name name ctype))))
                  ,@(loop for param in params
                          collect (if (mat-param-p param)
                                      `(offset-pointer ,(pop facet-vars))
                                      (param-name param)))
                  :grid-dim ,grid-dim
                  :block-dim ,block-dim)))))

(defmacro define-cuda-kernel ((name &key (ctypes '(:float :double)))
                              (return-type params) &body body)
  "This is an extended CL-CUDA:DEFKERNEL macro. It knows how to deal
  with MAT objects and can define the same function for multiple
  CTYPES. Example:

  ```commonlisp
  (define-cuda-kernel (cuda-.+!)
      (void ((alpha float) (x :mat :input) (n int)))
    (let ((stride (* block-dim-x grid-dim-x)))
      (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
              (+ i stride)))
          ((>= i n))
        (set (aref x i) (+ (aref x i) alpha)))))
  ```

  The signature looks pretty much like in CL-CUDA:DEFKERNEL, but
  parameters can take the form of `(<NAME> :MAT <DIRECTION>)` too, in
  which case the appropriate CL-CUDA.DRIVER-API:CU-DEVICE-PTR is
  passed to the kernel. `<DIRECTION>` is passed on to the WITH-FACET
  that's used to acquire the cuda array.

  Both the signature and the body are written as if for single floats,
  but one function is defined for each ctype in CTYPES by transforming
  types, constants and code by substituting them with their ctype
  equivalents. Currently this means that one needs to write only one
  kernel for [FLOAT][dislocated] and [DOUBLE][dislocated].

  Finally, a dispatcher function with NAME is defined which determines
  the ctype of the MAT objects passed for :MAT typed parameters. It's
  an error if they are not of the same type. Scalars declared
  [FLOAT][dislocated] are coerced to that type and the appropriate
  kernel is called."
  (let* ((*mat-param-type* 'float*)
         (kernel-parameters (mapcar #'param-to-cuda params))
         (lisp-parameters (mapcar #'param-name params)))
    `(progn
       ,@(loop for ctype in ctypes
               collect `(defkernel ,(cuda-kernel-name name ctype)
                            (,(cuda-code return-type ctype)
                             ,(cuda-code kernel-parameters ctype))
                          ,@(cuda-code body ctype)))
       (defun ,name (,@lisp-parameters &key grid-dim block-dim)
         (call-cuda-kernel ,name ,params ,ctypes
                           :grid-dim grid-dim
                           :block-dim block-dim)))))
