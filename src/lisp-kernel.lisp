;;;; DEFINE-LISP-KERNEL and supporting utilities

(in-package :mgl-mat)

(defun lisp-kernel-name (name ctype)
  (if ctype
      (append-to-symbol name (format nil "-~A" (ctype-blas-prefix ctype)))
      name))

(defun convert-to-lisp-code (object ctype)
  (cond ((eq object 'single-float)
         (ecase ctype
           ((:float) 'single-float)
           ((:double) 'double-float)))
        ((typep object 'single-float)
         (ecase ctype
           ((:float) object)
           ((:double) (float object 0d0))))
        ((eq object 'most-negative-single-float)
         (ecase ctype
           ((:float) object)
           ((:double) 'most-negative-double-float)))
        ((eq object 'most-positive-single-float)
         (ecase ctype
           ((:float) object)
           ((:double) 'most-positive-double-float)))
        (t
         object)))

(defun lisp-code (form ctype)
  (map-tree (lambda (object)
              (convert-to-lisp-code object ctype))
            form))

(defun params-to-type-declarations (params)
  (loop for param in params
        for type = (param-type param)
        unless (eq type t)
          collect `(type ,type ,(param-name param))))

(defparameter *default-lisp-kernel-declarations*
  '((optimize speed #.*no-array-bounds-check*))
  "These declarations are added automatically to kernel functions.")

(defmacro call-lisp-kernel (name (&rest params) ctypes)
  (let* ((mat-params (remove-if-not #'mat-param-p params))
         (facet-vars (facet-vars mat-params)))
    `(let ((ctype (common-mat-ctype ,@(mapcar #'param-name mat-params))))
       (with-facets (,@(loop for param in mat-params
                             for facet-var in facet-vars
                             collect `(,facet-var
                                       (,(param-name param) 'backing-array
                                        :direction ,(param-direction param)))))
         (funcall (ecase ctype
                    ,@(loop for ctype in ctypes
                            collect `((,ctype)
                                      ',(lisp-kernel-name name ctype))))
                  ,@(loop for param in params
                          collect (if (mat-param-p param)
                                      (pop facet-vars)
                                      (param-name param))))))))

(defmacro define-lisp-kernel ((name &key (ctypes '(:float :double)))
                              (&rest params) &body body)
  "This is an extended CL-CUDA:DEFKERNEL macro. It knows how to deal
  with MAT objects and can define the same function for multiple
  CTYPES. Example:

  ```commonlisp
  (define-lisp-kernel (lisp-.+!)
      ((alpha single-float) (x :mat :input) (start-x index) (n index))
    (loop for xi of-type index upfrom start-x
            below (the! index (+ start-x n))
          do (incf (aref x xi) alpha)))
  ```

  Parameters are either of the form `(<NAME> <LISP-TYPE)`
  or `(<NAME> :MAT <DIRECTION>)`. In the latter case, the appropriate
  CFFI pointer is passed to the kernel. `<DIRECTION>` is passed on to
  the WITH-FACET that's used to acquire the foreign array. Note that
  the return type is not declared.

  Both the signature and the body are written as if for single floats,
  but one function is defined for each ctype in CTYPES by transforming
  types, constants and code by substituting them with their ctype
  equivalents. Currently this only means that one needs to write only
  one kernel for SINGLE-FLOAT and DOUBLE-FLOAT. All such functions get
  the declaration from *DEFAULT-LISP-KERNEL-DECLARATIONS*.

  Finally, a dispatcher function with NAME is defined which determines
  the ctype of the MAT objects passed for :MAT typed parameters. It's
  an error if they are not of the same type. Scalars declared
  SINGLE-FLOAT are coerced to that type and the appropriate kernel is
  called."
  (let* ((*mat-param-type* '(simple-array single-float (*)))
         (lisp-parameters (mapcar #'param-name params))
         (declarations (append *default-lisp-kernel-declarations*
                               (params-to-type-declarations params))))
    `(progn
       ,@(loop for ctype in ctypes
               collect `(defun ,(lisp-kernel-name name ctype) ,lisp-parameters
                          (declare ,@(lisp-code declarations ctype))
                          ,@(lisp-code body ctype)))
       (defun ,(lisp-kernel-name name nil) (,@lisp-parameters)
         (call-lisp-kernel ,name ,params ,ctypes)))))
