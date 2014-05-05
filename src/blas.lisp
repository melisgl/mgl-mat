;;;; DEFINE-BLAS-FUNCTION and supporting cast. All of this is
;;;; internal.

(in-package :mgl-mat)

(defun blas-function-name (name ctype)
    (let ((*package* (find-package :mgl-mat)))
      (read-from-string (format nil "blas-~a~a" (ctype-blas-prefix ctype)
                                name))))

(defmacro call-blas-function (name (&rest params))
  (let* ((*mat-param-type* '(:pointer :float))
         (mat-params (remove-if-not #'mat-param-p params)))
    (alexandria:with-unique-names (ctype)
      `(let ((,ctype (common-mat-ctype ,@(mapcar #'param-name mat-params))))
         (funcall (blas-function-name ',name ,ctype)
                  ,@(mapcar (lambda (param)
                              (let ((name (param-name param)))
                                (if (and (not (mat-param-p param))
                                         (equal (param-type param)
                                                '(:pointer :float)))
                                    `(coerce-to-ctype ,name :ctype ,ctype)
                                    name)))
                            params))))))

(defun blas-foreign-function-name (name ctype)
  (format nil "~A~A_" (ctype-blas-prefix ctype)
          (string-downcase (symbol-name name))))

(defun blas-funcall-form (name ctype params return-type args)
  (let ((cname (blas-foreign-function-name name ctype)))
    `(cffi:foreign-funcall
      ,cname
      ,@(loop for param in params
              for arg in args
              append (list (convert-param-types
                            (param-type param)
                            ctype)
                           arg))
      ,(convert-param-types return-type ctype))))

(defun blas-call-form* (params args fn)
  (if (endp params)
      (funcall fn (reverse args))
      (let* ((param (first params))
             (name (param-name param))
             (ctype (param-type param))
             (direction (param-direction param)))
        (if (mat-param-p param)
            (let ((arg (gensym (symbol-name name))))
              `(with-facets ((,arg (,name 'foreign-array
                                          :direction ,direction)))
                 (let ((,arg (offset-pointer ,arg)))
                   ,(blas-call-form* (rest params) (cons arg args) fn))))
            (if (and (listp ctype)
                     (eq (first ctype) :pointer))
                (let ((pointer-ctype (second ctype))
                      (arg (gensym (symbol-name name))))
                  `(cffi:with-foreign-object (,arg ,pointer-ctype)
                     ,@(when (member direction '(:input :io))
                         `((setf (cffi:mem-ref ,arg ,pointer-ctype) ,name)))
                     ,(blas-call-form* (rest params) (cons arg args) fn)
                     ,@(when (member direction '(:io :output))
                         `((cffi:mem-ref ,arg ,pointer-ctype)))))
                (blas-call-form* (rest params) (cons name args) fn))))))

(defun blas-call-form (blas-name ctype params return-type)
  (blas-call-form* params ()
                   (lambda (args)
                     (blas-funcall-form blas-name ctype
                                        params return-type args))))

(defmacro define-blas-function ((name &key (ctypes '(:float :double)))
                                (return-type (&rest params)))
  (let* ((*mat-param-type* '(:pointer :float))
         (params (mapcar #'ensure-pointer-param params))
         (in-params (remove-if #'non-mat-output-param-p params))
         (lisp-parameters (mapcar #'param-name in-params)))
    `(progn
       ,@(loop for ctype in ctypes
               collect `(defun ,(blas-function-name name ctype)
                          ,lisp-parameters
                          ,(let ((params (convert-param-types params ctype)))
                             (blas-call-form name ctype params return-type))))
       (defun ,(blas-function-name name nil) (,@lisp-parameters)
         (call-blas-function ,name ,in-params)))))
