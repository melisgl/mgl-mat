;;;; TODO
;;;;
;;;; - doc: OFFSET-POINTER, BASE-POINTER, CUDA-ARRAY
;;;;
;;;; - doc: separate cuda extension api

(in-package :mgl-mat)

(defsection @mat-manual (:title "MAT manual")
  (mgl-mat asdf:system)
  (@mat-introduction section)
  (@mat-basics section)
  (@mat-ctypes section)
  (@mat-printing section)
  (@mat-shaping section)
  (@mat-caching section)
  (@mat-foreign section)
  (@mat-cuda section)
  (@mat-blas section)
  (@mat-destructive-api section)
  (@mat-non-destructive-api section)
  (@mat-mappings section)
  (@mat-random section)
  (@mat-io section)
  (@mat-extension-api section))

(defsection @mat-introduction (:title "Introduction")
  (@mat-what-is-it section)
  (@mat-what-kind-of-matrices section)
  (@mat-installation section))

(defsection @mat-what-is-it (:title "What's MAT?")
  "MAT is library for working with multi-dimensional arrays which
  supports efficient interfacing to foreign and CUDA code with
  automatic translations between cuda, foreign and lisp storage. BLAS
  and CUBLAS bindings are available.")

(defsection @mat-what-kind-of-matrices
    (:title "What kind of matrices are supported?")
  "Currently only row-major single and double float matrices are
  supported, but it would be easy to add single and double precision
  complex types too. Other numeric types, such as byte and native
  integer, can be added too, but they are not supported by CUBLAS.
  There are no restrictions on the number of dimensions, and reshaping
  is possible. The CUBLAS functions operate on the visible portion of
  the matrix (which is subject to displacement and shaping), invisible
  elements are not affected.")

(defsection @mat-installation (:title "Installation")
  "All dependencies are in quicklisp except for cl-cuda and some of
  its dependencies: cl-pattern and cl-reexport which you need to get
  from github.")

(defsection @mat-basics (:title "Basics")
  "A MAT is a CUBE (see @CUBE-MANUAL) whose facets are different
  representations of numeric arrays. These facets can be accessed with
  WITH-FACETS with one of the following [FACET-NAME][]s:"
  (backing-array facet-name)
  (array facet-name)
  (foreign-array facet-name)
  (cuda-array facet-name)
  (mat class)
  (mat-ctype (reader mat))
  (mat-displacement (reader mat))
  (mat-dimensions (reader mat))
  (mat-dimension function)
  (mat-initial-element (reader mat))
  (mat-size (reader mat))
  (mat-max-size (reader mat))
  (make-mat function)
  (array-to-mat function)
  (mat-to-array function)
  (replace! function)
  (mref function)
  (row-major-mref function))

(export 'with-facet)
(export 'with-facets)

(define-facet-name backing-array ()
  "The corresponding facet is a one dimensional lisp array.")

(define-facet-name array ()
  "Same as BACKING-ARRAY if the matrix is one-dimensional, all
  elements are visible (see @MAT-SHAPING), else it's a lisp array
  displaced to the backing array.")

(define-facet-name foreign-array ()
  "The facet is a [FOREIGN-ARRAY][class] which is an OFFSET-POINTER
  wrapping a CFFI:POINTER. See *FOREIGN-ARRAY-STRATEGY*.")

(define-facet-name cuda-array ()
  "The facet is CUDA-ARRAY which is an OFFSET-POINTER wrapping a
  CL-CUDA.DRIVER-API:CU-DEVICE-PTR, allocated with CU-MEM-ALLOC and
  freed automatically.

  Facets bound by with WITH-FACETS are to be treated as dynamic
  extent: it is not allowed to keep a reference to them beyond the
  dynamic scope of WITH-FACETS.

  For example, to fill matrix X of CTYPE :DOUBLE with ones it most
  convenient to work with the one dimensional BACKING-ARRAY:

  ```
  (let ((displacement (mat-displacement x))
        (size (mat-size x)))
   (with-facets ((x* (x 'backing-array :direction :output)))
     (fill x* 1d0 :start displacement :end (+ displacement size))))
  ```

  DIRECTION is :OUTPUT because we clobber all values in X. Armed with
  this knowledge about the direction, WITH-FACETS will not copy data
  from another facet if the backing array is not up-to-date.

  To transpose a 2d matrix with the ARRAY facet:

  ```
  (destructuring-bind (n-rows n-columns) (mat-dimensions x)
    (with-facets ((x* (x 'array :direction :io)))
      (dotimes (row n-rows)
        (dotimes (column n-columns)
          (setf (aref x* row column) (aref x* column row))))))
  ```

  Note that DIRECTION is :IO, because we need the data in this facet
  to be up-to-date (that's the input part) and we are invalidating all
  other facets by changing values (that's the output part).

  To sum the values of a matrix using the FOREIGN-ARRAY facet:

  ```
  (let ((sum 0))
    (with-facets ((x* (x 'foreign-array :direction :input)))
      (let ((pointer (offset-pointer x*)))
        (loop for index below (mat-size x)
              do (incf sum (cffi:mem-aref pointer (mat-ctype x) index)))))
    sum)
  ```

  See DIRECTION for a complete description of :INPUT, :OUTPUT and :IO.
  For MAT objects, that needs to be refined. If a MAT is reshaped
  and/or displaced in a way that not all elements are visible then
  those elements are always kept intact and copied around. This is
  accomplished by turning :OUTPUT into :IO automatically on such MATs.

  Most operations automatically use CUDA, if available and
  initialized. See WITH-CUDA for detail.")

(defclass mat (cube)
  ((ctype
    :type ctype :initform *default-mat-ctype*
    :initarg :ctype :reader mat-ctype
    :documentation "One of *SUPPORTED-CTYPES*. The matrix can hold
    only values of this type.")
   (displacement
    :initform 0 :initarg :displacement :reader mat-displacement
    :documentation "A value in the [0,MAX-SIZE] interval. This is like
    the DISPLACED-INDEX-OFFSET of a lisp array.")
   (dimensions
    :initarg :dimensions :reader mat-dimensions
    :documentation "Like ARRAY-DIMENSIONS. It holds a list of
    dimensions, but it is allowed to pass in scalars too.")
   (initial-element
    :initform 0 :initarg :initial-element
    :reader mat-initial-element
    :documentation "If non-nil, then when a facet is created, it is
    filled with INITIAL-ELEMENT coerced to the appropriate numeric
    type. If NIL, then no initialization is performed.")
   (size
    :reader mat-size
    :documentation "The number of elements in the visible portion of
    the array. This is always the product of the elements
    MAT-DIMENSIONS and is similar to ARRAY-TOTAL-SIZE.")
   (max-size
    :initarg :max-size :reader mat-max-size
    :documentation "The total size can be larger than MAT-SIZE, but
    cannot change. Also DISPLACEMENT + SIZE must not exceed it. This
    is not")
   ;; The number of bytes SIZE number of elements take.
   (n-bytes :reader mat-n-bytes))
  (:documentation "A MAT is a data CUBE that is much like a lisp
   array, it supports DISPLACEMENT, arbitrary DIMENSIONS and
   INITIAL-ELEMENT with the usual semantics. However, a MAT supports
   different representations of the same data. See @MAT-BASICS for a
   tuturialish treatment."))

(defmethod initialize-instance :after ((mat mat) &key initial-contents
                                       &allow-other-keys)
  (unless (listp (mat-dimensions mat))
    (setf (slot-value mat 'dimensions)
          (list (mat-dimensions mat))))
  (setf (slot-value mat 'size) (reduce #'* (mat-dimensions mat)))
  (unless (slot-boundp mat 'max-size)
    (setf (slot-value mat 'max-size)
          (+ (mat-displacement mat) (mat-size mat))))
  (assert (<= (+ (mat-displacement mat) (mat-size mat)) (mat-max-size mat)))
  (setf (slot-value mat 'n-bytes)
        (* (mat-max-size mat) (cffi:foreign-type-size (mat-ctype mat))))
  (when (mat-initial-element mat)
    (setf (slot-value mat 'initial-element)
          (coerce-to-ctype (mat-initial-element mat)
                           :ctype (mat-ctype mat))))
  (when initial-contents
    (replace! mat initial-contents)))

(defun mat-dimension (mat axis-number)
  "Return the dimension along AXIS-NUMBER. Similar to
  ARRAY-DIMENSION."
  (elt (mat-dimensions mat) axis-number))

(defun make-mat (dimensions &rest args &key (ctype *default-mat-ctype*)
                 (displacement 0) max-size (initial-element 0)
                 initial-contents)
  "Return a new matrix. If INITIAL-CONTENTS is given then the matrix
  contents are copied with REPLACE!. See class MAT for the description
  of the rest of the parameters. This is exactly what (MAKE-INSTANCE
  'MAT ...) does except DIMENSIONS is not a keyword argument so
  MAKE-MAT looks more like MAKE-ARRAY."
  (declare (ignore displacement max-size initial-element initial-contents))
  (apply #'make-instance 'mat :ctype ctype :dimensions dimensions args))

(defun array-to-mat (array &key ctype)
  "Create a MAT that's equivalent to ARRAY. Displacement of the
  created array will be 0 and the size will be equal to
  ARRAY-TOTAL-SIZE."
  (let* ((ctype (or ctype (lisp->ctype (array-element-type array))))
         (mat (make-instance 'mat
                             :ctype ctype
                             :dimensions (array-dimensions array))))
    (with-facet (backing-array (mat 'backing-array :direction :output))
      (loop for i upfrom 0
            for j upfrom 0
            repeat (mat-size mat)
            do (setf (aref backing-array i) (row-major-aref array j))))
    mat))

(defun mat-to-array (mat)
  (with-facet (array (mat 'array :direction :input))
    (alexandria:copy-array array)))

(defun replace! (mat seq-of-seqs)
  "Replace the contents of MAT with the elements of SEQ-OF-SEQS.
  SEQ-OF-SEQS is a nested sequence of sequences similar to the
  INITIAL-CONTENTS argument of MAKE-ARRAY. The total number of
  elements must match the size of MAT. Returns MAT."
  (with-facets ((m (mat 'backing-array :direction :output)))
    (replace-vector m (mat-displacement mat) (mat-dimensions mat)
                    seq-of-seqs (mat-ctype mat)))
  mat)

(defun replace-vector (vector start dimensions seq-of-seqs ctype)
  (let ((i start)
        (n (reduce #'* dimensions)))
    (labels ((foo (dims seq-of-seqs)
               (let ((n-dims (length dims)))
                 (cond ((= 0 n-dims)
                        (setf (aref vector i)
                              (coerce-to-ctype seq-of-seqs :ctype ctype))
                        (incf i))
                       ((= 1 n-dims)
                        (map nil (lambda (element)
                                   (setf (aref vector i)
                                         (coerce-to-ctype element :ctype ctype))
                                   (incf i))
                             seq-of-seqs))
                       (t
                        (map nil (lambda (seq-of-seqs)
                                   (foo (cdr dims) seq-of-seqs))
                             seq-of-seqs))))))
      (foo dimensions seq-of-seqs))
    (unless (= i (+ start n))
      (error "Total size of ~S is not ~S." seq-of-seqs n))))

(defun mref (mat &rest indices)
  "Like AREF for arrays. Don't use this if you care about performance
  at all. SETFable."
  (with-facets ((a (mat 'array :direction :input)))
    (apply #'aref a indices)))

(defun set-mref (value mat &rest indices)
  (with-facets ((a (mat 'array :direction :io)))
    (setf (apply #'aref a indices) value)))

(defsetf mref (mat &rest indices) (value)
  `(set-mref ,value ,mat ,@indices))

(defun row-major-mref (mat index)
  "Like ROW-MAJOR-AREF for arrays. Don't use this if you care about
  performance at all. SETFable."
  (with-facets ((a (mat 'array :direction :input)))
    (row-major-aref a index)))

(defun set-row-major-mref (mat index value)
  (with-facets ((a (mat 'array :direction :io)))
    (setf (row-major-aref a index) value)))

(defsetf row-major-mref set-row-major-mref)


(defsection @mat-printing (:title "Printing")
  (*print-mat* variable)
  (*print-mat-facets* variable))

(defvar *print-mat* t
  "Controls whether the contents of a MAT object are printed as an
  array (subject to the standard printer control variables).")

(defvar *print-mat-facets* t
  "Controls whether a summary of existing and up-to-date views is
  printed whe a MAT object is printed. The summary that looks like
  `ABcf` indicates that all four facets (ARRAY, BACKING-ARRAY,
  CUDA-ARRAY, FOREIGN-ARRAY) are present and the first two are
  up-to-date. A summary of a single #\- indicates that there are no
  facets.")

(defmethod print-object ((mat mat) stream)
  (print-unreadable-object (mat stream :type t :identity (not *print-mat*))
    (print-mat-dimensions mat stream)
    (when *print-mat-facets*
      (write-char #\Space stream)
      (print-mat-facets mat stream))
    (when *print-mat*
      (write-char #\Space stream)
      (let ((*let-input-through-p* t))
        (ignore-errors
         (with-facets ((a (mat 'array :direction :input)))
           (write a :stream stream)))))))

(defun print-mat-dimensions (mat stream)
  (multiple-value-bind (before after)
      (if (< (mat-size mat) (mat-max-size mat))
          (values (mat-displacement mat)
                  (- (mat-max-size mat)
                     (mat-size mat)
                     (mat-displacement mat)))
          (values nil nil))
    (let ((displacedp (or (and before (/= 0 before))
                          (and after (/= 0 after)))))
      (when displacedp
        (format stream "~A+" before))
      (format stream "~{~A~^x~}" (mat-dimensions mat))
      (when displacedp
        (format stream "+~A" after)))))

(defun mat-view-to-char (view)
  (let ((char (aref (symbol-name (view-facet-name view)) 0)))
    (if (view-up-to-date-p view)
        (char-upcase char)
        (char-downcase char))))

(defun print-mat-facets (mat stream)
  (let ((chars (mapcar #'mat-view-to-char (views mat))))
    (if chars
        (format stream "~{~A~}" (sort chars #'char-lessp))
        (format stream "-"))))


(defsection @mat-shaping (:title "Shaping")
  "Reshaping and displacement of MAT objects works somewhat similarly
  to lisp arrays. The key difference is that they are destructive
  operations. See RESHAPE-AND-DISPLACE!, RESHAPE!, DISPLACE!,
  RESHAPE-TO-ROW-MATRIX! and WITH-SHAPE-AND-DISPLACEMENT. ADJUST! is
  the odd one out, it may create a new MAT.

  Existing facets are adjusted by all operations. For LISP-ARRAY
  facets, this means creating a new lisp array displaced to the
  backing array. The backing array stays the same, clients are
  supposed to observe MAT-DISPLACEMENT, MAT-DIMENSIONS or MAT-SIZE.
  The FOREIGN-ARRAY and CUDA-ARRAY facets are OFFSET-POINTER's so
  displacement is done by changing the offset. Clients need to observe
  MAT-DIMENSIONS in any case."
  (reshape-and-displace! function)
  (reshape! function)
  (displace! function)
  (reshape-to-row-matrix! function)
  (with-shape-and-displacement macro)
  (adjust! function))

(defun reshape-and-displace! (mat dimensions displacement)
  "Change the visible (or active) portion of MAT by altering its
  displacement offset and dimensions. Future operations will only
  affect this visible portion as if the rest of the elements were not
  there. Return MAT.

  DISPLACEMENT + the new size must not exceed MAT-MAX-SIZE.
  Furthermore, there must be no facets being viewed (with WITH-FACETS)
  when calling this function as the identity of the facets is not
  stable."
  ;; If an ARRAY facet is being viewed and it's EQ to the
  ;; BACKING-ARRAY (i.e. it's not displaced), then we change the
  ;; identity of the array which is surprising to say the least.
  (check-no-watchers mat "Cannot reshape or displace the matrix")
  (let* ((dimensions (alexandria:ensure-list dimensions))
         (size (reduce #'* dimensions)))
    (assert (<= (+ displacement size) (mat-max-size mat)))
    (when (or (not (equal (mat-dimensions mat) dimensions))
              (not (= (mat-displacement mat) displacement)))
      (setf (slot-value mat 'dimensions) dimensions)
      (setf (slot-value mat 'displacement) displacement)
      (setf (slot-value mat 'size) size)
      (maybe-reshape-and-displace-facet mat 'array
                                        dimensions displacement)
      (maybe-reshape-and-displace-facet mat 'cuda-array
                                        dimensions displacement)
      (maybe-reshape-and-displace-facet mat 'foreign-array
                                        dimensions displacement))
    mat))

(defun maybe-reshape-and-displace-facet (mat facet-name dimensions displacement)
  (let ((view (find-view mat facet-name)))
    (when view
      (reshape-and-displace-facet* mat facet-name (view-facet view)
                                   dimensions displacement))))

(defgeneric reshape-and-displace-facet* (mat facet-name facet
                                         dimensions displacement)
  (:method ((mat mat) facet-name facet dimensions displacement)
    (declare (ignore facet-name facet dimensions displacement))))

(defun reshape! (mat dimensions)
  "Like RESHAPE-AND-DISPLACE! but only alters the dimensions."
  (reshape-and-displace! mat dimensions (mat-displacement mat)))

(defun displace! (mat displacement)
  "Like RESHAPE-AND-DISPLACE! but only alters the displacement."
  (reshape-and-displace! mat (mat-dimensions mat) displacement))

(defun reshape-to-row-matrix! (mat row)
  "Reshape the 2d MAT to make only a single ROW visible. This is made
  possible by the row-major layout, hence no column counterpart."
  (destructuring-bind (n-rows n-columns) (mat-dimensions mat)
    (assert (< row n-rows))
    (reshape-and-displace! mat (list 1 n-columns)
                           (+ (mat-displacement mat)
                              (* row n-columns)))))

(defmacro with-shape-and-displacement-restored ((mat) &body body)
  (alexandria:with-gensyms (saved-dimensions saved-displacement)
    (alexandria:once-only (mat)
      `(let ((,saved-dimensions (mat-dimensions ,mat))
             (,saved-displacement (mat-displacement ,mat)))
         (unwind-protect
              (progn ,@body)
           (reshape-and-displace! ,mat ,saved-dimensions
                                  ,saved-displacement))))))

(defmacro with-shape-and-displacement ((mat &optional
                                        (dimensions nil dimensionsp)
                                        (displacement nil displacementp))
                                       &body body)
  "Reshape and displace MAT if DIMENSIONS and/or DISPLACEMENT is given
  and restore the original shape and displacement after BODY is
  executed. If neither is specificed, then nothing will be changed,
  but BODY is still allowed to alter the shape and displacement."
  `(with-shape-and-displacement-restored (,mat)
     ,(cond ((and dimensionsp displacementp)
             `(reshape-and-displace! ,mat ,dimensions ,displacement))
            (dimensionsp
             `(reshape! ,mat ,dimensions))
            (displacementp
             `(displace! ,mat ,displacement)))
     (locally ,@body)))

(defun adjust! (mat dimensions displacement &key (destroy-old-p t))
  "Like RESHAPE-AND-DISPLACE! but creates a new matrix if MAT isn't
  large enough. If a new matrix is created, the contents are not
  copied over and the old matrix is destroyed with DESTROY-CUBE if
  DESTROY-OLD-P."
  (let* ((dimensions (alexandria:ensure-list dimensions))
         (size (reduce #'* dimensions)))
    (if (<= (+ displacement size) (mat-max-size mat))
        (reshape-and-displace! mat dimensions displacement)
        (prog1
            (make-mat dimensions :displacement displacement
                      :ctype (mat-ctype mat)
                      :initial-element (mat-initial-element mat))
          (when destroy-old-p
            (destroy-cube mat))))))


(defsection @mat-caching (:title "Caching")
  "Allocating and initializing a MAT object and its necessary facets
  can be expensive. The following macros remember the previous value
  of a binding in the same thread and lexical environment. Only weak
  references are constructed so the cached objects can be garbage
  collected.

  While the cache is global, thread safety is guaranteed by having
  separate subcaches per thread. Each subcache is keyed by a gensym
  that's unique to each invocation of the caching macro, so different
  occurrences of caching macros in the source never share data. Still
  recursion could lead to data sharing between different invocations
  of the same function. To prevent this, the cached object is removed
  from the cache while it is used so other invocations will create a
  fresh one which isn't particularly efficient but at least it's
  safe."
  (with-thread-cached-mat macro)
  (with-ones macro))

(defmacro with-thread-cached-mat ((var dimensions &rest args
                                   &key (ctype *default-mat-ctype*)
                                   (displacement 0)
                                   max-size
                                   (initial-element 0) initial-contents)
                                  &body body)
  "Bind VAR to a matrix of DIMENSIONS, CTYPE, etc. Cache this matrix,
  and possibly reuse it later by reshaping it. When BODY exits the
  cached object is updated with the binding of VAR which BODY may
  change."
  (declare (ignore max-size initial-contents))
  (let ((args (copy-list args)))
    (remf args :ctype)
    (remf args :displacement)
    (remf args :initial-element)
    (alexandria:with-unique-names (key)
      (alexandria:once-only (dimensions displacement ctype initial-element)
        `(let ((,key (list ,ctype ,initial-element)))
           (with-thread-cached-object
               (,var ,key (make-mat ,dimensions
                                    :ctype ,ctype
                                    :displacement ,displacement
                                    :initial-element ,initial-element
                                    ,@args))
             (setq ,var (adjust! ,var ,dimensions ,displacement))
             (locally ,@body)))))))

(defmacro with-ones ((var dimensions &key (ctype *default-mat-ctype*))
                     &body body)
  "Bind VAR to a matrix of DIMENSIONS whose every element is 1. The
  matrix is cached for efficiency."
  `(with-thread-cached-mat (,var ,dimensions :ctype ,ctype :initial-element 1)
     ;; Rebind VAR to make sure that WITH-THREAD-CACHED-MAT doesn't
     ;; update the cached object if BODY changes the binding.
     (let ((,var ,var))
       ,@body)))


;;;; Implementing @FACET-EXTENSION-API

(defmethod watch-facet ((mat mat) facet-name direction)
  (call-next-method mat facet-name
                    ;; If the reshaped array doesn't cover the whole
                    ;; index range, then make sure that the `hidden'
                    ;; elements survive even if they have to be copied
                    ;; from another facet.
                    (if (and (eq direction :output)
                             (< (mat-size mat) (mat-max-size mat)))
                        :io
                        direction)))

(defun make-array-facet (mat)
  (with-facet (backing-array (mat 'backing-array :direction :input))
    (if (and (zerop (mat-displacement mat))
             (= 1 (length (mat-dimensions mat))))
        backing-array
        (make-array (mat-dimensions mat)
                    :element-type (ctype->lisp (mat-ctype mat))
                    :displaced-to backing-array
                    :displaced-index-offset (mat-displacement mat)))))

;;; Unfortunately we can't generally tell whether an array is static
;;; so we must store this information in the VIEW-FACET-DESCRIPTION.
(defun static-backing-array-p (mat)
  (let ((backing-array-view (find-view mat 'backing-array)))
    (and backing-array-view
         (eq :static (view-facet-description backing-array-view)))))

;;; This is for debugging only.
(defparameter *n-statics* 0)

(defmethod make-facet* ((mat mat) (facet-name (eql 'backing-array)))
  (if (eq *foreign-array-strategy* :static-backing-array)
      (progn
        (incf *n-statics*)
        (values (if (mat-initial-element mat)
                    (static-vectors:make-static-vector
                     (mat-max-size mat)
                     :element-type (ctype->lisp (mat-ctype mat))
                     :initial-element (mat-initial-element mat))
                    (static-vectors:make-static-vector
                     (mat-max-size mat)
                     :element-type (ctype->lisp (mat-ctype mat))))
                :static
                t))
      (if (mat-initial-element mat)
          (make-array (mat-max-size mat)
                      :element-type (ctype->lisp (mat-ctype mat))
                      :initial-element (mat-initial-element mat))
          (make-array (mat-max-size mat)
                      :element-type (ctype->lisp (mat-ctype mat))))))

(defmethod make-facet* ((mat mat) (facet-name (eql 'array)))
  (make-array-facet mat))

(defmethod make-facet* ((mat mat) (facet-name (eql 'foreign-array)))
  (assert (or (eq *foreign-array-strategy* :dynamic)
              ;; It may be that this backing array was allocating when
              ;; *FOREIGN-ARRAY-STRATEGY* was :PIN-BACKING-ARRAY so it
              ;; is not static.
              (and (eq *foreign-array-strategy* :static-backing-array)
                   (null (view-facet-description
                          (find-view mat 'backing-array))))))
  (let* ((ctype (mat-ctype mat))
         (array (alloc-foreign-array ctype :count (mat-max-size mat)))
         (pointer (base-pointer array))
         (initial-element (mat-initial-element mat)))
    (reshape-and-displace-facet* mat facet-name array (mat-dimensions mat)
                                 (mat-displacement mat))
    (when initial-element
      (dotimes (i (mat-max-size mat))
        (setf (cffi:mem-aref pointer ctype i) initial-element)))
    (values array nil t)))

(defmethod make-facet* ((mat mat) (facet-name (eql 'cuda-array)))
  (let ((array (alloc-cuda-array (* (mat-max-size mat)
                                    (cffi:foreign-type-size (mat-ctype mat))))))
    (when (and (mat-initial-element mat)
               (endp (views mat)))
      (cuda-fill!-2 (mat-ctype mat) (mat-initial-element mat)
                    array (mat-max-size mat)))
    (reshape-and-displace-facet* mat facet-name array (mat-dimensions mat)
                                 (mat-displacement mat))
    (values array nil t)))

(defmethod destroy-facet* ((facet-name (eql 'array)) array description)
  (declare (ignore array description)))

(defmethod destroy-facet* ((facet-name (eql 'backing-array)) array description)
  (when (eq description :static)
    (decf *n-statics*)
    (static-vectors:free-static-vector array)))

(defmethod destroy-facet* (facet-name (array foreign-array) description)
  (declare (ignore facet-name description))
  (free-foreign-array array))

(defmethod destroy-facet* (facet-name (cuda-array cuda-array) description)
  (declare (ignore facet-name description))
  (free-cuda-array cuda-array))

(cffi:defcfun memcpy :void
  (dest :pointer)
  (src :pointer)
  (n cl-cuda.driver-api:size-t))

;;; array -> *
(defmethod copy-facet* ((mat mat) (from-name (eql 'array)) array
                        to-name to-facet)
  (declare (ignore array))
  (unless (eq to-name 'backing-array)
    (copy-facet* mat 'backing-array
                 (view-facet (find-view mat 'backing-array))
                 to-name to-facet)))

;;; * -> array
(defmethod copy-facet* ((mat mat) from-name from-facet
                        (to-name (eql 'array)) array)
  (declare (ignore array))
  (unless (eq from-name 'backing-array)
    (copy-facet* mat from-name from-facet 'backing-array
                 (view-facet (find-view mat 'backing-array)))))

;;; backing-array -> foreign-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'backing-array)) array
                        (to-name (eql 'foreign-array)) foreign-array)
  (cond ((use-pinning-p)
         (lla::with-pinned-array (ptr array)
           (memcpy (base-pointer foreign-array) ptr (mat-n-bytes mat))))
        ((static-backing-array-p mat)
         (memcpy (base-pointer foreign-array)
                 (static-vectors:static-vector-pointer array)
                 (mat-n-bytes mat)))
        (t
         (lla::copy-array-to-memory array (base-pointer foreign-array)
                                    (ctype->lla-internal (mat-ctype mat))))))

;;; foreign-array -> backing-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'foreign-array)) foreign-array
                        (to-name (eql 'backing-array)) array)
  (cond ((use-pinning-p)
         (lla::with-pinned-array (ptr array)
           (memcpy ptr (base-pointer foreign-array) (mat-n-bytes mat))))
        ((static-backing-array-p mat)
         (memcpy (static-vectors:static-vector-pointer array)
                 (base-pointer foreign-array) (mat-n-bytes mat)))
        (t
         (lla::copy-array-from-memory array (base-pointer foreign-array)
                                      (ctype->lla-internal (mat-ctype mat))))))

;;; foreign-array -> cuda-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'foreign-array)) foreign-array
                        (to-name (eql 'cuda-array)) cuda-array)
  (incf *n-memcpy-host-to-device*)
  (cl-cuda.driver-api::cu-memcpy-host-to-device (base-pointer cuda-array)
                                                (base-pointer foreign-array)
                                                (mat-n-bytes mat)))

;;; cuda-array -> foreign-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'cuda-array)) cuda-array
                        (to-name (eql 'foreign-array)) foreign-array)
  (incf *n-memcpy-device-to-host*)
  (cl-cuda.driver-api::cu-memcpy-device-to-host (base-pointer foreign-array)
                                                (base-pointer cuda-array)
                                                (mat-n-bytes mat)))

;;; backing-array -> cuda-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'backing-array)) array
                        (to-name (eql 'cuda-array)) cuda-array)
  (declare (ignorable array))
  (incf *n-memcpy-host-to-device*)
  ;; (break)
  (cond ((use-pinning-p)
         (lla::with-pinned-array (ptr array)
           (cl-cuda.driver-api::cu-memcpy-host-to-device
            (base-pointer cuda-array) ptr
            (mat-n-bytes mat))))
        ((static-backing-array-p mat)
         (cl-cuda.driver-api::cu-memcpy-host-to-device
          (base-pointer cuda-array)
          (static-vectors:static-vector-pointer array)
          (mat-n-bytes mat)))
        (t
         (with-facet (foreign-array (mat 'foreign-array :direction :input))
           (cl-cuda.driver-api::cu-memcpy-host-to-device
            (base-pointer cuda-array)
            (base-pointer foreign-array)
            (mat-n-bytes mat))))))

;;; cuda-array -> backing-array
(defmethod copy-facet* ((mat mat) (from-name (eql 'cuda-array)) cuda-array
                        (to-name (eql 'backing-array)) array)
  (declare (ignorable cuda-array))
  (incf *n-memcpy-device-to-host*)
  #+nil
  (unless *let-input-through-p*
    (break))
  (cond ((use-pinning-p)
         (lla::with-pinned-array (ptr array)
           (cl-cuda.driver-api::cu-memcpy-device-to-host
            ptr (base-pointer cuda-array)
            (mat-n-bytes mat))))
        ((static-backing-array-p mat)
         (cl-cuda.driver-api::cu-memcpy-device-to-host
          (static-vectors:static-vector-pointer array)
          (base-pointer cuda-array)
          (mat-n-bytes mat)))
        (t
         (with-facet (foreign-array (mat 'foreign-array :direction :input))
           (copy-facet* mat 'foreign-array foreign-array
                        'backing-array array)))))

(defun displacement-bytes (mat)
  (* (mat-displacement mat)
     (cffi:foreign-type-size (mat-ctype mat))))

(defmethod reshape-and-displace-facet* ((mat mat) facet-name
                                        (facet offset-pointer)
                                        dimensions displacement)
  (declare (ignore facet-name dimensions displacement))
  (setf (slot-value facet 'offset) (displacement-bytes mat)))

(defmethod reshape-and-displace-facet* ((mat mat) (facet-name (eql 'array))
                                        facet dimensions displacement)
  (declare (ignore facet dimensions displacement))
  (when (view-up-to-date-p (find-view mat 'array))
    (setf (view-up-to-date-p (find-view mat 'backing-array)) t))
  (destroy-facet mat 'array))

(defmethod call-with-facet* ((mat mat) (facet-name (eql 'foreign-array))
                             direction fn)
  (let ((backing-array-view (find-view mat 'backing-array))
        (foreign-array-view (find-view mat 'foreign-array)))
    (cond ((and (use-pinning-p)
                (or (not foreign-array-view)
                    (not (view-up-to-date-p foreign-array-view))))
           (with-facet (array (mat 'backing-array :direction direction))
             (declare (ignorable array))
             (lla::with-pinned-array (pointer array)
               (funcall fn (make-instance 'foreign-array
                                          :base-pointer pointer
                                          :offset (displacement-bytes mat))))))
          ((and (or (and (null backing-array-view)
                         (eq :static-backing-array *foreign-array-strategy*))
                    (and backing-array-view
                         (eq :static (view-facet-description
                                      backing-array-view))))
                (or (not foreign-array-view)
                    (not (view-up-to-date-p foreign-array-view))))
           (with-facet (array (mat 'backing-array :direction direction))
             (let ((pointer (static-vectors:static-vector-pointer array)))
               (funcall fn (make-instance 'foreign-array
                                          :base-pointer pointer
                                          :offset (displacement-bytes mat))))))
          (t
           (call-next-method)))))

(defmethod set-up-to-date-p* ((mat mat) (facet-name (eql 'array)) view value)
  (unless (eq (view-up-to-date-p view) value)
    (setf (view-up-to-date-p view) value)
    (let ((backing-array-view (find-view mat 'backing-array)))
      (when backing-array-view
        (setf (view-up-to-date-p backing-array-view) value)))))

(defmethod set-up-to-date-p* ((mat mat) (facet-name (eql 'backing-array))
                              view value)
  (unless (eq (view-up-to-date-p view) value)
    (setf (view-up-to-date-p view) value)
    (let ((array-view (find-view mat 'array)))
      (when array-view
        (setf (view-up-to-date-p array-view) value)))))


;;;; Utilities for defining the high[er] level api

(defun common-mat-ctype (&rest mats)
  (when mats
    (let ((ctype (mat-ctype (first mats))))
      (dolist (mat (rest mats))
        (assert (eq ctype (mat-ctype mat))))
      ctype)))


;;;; Elementwise operation definers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-elementwise-cuda-kernel (name (e) &body body)
    `(define-cuda-kernel (,name) (void ((x :mat :io) (n int)))
       (let ((stride (* block-dim-x grid-dim-x)))
         (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
                 (+ i stride)))
             ((>= i n))
           ,(if e
                `(let ((,e (aref x i)))
                   (set (aref x i) ,@body))
                `(set (aref x i) ,@body))))))

  (defmacro define-elementwise-lisp-kernel (name (e) &body body)
    `(define-lisp-kernel (,name) ((v :mat :io) (start index) (end index))
       (loop for i of-type index upfrom start below end
             do (setf (aref v i)
                      ,(if e
                           `(let ((,e (aref v i)))
                              ,@body)
                           body)))))

  (defmacro define-elementwise-dispatcher (name cuda-name lisp-name
                                           &optional docstring)
    `(defun ,name (x &key (n (mat-size x)))
       ,@(when docstring (list docstring))
       (assert (<= n (mat-size x)))
       (if (use-cuda-p)
           (,cuda-name x n :grid-dim (list (ceiling n 256) 1 1)
                       :block-dim (list 256 1 1))
           (let* ((start (mat-displacement x))
                  (end (+ start n)))
             (,lisp-name x start end)))
       x)))


;;;; Misc destructive operations

(defsection @mat-destructive-api (:title "Destructive API")
  (.square! function)
  (.sqrt! function)
  (.logistic! function)
  (.+! function)
  (.*! function)
  (geem! function)
  (.<! function)
  (add-sign! function)
  (fill! function)
  (sum! function)
  (scale-rows! function)
  "Finally, some neural network operations."
  (convolve! function)
  (derive-convolve! function)
  (max-pool! function)
  (derive-max-pool! function))

(define-elementwise-dispatcher .square! cuda-.square! lisp-.square!
  "Set X to its elementwise square. Return X.")
(define-elementwise-cuda-kernel cuda-.square! (e) (* e e))
(define-elementwise-lisp-kernel lisp-.square! (e) (* e e))

(define-elementwise-dispatcher .sqrt! cuda-.sqrt! lisp-.sqrt!
  "Set X to its elementwise square root. Return X.")
(define-elementwise-cuda-kernel cuda-.sqrt! (e) (sqrt e))
(define-elementwise-lisp-kernel lisp-.sqrt! (e) (the! single-float (sqrt e)))

(define-elementwise-dispatcher .logistic! cuda-.logistic! lisp-.logistic!
  "Destructively apply the logistic function to X in an elementwise
  manner. Return X.")
(define-elementwise-cuda-kernel cuda-.logistic! (e) (/ 1.0 (+ 1.0 (exp (- e)))))
(define-elementwise-lisp-kernel lisp-.logistic! (e)
  (/ 1.0 (+ 1.0 (with-zero-on-underflow (e) (exp (- e))))))

(define-cuda-kernel (cuda-.+!)
    (void ((alpha float) (x :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref x i) (+ (aref x i) alpha)))))

(define-lisp-kernel (lisp-.+!)
    ((alpha single-float) (x :mat :io) (start-x index) (n index))
  (loop for xi of-type index upfrom start-x
          below (the! index (+ start-x n))
        do (incf (aref x xi) alpha)))

(defun .+! (alpha x)
  "Add the scalar ALPHA to each element of X destructively modifying
  X. Return X."
  (let ((n (mat-size x))
        (alpha (coerce-to-ctype alpha :ctype (mat-ctype x))))
    (if (use-cuda-p)
        (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
          (cuda-.+! alpha x n :grid-dim grid-dim :block-dim block-dim))
        (lisp-.+! alpha x (mat-displacement x) n)))
  x)

(define-cuda-kernel (cuda-mult)
    (void ((x :mat :input) (y :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref y i) (* (aref x i) (aref y i))))))

(define-lisp-kernel (lisp-.*!)
    ((x :mat :input) (start-x index) (y :mat :io) (start-y index) (n index))
  (loop for xi of-type index upfrom start-x below (+ start-x n)
        for yi of-type index upfrom start-y
        do (setf (aref y yi) (* (aref x yi) (aref y yi)))))

(defun .*! (x y)
  (assert (= (mat-size x) (mat-size y)))
  (let ((n (mat-size x)))
    (if (use-cuda-p)
        (cuda-mult x y n :grid-dim (list (ceiling n 256) 1 1)
                   :block-dim (list 256 1 1))
        (lisp-.*! x (mat-displacement x) y (mat-displacement y) n))))

(define-cuda-kernel (cuda-geem!)
    (void ((alpha float) (a :mat :input) (b :mat :input)
           (beta float) (c :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref c i) (+ (* alpha (* (aref a i) (aref b i)))
                         (* beta (aref c i)))))))

(define-lisp-kernel (lisp-geem!)
    ((alpha single-float) (a :mat :input) (start-a index)
     (b :mat :input) (start-b index)
     (beta single-float) (c :mat :io) (start-c index) (n index))
  (loop for ai of-type index upfrom start-a
          below (the! index (+ start-a n))
        for bi of-type index upfrom start-b
        for ci of-type index upfrom start-c
        do (setf (aref c ci) (+ (* alpha (* (aref a ai) (aref b bi)))
                                (* beta (aref c ci))))))

(defun geem! (alpha a b beta c)
  "Like GEMM!, but multiplication is elementwise."
  (let* ((n (mat-size a))
         (ctype (mat-ctype a))
         (alpha (coerce-to-ctype alpha :ctype ctype))
         (beta (coerce-to-ctype beta :ctype ctype)))
    (assert (= n (mat-size b)))
    (assert (= n (mat-size c)))
    (if (use-cuda-p)
        (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
          (cuda-geem! alpha a b beta c n
                      :grid-dim grid-dim :block-dim block-dim))
        (lisp-geem! alpha a (mat-displacement a)
                    b (mat-displacement b)
                    beta c (mat-displacement c) n)))
  c)

(define-cuda-kernel (cuda-less-than!)
    (void ((x :mat :input) (y :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (if (< (aref x i) (aref y i))
          (set (aref y i) 1.0)
          (set (aref y i) 0.0)))))

(define-lisp-kernel (lisp-less-than!)
    ((x :mat :input) (start-x index) (y :mat :io) (start-y index) (n index))
  (loop for xi of-type index upfrom start-x below (+ start-x n)
        for yi of-type index upfrom start-y
        do (setf (aref y yi)
                 (if (< (aref x xi) (aref y yi))
                     1.0
                     0.0))))

(defun .<! (x y)
  "For each element of X and Y set Y to 1 if the element in Y is
  greater than the element in X, and to 0 otherwise."
  (assert (= (mat-size x) (mat-size y)))
  (let ((n (mat-size x)))
    (if (use-cuda-p)
        (cuda-less-than! x y n :grid-dim (list (ceiling n 256) 1 1)
                         :block-dim (list 256 1 1))
        (lisp-less-than! x (mat-displacement x) y (mat-displacement y) n))))

(define-lisp-kernel (lisp-add-sign!)
    ((alpha single-float) (x :mat :input) (start-x index)
     (beta single-float) (y :mat :io) (start-y index) (n index))
  (loop for xi of-type index upfrom start-x below (+ start-x n)
        for yi of-type index upfrom start-y
        do (setf (aref y yi)
                 (+ (* beta (aref y yi))
                    (let ((xe (aref x xi)))
                      (cond ((= 0.0 xe)
                             0.0)
                            ((< 0.0 xe)
                             alpha)
                            (t
                             (- alpha))))))))

(define-cuda-kernel (cuda-add-sign!)
    (void ((alpha float) (x :mat :input) (beta float) (y :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (let ((xe (aref x i)))
        (set (aref y i)
             (+ (* beta (aref y i))
                (if (= 0.0 xe)
                    0.0
                    (if (< 0.0 xe)
                        alpha
                        (- alpha)))))))))

(defun add-sign! (alpha a beta b)
  "Add the elementwise sign (-1, 0 or 1 for negative, zero and
  positive numbers respectively) of A times ALPHA to BETA * B. Return
  B."
  (let* ((n (mat-size a))
         (ctype (mat-ctype a))
         (alpha (coerce-to-ctype alpha :ctype ctype))
         (beta (coerce-to-ctype beta :ctype ctype)))
    (assert (= n (mat-size b)))
    (if (use-cuda-p)
        (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
          (cuda-add-sign! alpha a beta b n
                          :grid-dim grid-dim :block-dim block-dim))
        (lisp-add-sign! alpha a (mat-displacement a)
                        beta b (mat-displacement b) n)))
  b)

(define-cuda-kernel (cuda-fill!)
    (void ((alpha float) (x :mat :output) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref x i) alpha))))

(defun cuda-fill!-2 (ctype alpha cuda-array n)
  (let ((alpha (coerce-to-ctype alpha :ctype ctype)))
    (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
      (ecase ctype
        ((:float) (cuda-fill!-s alpha (offset-pointer cuda-array) n
                                :grid-dim grid-dim
                                :block-dim block-dim))
        ((:double) (cuda-fill!-d alpha (offset-pointer cuda-array) n
                                 :grid-dim grid-dim
                                 :block-dim block-dim))))))

(defun fill! (alpha x &key (n (mat-size x)))
  "Fill matrix X with ALPHA. Return X."
  (assert (<= n (mat-size x)))
  (let* ((start (mat-displacement x))
         (n (mat-size x))
         (end (+ start n))
         (ctype (mat-ctype x)))
    (cond ((use-cuda-p)
           (with-facets ((x* (x 'cuda-array :direction :output)))
             (cuda-fill!-2 ctype alpha x* n)))
          ((eq ctype :float)
           (let ((alpha (float alpha)))
             (with-facets ((x* (x 'backing-array :direction :output
                                  :type (simple-array single-float (*)))))
               (fill x* alpha :start start :end end))))
          ((eq ctype :double)
           (let ((alpha (float alpha 0d0)))
             (with-facets ((x* (x 'backing-array :direction :output
                                  :type (simple-array double-float (*)))))
               (fill x* alpha :start start :end end))))))
  x)

(defun sum! (x y &key axis (alpha 1) (beta 0))
  "Sum matrix X along AXIS and add ALPHA * SUMS to BETA * Y
  destructively modifying Y. Return Y. On a 2d matrix (nothing else is
  supported currently), if AXIS is 0, then columns are summed, if AXIS
  is 1 then rows are summed."
  (let* ((dimensions (mat-dimensions x))
         (n-dimensions (length dimensions))
         (ctype (common-mat-ctype x y)))
    (assert (<= 0 axis n-dimensions))
    (if (= 2 n-dimensions)
        (let ((n-rows (elt dimensions 0))
              (n-columns (elt dimensions 1)))
          (if (= axis 0)
              ;; sum columns
              (with-ones (ones n-rows :ctype ctype)
                (reshape! ones (list n-rows 1))
                (gemm! alpha x ones beta y :transpose-a? t
                       :m n-columns :n 1 :ldc 1))
              ;; sum rows
              (with-ones (ones n-columns :ctype ctype)
                (reshape! ones (list n-columns 1))
                (gemm! alpha x ones beta y
                       :m n-rows :n 1 :ldc 1))))
        (error "Not implemented."))
    y))

(define-lisp-kernel (lisp-scale-rows!)
    ((scales :mat :input) (start-scales index)
     (x :mat :input) (start-x index)
     (y :mat :output) (start-y index)
     (n-rows index) (n-columns index))
  (loop for row of-type index below n-rows do
    (let ((scale (aref scales (+ start-scales row)))
          (row-offset (the! index (* row n-columns))))
      (declare (type index row-offset))
      (loop for xi of-type index upfrom (+ start-x row-offset)
              below (+ start-x row-offset n-columns)
            for yi of-type index upfrom (+ start-y row-offset)
            do (setf (aref y yi) (* scale (aref x xi)))))))

(define-cuda-kernel (cuda-scale-rows!)
    (void ((scales :mat :input) (x :mat :input) (y :mat :output)
           (n-rows int) (n-columns int)))
  (let ((stride (* block-dim-x grid-dim-x))
        (n (* n-rows n-columns)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref y i)
           (* (aref scales (/ i n-columns))
              (aref x i))))))

(defun scale-rows! (scales a b)
  (destructuring-bind (n-rows n-columns) (mat-dimensions a)
    (assert (equal (mat-dimensions a) (mat-dimensions b)))
    (assert (= n-rows (mat-size scales)))
    (if (use-cuda-p)
        (multiple-value-bind (block-dim grid-dim)
            (choose-1d-block-and-grid (mat-size a) 4)
          (cuda-scale-rows! scales a b n-rows n-columns
                            :grid-dim grid-dim :block-dim block-dim))
        (lisp-scale-rows! scales (mat-displacement scales)
                          a (mat-displacement a)
                          b (mat-displacement b)
                          n-rows n-columns)))
  b)


(defsection @mat-blas (:title "BLAS")
  "Only some BLAS functions are implemented, but it should be easy to
  add more as needed. All of them default to using CUDA, if it is
  initialized and enabled (see USE-CUDA-P)."
  "Level 1 BLAS operations"
  (asum function)
  (axpy! function)
  (copy! function)
  (dot function)
  (nrm2 function)
  (scal! function)
  "Level 3 BLAS operations"
  (gemm! function))

;;;; Level 1 BLAS operations

(defun asum (x &key (n (mat-size x)) (incx 1))
  "Return the l1 norm of X, that is, sum of the absolute values of its
  elements."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (if (use-cuda-p)
      (cublas-asum n x incx)
      (blas-asum n x incx)))

(defun axpy! (alpha x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Set Y to ALPHA * X + Y. Return Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p)
      (cublas-axpy n alpha x incx y incy)
      (blas-axpy n alpha x incx y incy))
  y)

(defun copy! (x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Copy X into Y. Return Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p)
      (cublas-copy n x incx y incy)
      (blas-copy n x incx y incy))
  y)

(defun dot (x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Return the dot product of X and Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p)
      (cublas-dot n x incx y incy)
      (blas-dot n x incx y incy)))

(defun nrm2 (x &key (n (mat-size x)) (incx 1))
  "Return the l2 norm of X, which is the square root of the sum of the
  squares of its elements."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (if (use-cuda-p)
      (cublas-nrm2 n x incx)
      (blas-nrm2 n x incx)))

(defun scal! (alpha x &key (n (mat-size x)) (incx 1))
  "Set X to ALPHA * X. Return X."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (if (use-cuda-p)
      (cublas-scal n alpha x incx)
      (blas-scal n alpha x incx))
  x)

;;;; Level 3 BLAS operations

(defun gemm! (alpha a b beta c &key transpose-a? transpose-b?
              m n k lda ldb ldc)
  "Basically C = ALPHA * A' * B' + BETA * C. A' is A or its transpose
  depending on TRANSPOSE-A?. B' is B or its transpose depending on
  TRANSPOSE-B?. Returns C.

  A' is an MxK matrix. B' is a KxN matrix. C is an MxN matrix.

  LDA is the width of the matrix A (not of A'). If A is not transposed,
  then K <= LDA, if it's transposed then M <= LDA.

  LDB is the width of the matrix B (not of B'). If B is not transposed,
  then N <= LDB, if it's transposed then K <= LDB.

  In the example below M=3, N=2, K=5, LDA=6, LDB=3, LDC=4. The cells
  marked with + do not feature in the calculation.

                 N
                --+
                --+
              K -B+
                --+
                --+
                +++
          K
        -----+  --++
      M --A--+  -C++
        -----+  --++
        ++++++  ++++"
  (let* ((m (or m (mat-dimension c 0)))
         (n (or n (mat-dimension c 1)))
         (k (or k (if transpose-a?
                      (mat-dimension a 0)
                      (mat-dimension a 1))))
         (lda (or lda (mat-dimension a 1)))
         (ldb (or ldb (mat-dimension b 1)))
         (ldc (or ldc (mat-dimension c 1)))
         (ctype (common-mat-ctype a b c))
         (alpha (coerce-to-ctype alpha :ctype ctype))
         (beta (coerce-to-ctype beta :ctype ctype))
         (a-size (mat-size a))
         (b-size (mat-size b))
         (c-size (mat-size c)))
    (cond (transpose-a?
           (assert (<= m lda))
           (assert (<= (* lda k) a-size)))
          (t
           (assert (<= k lda))
           (assert (<= (* m lda) a-size))))
    (cond (transpose-b?
           (assert (<= k ldb))
           (assert (<= (* ldb n) b-size)))
          (t
           (assert (<= n ldb))
           (assert (<= (* k ldb) b-size))))
    (assert (<= n ldc))
    (assert (<= (* m ldc) c-size))
    (if (use-cuda-p)
        (cublas-gemm (if transpose-b? :cublas-op-c :cublas-op-n)
                     (if transpose-a? :cublas-op-c :cublas-op-n)
                     n m k
                     alpha b ldb a lda
                     beta c ldc)
        (blas-gemm (if transpose-b? #.(char-code #\C) #.(char-code #\N))
                   (if transpose-a? #.(char-code #\C) #.(char-code #\N))
                   n m k
                   alpha b ldb a lda
                   beta c ldc)))
  c)


(defsection @mat-non-destructive-api (:title "Non-destructive API")
  (copy-mat function)
  (copy-row function)
  (copy-column function)
  (mat-as-scalar function)
  (scalar-as-mat function)
  (m= function)
  (transpose function)
  (m* function)
  (mm* function)
  (m- function)
  (m+ function)
  (invert function)
  (logdet function))

(defun copy-mat (a)
  "Return a copy of the active portion with regards to displacement
  and shape of A. "
  (let ((b (make-mat (mat-dimensions a) :ctype (mat-ctype a))))
    (copy! a b)))

(defun copy-row (a row)
  "Return ROW of A as a new 1d matrix."
  (destructuring-bind (n-rows n-columns) (mat-dimensions a)
    (assert (< row n-rows))
    (let ((b (make-mat n-columns :ctype (mat-ctype a))))
      (with-shape-and-displacement (a n-columns (+ (mat-displacement a)
                                                   (* row n-columns)))
        (copy! a b)))))

(defun copy-column (a column)
  "Return COLUMN of A as a new 1d matrix."
  (destructuring-bind (n-rows n-columns) (mat-dimensions a)
    (assert (< column n-columns))
    (let ((b (make-mat n-rows :ctype (mat-ctype a))))
      (with-shape-and-displacement (a (- (* n-rows n-columns) column) column)
        (copy! a b :incx n-columns)))))

(defun mat-as-scalar (a)
  "Return the first element of A. A must be of size 1."
  (assert (= 1 (mat-size a)))
  (with-facets ((a* (a 'backing-array :direction :input)))
    (aref a* (mat-displacement a))))

(defun scalar-as-mat (x &key (ctype (lisp->ctype (type-of x))))
  "Return a matrix of one dimension and one element: X. CTYPE, the
  type of the matrix, defaults to the ctype corresponding to the type
  of X."
  (array-to-mat (vector x) :ctype ctype))

(defun m= (a b)
  "Check whether A and B, which must be matrices of the same size, are
  elementwise equal."
  (let ((start-a (mat-displacement a))
        (start-b (mat-displacement b))
        (n (mat-size a)))
    (assert (= n (mat-size b)))
    (with-facets ((a* (a 'backing-array :direction :input))
                  (b* (b 'backing-array :direction :input)))
      (loop for ai upfrom start-a below (+ start-a n)
            for bi upfrom start-b
            do (when (/= (aref a* ai) (aref b* bi))
                 (return nil))
            finally (return t)))))

(defun transpose (a)
  "Return the transpose of A."
  (with-facets ((array (a 'array :direction :input)))
    (array-to-mat (clnu:transpose array))))

(defun m* (a b &key transpose-a? transpose-b?)
  "Compute op(A) * op(B). Where op is either the identity or the
  transpose operation depending on TRANSPOSE-A? and TRANSPOSE-B?."
  (let* ((m (mat-dimension a (if transpose-a? 1 0)))
         (k (mat-dimension a (if transpose-a? 0 1)))
         (n (mat-dimension b (if transpose-b? 0 1)))
         (c (make-mat (list m n) :ctype (mat-ctype a))))
    (gemm! 1 a b 0 c
           :transpose-a? transpose-a? :transpose-b? transpose-b?
           :m m :n n :k k)
    c))

(defun mm* (m &rest args)
  "Convenience function to multiply several matrices. 

  (mm* a b c) => a * b * c"
  (let* ((args (cons m args))
         (n (length args)))
    (if (= 1 n)
        (destructuring-bind (a &key transpose?)
            (alexandria:ensure-list (first args))
          (if transpose?
              (transpose a)
              a))
        (destructuring-bind (a &key transpose?)
            (alexandria:ensure-list (pop args))
          (let ((transpose-a? transpose?))
            (destructuring-bind (b &key transpose?)
                (alexandria:ensure-list (pop args))
              (let ((c (m* a b :transpose-a? transpose-a?
                           :transpose-b? transpose?)))
                (loop while args
                      do (destructuring-bind (b &key transpose?)
                             (alexandria:ensure-list (pop args))
                           (setq c (m* c b :transpose-b? transpose?))))
                c)))))))

(defun m- (a b)
  "Return A - B."
  (let ((c (make-mat (mat-dimensions a) :ctype (mat-ctype a))))
    (copy! a c)
    (axpy! -1 b c)))

(defun m+ (a b)
  "Return A + B."
  (let ((c (make-mat (mat-dimensions a) :ctype (mat-ctype a))))
    (copy! a c)
    (axpy! 1 b c)))

(defun invert (a)
  "Return the inverse of A."
  (with-facets ((array (a 'array :direction :input)))
    (array-to-mat (lla:invert array))))

(defun logdet (mat)
  "Logarithm of the determinant of a matrix. Return -1, 1 or 0 (or
  equivalent) to correct for the sign, as a second value."
  (with-facets ((array (mat 'array :direction :input)))
    (lla:logdet array)))


(defsection @mat-mappings (:title "Mappings")
  (map-concat function)
  (map-rows function)
  (map-mats-into function))

(defun map-concat (fn mats mat &key key)
  "Call FN with each element of MATS and MAT temporarily reshaped to
  the dimensions of the current element of MATS and return MAT. For
  the next element the displacement is increased so that there is no
  overlap. MATS is keyed by KEY just like the CL sequence functions."
  (let* ((start (mat-displacement mat))
         (end (+ start (mat-size mat))))
    (with-shape-and-displacement (mat)
      (map nil (lambda (m)
                 (let* ((m (if key (funcall key m) m))
                        (size (mat-size m)))
                   (assert (<= (+ start size) end))
                   (reshape-and-displace! mat (mat-dimensions m) start)
                   (funcall fn m mat)
                   (incf start size)))
           mats)))
  mat)

(defun map-rows (fn mats mat &key key (from-row 0) (from-column 0))
  "Call FN with each element of MATS and MAT temporarily reshaped to
  its first row, second row, etc and return MAT. Actually the first
  row is given by FROM-ROW and rows are not necessarily full rows if
  FROM-COLUMN is greater than 0. MATS is keyed by KEY just like in CL
  sequence functions. It is not an error if there are fewer MATS than
  rows in MAT."
  (let* ((n-columns (mat-dimension mat 1))
         (displacement (mat-displacement mat))
         (size (- n-columns from-column)))
    (assert (<= 0 size))
    (with-shape-and-displacement (mat)
      (reshape! mat (list 1 size))
      (loop for m in mats
            for row upfrom from-row
            do (displace! mat (+ displacement (* row n-columns) from-column))
               (funcall fn (if key (funcall key m) m) mat))))
  mat)

(defun map-mats-into (result-mat fn &rest mats)
  "Like CL:MAP-INTO but for MAT objects. Destructively modifies
  RESULT-MAT to contain the results of applying FN to each element in
  the argument MATS in turn."
  (let ((n (mat-size result-mat)))
    (assert (every (lambda (mat) (= n (mat-size mat)))
                   mats))
    (with-facets ((r (result-mat 'array :direction :io)))
      (let ((mats-and-arrays (list (cons result-mat r))))
        (labels ((foo (mats)
                   (if (endp mats)
                       (let ((arrays (mapcar #'cdr (reverse mats-and-arrays))))
                         (destructuring-bind (result-array &rest arrays) arrays
                           (dotimes (i n)
                             (setf (row-major-aref result-array i)
                                   (apply fn (mapcar (lambda (array)
                                                       (row-major-aref array i))
                                                     arrays))))))
                       (let* ((mat (first mats))
                              (mat-and-array
                                (find mat mats-and-arrays :key #'car)))
                         (if mat-and-array
                             (progn
                               (push mat-and-array mats-and-arrays)
                               (foo (rest mats)))
                             (with-facets ((a (mat 'array :direction :input)))
                               (push (cons mat a) mats-and-arrays)
                               (foo (rest mats))))))))
          (foo mats)))))
  result-mat)


(defsection @mat-random (:title "Random numbers")
  "This is rather experimental."
  (mv-gaussian-random function)
  (copy-random-state generic-function)
  (uniform-random! function)
  (gaussian-random! function))

(defun gaussian-random-1 ()
  "Return a double float of zero mean and unit variance."
  (loop
    (let* ((x1 (1- (* 2d0 (random 1d0))))
           (x2 (1- (* 2d0 (random 1d0))))
           (w (+ (* x1 x1) (* x2 x2))))
      (declare (type double-float x1 x2)
               (type double-float w)
               (optimize (speed 3)))
      (when (< w 1d0)
        ;; Now we have two random numbers but return only one. The
        ;; other would be X1 times the same.
        (return
          (* x2
             (locally (declare (optimize (safety 0)))
               (the double-float (sqrt (/ (* -2d0 (log w)) w))))))))))

(defun mv-gaussian-random (&key means covariances)
  "Return a column vector of samples from the multivariate normal
  distribution defined by MEANS (Nx1) and COVARIANCES (NxN)."
  (let* ((n (mat-size means))
         (z (make-mat (list n 1) :ctype (mat-ctype means))))
    (with-facets ((z (z 'backing-array :direction :output)))
      (ecase (mat-ctype means)
        ((:float)
         (dotimes (i n)
           (setf (aref z i) (float (gaussian-random-1)))))
        ((:double)
         (dotimes (i n)
           (setf (aref z i) (gaussian-random-1))))))
    (m+ means (array-to-mat
               (with-facets ((covariances (covariances 'array
                                                       :direction :input))
                             (z (z 'array :direction :input)))
                 (lla:mm (lla:cholesky (clnu:hermitian-matrix covariances))
                         z))))))

(define-lisp-kernel (lisp-uniform-random)
    ((v :mat :output) (start index) (end index) (limit single-float))
  (loop for i of-type index upfrom start below end
        do (setf (aref v i) (random limit))))

(defun uniform-random! (mat &key (limit 1))
  "Fill MAT with random numbers sampled uniformly from the [0,LIMIT)
  interval of MAT's type."
  (let* ((ctype (mat-ctype mat))
         (limit (coerce-to-ctype limit :ctype ctype)))
    (cond ((use-cuda-p)
           (curand-uniform *curand-state* mat)
           (unless (= limit 1)
             (scal! limit mat)))
          (t
           (let* ((start (mat-displacement mat))
                  (end (+ start (mat-size mat))))
             (lisp-uniform-random mat start end limit)))))
  mat)

(defun gaussian-random! (mat &key (mean 0) (stddev 1))
  "Fill MAT with independent normally distributed random numbers with
  MEAN and STDDEV."
  
  (let* ((ctype (mat-ctype mat))
         (mean (coerce-to-ctype mean :ctype ctype))
         (stddev (coerce-to-ctype stddev :ctype ctype)))
    (cond ((use-cuda-p)
           (curand-normal *curand-state* mat)
           (unless (= stddev 1)
             (scal! stddev mat))
           (unless (= mean 0)
             (.+! mean mat)))
          (t
           (let* ((start (mat-displacement mat))
                  (end (+ start (mat-size mat))))
             (with-facets ((a (mat 'backing-array :direction :output)))
               (loop for i upfrom start below end
                     do (setf (aref a i)
                              (+ mean (* stddev (coerce-to-ctype (gaussian-random-1)
                                                                 :ctype ctype))))))))))
  mat)

(defgeneric copy-random-state (state))


(defsection @mat-io (:title "I/O")
  (write-mat generic-function)
  (read-mat generic-function))

(defgeneric write-mat (mat stream)
  (:method ((mat mat) stream)
    (let* ((start (mat-displacement mat))
           (end (+ start (mat-size mat))))
      (with-facets ((array (mat 'backing-array :direction :input)))
        (ecase (mat-ctype mat)
          ((:float)
           (write-single-float-array array stream :start start :end end))
          ((:double)
           (write-double-float-array array stream :start start :end end))))))
  (:documentation "Write MAT to STREAM in portable binary format.
  Displacement and size are taken into account, only visible elements
  are written."))

(defgeneric read-mat (mat stream)
  (:method ((mat mat) stream)
    (let* ((start (mat-displacement mat))
           (end (+ start (mat-size mat))))
      (with-facets ((array (mat 'backing-array :direction :input)))
        (ecase (mat-ctype mat)
          ((:float)
           (read-single-float-array array stream :start start :end end))
          ((:double)
           (read-double-float-array array stream :start start :end end))))))
  (:documentation "Destructively modify the visible portion (with
  regards to displacement and shape) of MAT by reading MAT-SIZE number
  of elements from STREAM. No sanity checks are performed, READ-MAT
  may return without error even if STREAM contains garbage."))


(defsection @mat-extension-api (:title "Extension API")
  "Macros for defining cuda and lisp kernels. Typically operations
  have a cuda and a lisp implementations and decide which to use with
  USE-CUDA-P. These are provided to help writing new operations."
  (define-lisp-kernel macro)
  (*default-lisp-kernel-declarations* variable)
  (define-cuda-kernel macro))
