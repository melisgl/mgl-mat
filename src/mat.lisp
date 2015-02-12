;;;; TODO
;;;;
;;;; - doc: OFFSET-POINTER, BASE-POINTER, CUDA-ARRAY
;;;;
;;;; - doc: separate cuda extension api

(in-package :mgl-mat)

(defsection @mat-manual (:title "MAT Manual")
  (mgl-mat asdf:system)
  (@mat-introduction section)
  (@mat-tutorial section)
  (@mat-basics section)
  (@mat-ctypes section)
  (@mat-printing section)
  (@mat-shaping section)
  (@mat-assembling section)
  (@mat-caching section)
  (@mat-blas section)
  (@mat-destructive-api section)
  (@mat-non-destructive-api section)
  (@mat-mappings section)
  (@mat-random section)
  (@mat-io section)
  (@mat-debugging section)
  (@mat-facet-api section)
  (@mat-extensions section))

(defsection @mat-introduction (:title "Introduction")
  (@mat-what-is-it section)
  (@mat-what-kind-of-matrices section)
  (@mat-installation section))

(defsection @mat-what-is-it (:title "What's MGL-MAT?")
  "\\MGL-MAT is library for working with multi-dimensional arrays
  which supports efficient interfacing to foreign and CUDA code with
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

(defsection @mat-installation (:title "Where to Get it?")
  "All dependencies are in quicklisp except for
  [CL-CUDA](https://github.com/takagi/cl-cuda) that needs to be
  fetched from github. Just clone \\CL-CUDA and \\MGL-MAT into
  `quicklisp/local-projects/` and you are set. \\MGL-MAT itself lives
  [at github](https://github.com/melisgl/mgl-mat), too.

  Prettier-than-markdown HTML documentation cross-linked with other
  libraries is
  [available](http://melisgl.github.io/mgl-pax-world/mat-manual.html)
  as part of [PAX World](http://melisgl.github.io/mgl-pax-world/).")


(defsection @mat-tutorial (:title "Tutorial")
  "We are going to see how to create matrices, access their contents.

  Creating matrices is just like creating lisp arrays:

  ```commonlisp
  (make-mat '6)
  ==> #<MAT 6 A #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0)>

  (make-mat '(2 3) :ctype :float :initial-contents '((1 2 3) (4 5 6)))
  ==> #<MAT 2x3 AB #2A((1.0 2.0 3.0) (4.0 5.0 6.0))>

  (make-mat '(2 3 4) :initial-element 1)
  ==> #<MAT 2x3x4 A #3A(((1.0d0 1.0d0 1.0d0 1.0d0)
  -->                    (1.0d0 1.0d0 1.0d0 1.0d0)
  -->                    (1.0d0 1.0d0 1.0d0 1.0d0))
  -->                   ((1.0d0 1.0d0 1.0d0 1.0d0)
  -->                    (1.0d0 1.0d0 1.0d0 1.0d0)
  -->                    (1.0d0 1.0d0 1.0d0 1.0d0)))>
  ```

  The most prominent difference from lisp arrays is that `MAT`s are
  always numeric and their element type (called CTYPE here) defaults
  to :DOUBLE.

  Individual elements can be accessed or set with MREF:

  ```commonlisp
  (let ((m (make-mat '(2 3))))
    (setf (mref m 0 0) 1)
    (setf (mref m 0 1) (* 2 (mref m 0 0)))
    (incf (mref m 0 2) 4)
    m)
  ==> #<MAT 2x3 AB #2A((1.0d0 2.0d0 4.0d0) (0.0d0 0.0d0 0.0d0))>
  ```

  Compared to AREF MREF is a very expensive operation and it's best
  used sparingly. Instead, typical code relies much more on matrix
  level operations:

  ```commonlisp
  (princ (scal! 2 (fill! 3 (make-mat 4))))
  .. #<MAT 4 BF #(6.0d0 6.0d0 6.0d0 6.0d0)>
  ==> #<MAT 4 ABF #(6.0d0 6.0d0 6.0d0 6.0d0)>
  ```

  Notice the `ABF` in the printed results. It illustrates that behind
  the scenes FILL! worked on the [BACKING-ARRAY][facet-name]
  facet (hence the `B`) that's basically a 1d lisp array. SCAL! on the
  other hand made a foreign call to the BLAS `dscal` function for
  which it needed the [FOREIGN-ARRAY][facet-name] facet (`F`).
  Finally, the `A` stands for the [ARRAY][facet-name] facet that was
  created when the array was printed. All facets are up-to-date (else
  some of the characters would be lowercase). This is possible because
  these three facets actually share storage which is never the case
  for the [CUDA-ARRAY][facet-name] facet. Now if we have a
  CUDA-capable GPU, CUDA can be enabled with WITH-CUDA*:

  ```commonlisp
  (with-cuda* ()
    (princ (scal! 2 (fill! 3 (make-mat 4)))))
  .. #<MAT 4 C #(6.0d0 6.0d0 6.0d0 6.0d0)>
  ==> #<MAT 4 A #(6.0d0 6.0d0 6.0d0 6.0d0)>
  ```

  Note the lonely `C` showing that only the [CUDA-ARRAY][facet-name]
  facet was used for both FILL! and SCAL!. When WITH-CUDA* exits and
  destroys the CUDA context, it destroys all CUDA facets, moving their
  data to the [ARRAY][facet-name] facet, so the returned MAT only has
  that facet.

  When there is no high-level operation that does what we want, we may
  need to add new operations. This is usually best accomplished by
  accessing one of the facets directly, as in the following example:"
  (log-det-example (include (:start (logdet function)
                                    :end (end-of-logdet-example variable))
                            :header-nl "```commonlisp"
                            :footer-nl "```")))


(defsection @mat-basics (:title "Basics")
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
  (row-major-mref function)
  (mat-row-major-index function))

(defvar *default-mat-cuda-enabled* t
  "The default for [CUDA-ENABLED][(accessor mat)].")

(defclass mat (cube)
  ((displacement
    :initform 0 :initarg :displacement :reader mat-displacement
    :documentation "A value in the [0,MAX-SIZE] interval. This is like
    the DISPLACED-INDEX-OFFSET of a lisp array.")
   (dimensions
    :initarg :dimensions :reader mat-dimensions
    :documentation "Like ARRAY-DIMENSIONS. It holds a list of
    dimensions, but it is allowed to pass in scalars too.")
   (size
    :reader mat-size
    :documentation "The number of elements in the visible portion of
    the array. This is always the product of the elements
    MAT-DIMENSIONS and is similar to ARRAY-TOTAL-SIZE.")
   (cuda-enabled
    :initform *default-mat-cuda-enabled*
    :initarg cuda-enabled :accessor cuda-enabled
    :documentation "The control provided by *CUDA-ENABLED* can be too
    coarse. This flag provides a per-object mechanism to turn cuda
    off. If it is set to NIL, then any operation that pays attention
    to this flag will not create or access the CUDA-ARRAY facet.
    Implementationally speaking, this is easily accomplished by using
    USE-CUDA-P.")
   ;; VEC is a CUBE to which we delegate most work and which has
   ;; LISP-VECTOR, STATIC-VECTOR and CUDA-ARRAY facets.
   (vec :initarg :vec :reader vec)
   ;; The rest of the slots here are fake, and redefined below in
   ;; DEFMETHOD forms to fetch values from VEC. By listing them here
   ;; we can document them without giving away implementation details.
   (ctype
    :type ctype :initform *default-mat-ctype*
    :initarg :ctype :reader mat-ctype
    :documentation "One of *SUPPORTED-CTYPES*. The matrix can hold
    only values of this type.")
   (initial-element
    :initform 0 :initarg :initial-element
    :reader mat-initial-element
    :documentation "If non-nil, then when a facet is created, it is
    filled with INITIAL-ELEMENT coerced to the appropriate numeric
    type. If NIL, then no initialization is performed.")
   (max-size
    :initarg :max-size :reader mat-max-size
    :documentation "The total size can be larger than MAT-SIZE, but
    cannot change. Also DISPLACEMENT + SIZE must not exceed it. This
    is not"))
  (:documentation "A MAT is a data CUBE that is much like a lisp
   array, it supports DISPLACEMENT, arbitrary DIMENSIONS and
   INITIAL-ELEMENT with the usual semantics. However, a MAT supports
   different representations of the same data. See @MAT-TUTORIAL for
   an introduction."))

(defmethod mat-ctype ((mat mat))
  (vec-ctype (vec mat)))

(defmethod mat-initial-element ((mat mat))
  (vec-initial-element (vec mat)))

(defmethod mat-max-size ((mat mat))
  (vec-size (vec mat)))

(defmethod initialize-instance :after ((mat mat) &key initial-contents
                                       (ctype *default-mat-ctype*)
                                       (initial-element 0)
                                       max-size
                                       &allow-other-keys)
  (unless (listp (mat-dimensions mat))
    (setf (slot-value mat 'dimensions)
          (list (mat-dimensions mat))))
  (setf (slot-value mat 'size) (mat-size-from-dimensions (mat-dimensions mat)))
  (setf (slot-value mat 'vec)
        (make-instance 'vec :ctype ctype :initial-element initial-element
                       :size (or max-size
                                 (+ (mat-displacement mat) (mat-size mat)))))
  (assert (<= (+ (mat-displacement mat) (mat-size mat)) (mat-max-size mat)))
  (when initial-contents
    (replace! mat initial-contents)))

;;; Optimized version of (REDUCE #'* DIMENSIONS).
(defun mat-size-from-dimensions (dimensions)
  (let ((product 1))
    (declare (type index product)
             (optimize speed))
    (dolist (dimension dimensions)
      (declare (type index dimension))
      (setq product (the! index (* product dimension))))
    product))

(defun mat-dimension (mat axis-number)
  "Return the dimension along AXIS-NUMBER. Similar to
  ARRAY-DIMENSION."
  (elt (mat-dimensions mat) axis-number))

(defun make-mat (dimensions &rest args &key (ctype *default-mat-ctype*)
                 (displacement 0) max-size (initial-element 0)
                 initial-contents (synchronization *default-synchronization*)
                 (cuda-enabled *default-mat-cuda-enabled*))
  "Return a new MAT object. If INITIAL-CONTENTS is given then the
  matrix contents are copied with REPLACE!. See class MAT for the
  description of the rest of the parameters. This is exactly
  what (MAKE-INSTANCE 'MAT ...) does except DIMENSIONS is not a
  keyword argument so that MAKE-MAT looks more like MAKE-ARRAY. The
  semantics of SYNCHRONIZATION are desribed in the
  @CUBE-SYNCHRONIZATION section."
  (declare (ignore displacement max-size initial-element initial-contents
                   cuda-enabled)
           (optimize speed)
           (dynamic-extent args))
  (apply #'make-instance 'mat :ctype ctype :dimensions dimensions
         :synchronization synchronization args))

(defun array-to-mat (array &key ctype
                     (synchronization *default-synchronization*))
  "Create a MAT that's equivalent to ARRAY. Displacement of the
  created array will be 0 and the size will be equal to
  ARRAY-TOTAL-SIZE. If CTYPE is non-nil, then it will be the ctype of
  the new matrix. Else ARRAY's type is converted to a ctype. If there
  is no corresponding ctype, then *DEFAULT-MAT-CTYPE* is used.
  Elements of ARRAY are coerced to CTYPE.

  Also see @CUBE-SYNCHRONIZATION."
  (let* ((ctype (or ctype
                    (lisp->ctype (array-element-type array))
                    *default-mat-ctype*))
         (mat (make-instance 'mat
                             :ctype ctype
                             :dimensions (array-dimensions array)
                             :synchronization synchronization)))
    (with-facet (backing-array (mat 'backing-array :direction :output))
      (loop for i upfrom 0
            for j upfrom 0
            repeat (mat-size mat)
            do (setf (aref backing-array i)
                     (coerce-to-ctype (row-major-aref array j)
                                      :ctype ctype))))
    mat))

(defun mat-to-array (mat)
  (with-facet (array (mat 'array :direction :input))
    (alexandria:copy-array array)))

(defun replace! (mat seq-of-seqs)
  "Replace the contents of MAT with the elements of SEQ-OF-SEQS.
  SEQ-OF-SEQS is a nested sequence of sequences similar to the
  INITIAL-CONTENTS argument of MAKE-ARRAY. The total number of
  elements must match the size of MAT. Returns MAT.

  SEQ-OF-SEQS may contain multi-dimensional arrays as _leafs_, so the
  following is legal:

  ```common-lisp
  (replace! (make-mat '(1 2 3)) '(#2A((1 2 3) (4 5 6))))
  ==> #<MAT 1x2x3 AB #3A(((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0)))>
  ```"
  (with-facets ((m (mat 'backing-array :direction :output)))
    (replace-vector m (mat-displacement mat) (mat-dimensions mat)
                    seq-of-seqs (mat-ctype mat)))
  mat)

(defun replace-vector (vector start dimensions seq-of-seqs ctype)
  (let ((i start)
        (n (mat-size-from-dimensions dimensions)))
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
                       ((typep seq-of-seqs 'sequence)
                        (map nil (lambda (seq-of-seqs)
                                   (foo (cdr dims) seq-of-seqs))
                             seq-of-seqs))
                       ((arrayp seq-of-seqs)
                        (let* ((n (length (array-dimensions seq-of-seqs)))
                               (dims (nthcdr n dims)))
                          (loop for j below (array-total-size seq-of-seqs)
                                do (foo dims (row-major-aref seq-of-seqs j)))))
                       (t
                        (assert nil () "Unexpected non-sequence type ~S."
                                (type-of seq-of-seqs)))))))
      (foo dimensions seq-of-seqs))
    (unless (= i (+ start n))
      (error "Total size of ~S is not ~S." seq-of-seqs n))))

(defun mref (mat &rest indices)
  "Like AREF for arrays. Don't use this if you care about performance
  at all. SETFable. When set, the value is coerced to the ctype of MAT
  with COERCE-TO-CTYPE. Note that currently MREF always operates on
  the BACKING-ARRAY facet so it can trigger copying of facets. When
  it's SETF'ed, however, it will update the CUDA-ARRAY if cuda is
  enabled and it is up-to-date or there are no facets at all."
  (declare (dynamic-extent indices))
  (let ((index (apply #'mat-row-major-index mat indices)))
    (with-facets ((a (mat 'backing-array :direction :input)))
      (row-major-aref a (+ (mat-displacement mat) index)))))

(defun set-mref (value mat &rest indices)
  (declare (optimize speed)
           (dynamic-extent indices))
  (set-row-major-mref mat (apply #'mat-row-major-index mat indices) value))

(define-cuda-kernel (cuda-setf-mref)
    (void ((x :mat :io) (index int) (value float)))
  (set (aref x index) value))

(defsetf mref (mat &rest indices) (value)
  `(set-mref ,value ,mat ,@indices))

(defun row-major-mref (mat index)
  "Like ROW-MAJOR-AREF for arrays. Don't use this if you care about
  performance at all. SETFable. When set, the value is coerced to the
  ctype of MAT with COERCE-TO-CTYPE. Note that currently
  ROW-MAJOR-MREF always operates on the BACKING-ARRAY facet so it can
  trigger copying of facets. When it's SETF'ed, however, it will
  update the CUDA-ARRAY if cuda is enabled and it is up-to-date or
  there are no facets at all."
  (with-facets ((a (mat 'backing-array :direction :input)))
    (row-major-aref a (+ (mat-displacement mat) index))))

(defun set-row-major-mref (mat index value)
  (let ((value (coerce-to-ctype value :ctype (mat-ctype mat))))
    (cond ((and (use-cuda-p mat)
                (let ((facet (find-facet mat 'cuda-array)))
                  (or (and facet (facet-up-to-date-p* mat 'cuda-array facet))
                      (endp (facets mat)))))
           (assert (and (<= 0 index) (< index (mat-size mat)))
                   () "Index ~S out of bounds for ~S." index mat)
           (cuda-setf-mref mat index value
                           :grid-dim '(1 1 1) :block-dim '(1 1 1)))
          (t
           (with-facets ((a (mat 'backing-array :direction :io)))
             (setf (row-major-aref a (+ (mat-displacement mat) index))
                   (coerce-to-ctype value :ctype (mat-ctype mat))))))
    value))

(defsetf row-major-mref set-row-major-mref)

(defun mat-row-major-index (mat &rest subscripts)
  "Like ARRAY-ROW-MAJOR-INDEX for arrays."
  (declare (optimize speed)
           (dynamic-extent subscripts))
  ;; Can't call ARRAY-ROW-MAJOR-INDEX because we may not have an ARRAY
  ;; facet.
  (let ((sum 0)
        (multiplier (mat-size mat)))
    (declare (optimize speed)
             (type index sum multiplier))
    (loop for index of-type index in subscripts
          for dimension of-type index in (mat-dimensions mat)
          do (setq multiplier (/ multiplier dimension))
             (setq sum (the! index (+ sum (the! index (* multiplier index))))))
    sum))


(defsection @mat-printing (:title "Printing")
  (*print-mat* variable)
  (*print-mat-facets* variable))

(defvar *print-mat* t
  "Controls whether the contents of a MAT object are printed as an
  array (subject to the standard printer control variables).")

(defvar *print-mat-facets* t
  "Controls whether a summary of existing and up-to-date facets is
  printed when a MAT object is printed. The summary that looks like
  `ABcfh` indicates that all five facets ([ARRAY][facet-name],
  [BACKING-ARRAY][facet-name], [CUDA-ARRAY][facet-name],
  [FOREIGN-ARRAY][facet-name], [CUDA-HOST-ARRAY][facet-name]) are
  present and the first two are up-to-date. A summary of a single #\-
  indicates that there are no facets.")

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

(defun mat-facet-to-char (mat facet)
  (let* ((name (facet-name facet))
         (char (if (eq name 'cuda-host-array)
                   #\h
                   (aref (symbol-name name) 0))))
    (if (facet-up-to-date-p* mat name facet)
        (char-upcase char)
        (char-downcase char))))

(defun print-mat-facets (mat stream)
  (let ((chars (mapcar (lambda (facet)
                         (mat-facet-to-char mat facet))
                       (facets mat))))
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
  The [FOREIGN-ARRAY][facet-name] and [CUDA-ARRAY][facet-name] facets
  are [OFFSET-POINTER][class] objects so displacement is done by
  changing the offset. Clients need to observe MAT-DIMENSIONS in any
  case."
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
  (check-no-watchers mat nil "Cannot reshape or displace the matrix")
  (let ((dimensions (alexandria:ensure-list dimensions)))
    (when (or (not (equal (mat-dimensions mat) dimensions))
              (not (= (mat-displacement mat) displacement)))
      (let ((size (mat-size-from-dimensions dimensions)))
        (assert (<= (+ displacement size) (mat-max-size mat)))
        (setf (slot-value mat 'dimensions) dimensions)
        (setf (slot-value mat 'displacement) displacement)
        (setf (slot-value mat 'size) size)
        (dolist (facet (facets mat))
          (reshape-and-displace-facet* mat (facet-name facet) facet
                                       dimensions displacement))))
    mat))

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
  possible by the row-major layout, hence no column counterpart.
  Return MAT."
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
         (size (mat-size-from-dimensions dimensions)))
    (if (<= (+ displacement size) (mat-max-size mat))
        (reshape-and-displace! mat dimensions displacement)
        (prog1
            (make-mat dimensions :displacement displacement
                      :ctype (mat-ctype mat)
                      :initial-element (mat-initial-element mat))
          (when destroy-old-p
            (destroy-cube mat))))))


(defsection @mat-assembling (:title "Assembling")
  "The functions here assemble a single MAT from a number of
  [MAT][]s."
  (stack! function)
  (stack function))

(defun stack! (axis mats mat)
  "Stack MATS along AXIS into MAT and return MAT. If AXIS is 0, place
  MATS into MAT below each other starting from the top. If AXIS is 1,
  place MATS side by side starting from the left. Higher AXIS are also
  supported. All dimensions except for AXIS must be the same for all
  MATS."
  ;; FIXME: this implementation is consing a lot.
  (labels ((foo (mats facets)
             (if (endp mats)
                 (apply #'aops:stack axis (reverse facets))
                 (call-with-facet* (first mats) 'array :input
                                   (lambda (facet)
                                     (foo (rest mats)
                                          (cons facet facets)))))))
    (replace! mat (foo mats ()))))

(defun stack (axis mats &key (ctype *default-mat-ctype*))
  "Like STACK! but return a new MAT of CTYPE.

  ```commonlisp
  (stack 1 (list (make-mat '(3 2) :initial-element 0)
                 (make-mat '(3 1) :initial-element 1)))
  ==> #<MAT 3x3 B #2A((0.0d0 0.0d0 1.0d0)
  -->                 (0.0d0 0.0d0 1.0d0)
  -->                 (0.0d0 0.0d0 1.0d0))>
  ```"
  (labels ((foo (mats facets)
             (if (endp mats)
                 (apply #'aops:stack axis (reverse facets))
                 (call-with-facet* (first mats) 'array :input
                                   (lambda (facet)
                                     (foo (rest mats)
                                          (cons facet facets)))))))
    
    (array-to-mat (foo mats ()) :ctype ctype)))


(defsection @mat-caching (:title "Caching")
  "Allocating and initializing a MAT object and its necessary facets
  can be expensive. The following macros remember the previous value
  of a binding in the same thread and /place/. Only weak references
  are constructed so the cached objects can be garbage collected.

  While the cache is global, thread safety is guaranteed by having
  separate subcaches per thread. Each subcache is keyed by a /place/
  object that's either explicitly specified or else is unique to each
  invocation of the caching macro, so different occurrences of caching
  macros in the source never share data. Still, recursion could lead
  to data sharing between different invocations of the same function.
  To prevent this, the cached object is removed from the cache while
  it is used so other invocations will create a fresh one which isn't
  particularly efficient but at least it's safe."
  (with-thread-cached-mat macro)
  (with-thread-cached-mats macro)
  (with-ones macro))

(defmacro with-thread-cached-mat ((var dimensions &rest args
                                   &key (place :scratch)
                                   (ctype '*default-mat-ctype*)
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
    (remf args :place)
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
                                    ,@args)
                :place ,place)
             (setq ,var (adjust! ,var ,dimensions ,displacement))
             (locally ,@body)))))))

(defmacro with-thread-cached-mats (specs &body body)
  "A shorthand for writing nested WITH-THREAD-CACHED-MAT calls.

  ```
  (with-thread-cached-mat (a ...)
    (with-thread-cached-mat (b ...)
      ...))
  ```

  is equivalent to:

  ```
  (with-thread-cached-mat ((a ...)
                           (b ...))
    ...)
  ```"
  (labels ((foo (specs)
             (if (endp specs)
                 `(locally ,@body)
                 `(with-thread-cached-mat ,(first specs)
                    ,(foo (rest specs))))))
    (foo specs)))

(defmacro with-ones ((var dimensions &key (ctype '*default-mat-ctype*)
                      (place :ones))
                     &body body)
  "Bind VAR to a matrix of DIMENSIONS whose every element is 1. The
  matrix is cached for efficiency."
  `(with-thread-cached-mat (,var ,dimensions :place ,place
                                 :ctype ,ctype :initial-element 1)
     ;; Rebind VAR to make sure that WITH-THREAD-CACHED-MAT doesn't
     ;; update the cached object if BODY changes the binding.
     (let ((,var ,var))
       ,@body)))


;;;; Utilities for defining the high[er] level api

(defun common-mat-ctype (&rest mats)
  (declare (optimize speed)
           (dynamic-extent mats))
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
       (if (use-cuda-p x)
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
  (.log! function)
  (.exp! function)
  (.logistic! function)
  (.+! function)
  (.*! function)
  (geem! function)
  (geerv! function)
  (.<! function)
  (.min! function)
  (.max! function)
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

(define-elementwise-dispatcher .log! cuda-.log! lisp-.log!
  "Set X to its elementwise natural logarithm. Return X.")
(define-elementwise-cuda-kernel cuda-.log! (e) (log e))
(define-elementwise-lisp-kernel lisp-.log! (e) (the! single-float (log e)))

(define-elementwise-dispatcher .exp! cuda-.exp! lisp-.exp!
  "Apply EXP elementwise to X in a destructive manner. Return X.")
(define-elementwise-cuda-kernel cuda-.exp! (e) (exp e))
(define-elementwise-lisp-kernel lisp-.exp! (e) (the! single-float (exp e)))

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
    (if (use-cuda-p x)
        (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
          (cuda-.+! alpha x n :grid-dim grid-dim :block-dim block-dim))
        (lisp-.+! alpha x (mat-displacement x) n)))
  x)

(defun .*! (x y)
  (geem! 1 x y 0 y))

(defun geem! (alpha a b beta c)
  "Like GEMM!, but multiplication is elementwise. This is not a
  standard BLAS routine."
  (let* ((n (mat-size a))
         (ctype (mat-ctype a))
         (alpha (coerce-to-ctype alpha :ctype ctype))
         (beta (coerce-to-ctype beta :ctype ctype)))
    (assert (= n (mat-size b)))
    (assert (= n (mat-size c)))
    (if (use-cuda-p a b c)
        (multiple-value-bind (block-dim grid-dim) (choose-1d-block-and-grid n 4)
          (cuda-geem! alpha a b beta c n
                      :grid-dim grid-dim :block-dim block-dim))
        (lisp-geem! alpha a (mat-displacement a)
                    b (mat-displacement b)
                    beta c (mat-displacement c) n)))
  c)

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

(defun geerv! (alpha a x beta b)
  "GEneric Elementwise Row - Vector multiplication. B = beta * B + alpha * a
  * diag(x). In other words, perform elementwise multiplication on
  each row of A with the vector X and add the scaled result to the
  corresponding row of B. Return B. This is not a standard BLAS
  routine."
  (assert (equal (mat-dimensions a) (mat-dimensions b)))
  (let* ((ctype (mat-ctype a))
         (alpha (coerce-to-ctype alpha :ctype ctype))
         (beta (coerce-to-ctype beta :ctype ctype)))
    (destructuring-bind (n-rows n-columns) (mat-dimensions a)
      (assert (= n-columns (mat-size x)))
      (if (use-cuda-p a x b)
          (multiple-value-bind (block-dim grid-dim)
              (choose-1d-block-and-grid (mat-size a) 4)
            (cuda-geerv! alpha a x beta b n-rows n-columns
                         :grid-dim grid-dim :block-dim block-dim))
          (lisp-geerv! alpha a (mat-displacement a)
                       x (mat-displacement x)
                       beta
                       b (mat-displacement b)
                       n-rows n-columns))))
  b)

(define-cuda-kernel (cuda-geerv!)
    (void ((alpha float) (a :mat :input) (x :mat :input)
           (beta float) (b :mat :io) (n-rows int) (n-columns int)))
  (let ((stride (* block-dim-x grid-dim-x))
        (n (* n-rows n-columns)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (set (aref b i) (+ (* alpha (aref a i) (aref x (mod i n-columns)))
                         (* beta (aref b i)))))))

(define-lisp-kernel (lisp-geerv!)
    ((alpha single-float) (a :mat :input) (start-a index)
     (x :mat :input) (start-x index)
     (beta single-float)
     (b :mat :io) (start-b index) (n-rows index) (n-columns index))
  (dotimes (row n-rows)
    (let ((row-offset (the! index (* row n-columns))))
      (loop for ai of-type index upfrom (the! index (+ start-a row-offset))
              below (the! index (+ start-a row-offset n-columns))
            for xi of-type index upfrom start-x
            for bi of-type index upfrom (the! index (+ start-b row-offset))
            do (setf (aref b bi)
                     (+ (* alpha (aref a ai) (aref x xi))
                        (* beta (aref b bi))))))))

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
  greater than the element in X, and to 0 otherwise. Return Y."
  (assert (= (mat-size x) (mat-size y)))
  (let ((n (mat-size x)))
    (if (use-cuda-p x y)
        (cuda-less-than! x y n :grid-dim (list (ceiling n 256) 1 1)
                         :block-dim (list 256 1 1))
        (lisp-less-than! x (mat-displacement x) y (mat-displacement y) n))
    y))

(defun .min! (alpha x)
  "Set each element of X to ALPHA if it's greater than ALPHA. Return
  X."
  (let* ((n (mat-size x))
         (ctype (mat-ctype x))
         (alpha (coerce-to-ctype alpha :ctype ctype)))
    (if (use-cuda-p x)
        (cuda-.min! alpha x n :grid-dim (list (ceiling n 256) 1 1)
                    :block-dim (list 256 1 1))
        (lisp-.min! alpha x (mat-displacement x) n))
    x))

(define-cuda-kernel (cuda-.min!)
    (void ((alpha float) (x :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (when (< alpha (aref x i))
        (set (aref x i) alpha)))))

(define-lisp-kernel (lisp-.min!)
    ((alpha single-float) (x :mat :io) (start-x index) (n index))
  (loop for xi of-type index upfrom start-x
          below (the! index (+ start-x n))
        do (when (< alpha (aref x xi))
             (setf (aref x xi) alpha))))

(defun .max! (alpha x)
  "Set each element of X to ALPHA if it's less than ALPHA. Return X."
  (let* ((n (mat-size x))
         (ctype (mat-ctype x))
         (alpha (coerce-to-ctype alpha :ctype ctype)))
    (if (use-cuda-p x)
        (cuda-.max! alpha x n :grid-dim (list (ceiling n 256) 1 1)
                    :block-dim (list 256 1 1))
        (lisp-.max! alpha x (mat-displacement x) n))
    x))

(define-cuda-kernel (cuda-.max!)
    (void ((alpha float) (x :mat :io) (n int)))
  (let ((stride (* block-dim-x grid-dim-x)))
    (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
            (+ i stride)))
        ((>= i n))
      (when (< (aref x i) alpha)
        (set (aref x i) alpha)))))

(define-lisp-kernel (lisp-.max!)
    ((alpha single-float) (x :mat :io) (start-x index) (n index))
  (loop for xi of-type index upfrom start-x
          below (the! index (+ start-x n))
        do (when (< (aref x xi) alpha)
             (setf (aref x xi) alpha))))

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
    (if (use-cuda-p a b)
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
    (cond ((use-cuda-p x)
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
    (if (use-cuda-p a b)
        (multiple-value-bind (block-dim grid-dim)
            (choose-1d-block-and-grid (mat-size a) 4)
          (cuda-scale-rows! scales a b n-rows n-columns
                            :grid-dim grid-dim :block-dim block-dim))
        (lisp-scale-rows! scales (mat-displacement scales)
                          a (mat-displacement a)
                          b (mat-displacement b)
                          n-rows n-columns)))
  b)


(defsection @mat-blas (:title "BLAS Operations")
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
  (if (use-cuda-p x)
      (cublas-asum n x incx)
      (blas-asum n x incx)))

(defun axpy! (alpha x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Set Y to ALPHA * X + Y. Return Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p x y)
      (cublas-axpy n alpha x incx y incy)
      (blas-axpy n alpha x incx y incy))
  y)

(defun copy! (x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Copy X into Y. Return Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p x y)
      (cublas-copy n x incx y incy)
      (blas-copy n x incx y incy))
  y)

(defun dot (x y &key (n (mat-size x)) (incx 1) (incy 1))
  "Return the dot product of X and Y."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (assert (<= (abs (* n incy)) (mat-size y)))
  (if (use-cuda-p x y)
      (cublas-dot n x incx y incy)
      (blas-dot n x incx y incy)))

(defun nrm2 (x &key (n (mat-size x)) (incx 1))
  "Return the l2 norm of X, which is the square root of the sum of the
  squares of its elements."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (if (use-cuda-p x)
      (cublas-nrm2 n x incx)
      (blas-nrm2 n x incx)))

(defun scal! (alpha x &key (n (mat-size x)) (incx 1))
  "Set X to ALPHA * X. Return X."
  (assert (<= (abs (* n incx)) (mat-size x)))
  (if (use-cuda-p x)
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
    (if (use-cuda-p a b c)
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
  (declare (optimize speed)
           (dynamic-extent args))
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
  "Logarithm of the determinant of MAT. Return -1, 1 or 0 (or
  equivalent) to correct for the sign, as the second value."
  (with-facets ((array (mat 'array :direction :input)))
    (lla:logdet array)))

(defvar end-of-logdet-example)


(defsection @mat-mappings (:title "Mappings")
  (map-concat function)
  (map-displacements function)
  (map-mats-into function))

(defun map-concat (fn mats mat &key key pass-raw-p)
  "Call FN with each element of MATS and MAT temporarily reshaped to
  the dimensions of the current element of MATS and return MAT. For
  the next element the displacement is increased so that there is no
  overlap.

  MATS is keyed by KEY just like the CL sequence functions. Normally,
  FN is called with the matrix returned by KEY. However, if
  PASS-RAW-P, then the matrix returned by KEY is only used to
  calculate dimensions and the element of MATS that was passed to KEY
  is passed to FN, too.

  ```
  (map-concat #'copy! (list (make-mat 2) (make-mat 4 :initial-element 1))
              (make-mat '(2 3)))
  ==> #<MAT 2x3 AB #2A((0.0d0 0.0d0 1.0d0) (1.0d0 1.0d0 1.0d0))>
  ```"
  (let* ((start (mat-displacement mat))
         (end (+ start (mat-size mat))))
    (with-shape-and-displacement (mat)
      (map nil (lambda (m)
                 (let* ((m0 (if key (funcall key m) m))
                        (size (mat-size m0)))
                   (assert (<= (+ start size) end))
                   (reshape-and-displace! mat (mat-dimensions m0) start)
                   (funcall fn (if pass-raw-p m m0) mat)
                   (incf start size)))
           mats)))
  mat)

(defun map-displacements (fn mat dimensions &key (displacement-start 0)
                          displacement-step)
  ;; KLUDGE: No cl-transcript because whether A shows up in the
  ;; up-to-date facet list depends on the value of *PRINT-CIRCLE*, its
  ;; implementation and whether DESCRIBE binds *PRINT-CIRCLE*.
  "Call FN with MAT reshaped to DIMENSIONS, first displaced by
  DISPLACEMENT-START that's incremented by DISPLACEMENT-STEP each
  iteration while there are enough elements left for DIMENSIONS at the
  current displacement. Returns MAT.

  ```commonlisp
  (let ((mat (make-mat 14 :initial-contents '(-1 0 1 2 3
                                              4 5 6 7
                                              8 9 10 11 12))))
    (reshape-and-displace! mat '(4 3) 1)
    (map-displacements #'print mat 4))
  ..
  .. #<MAT 1+4+9 B #(0.0d0 1.0d0 2.0d0 3.0d0)> 
  .. #<MAT 5+4+5 B #(4.0d0 5.0d0 6.0d0 7.0d0)> 
  .. #<MAT 9+4+1 B #(8.0d0 9.0d0 10.0d0 11.0d0)> 
  ```"
  (let ((displacement-step
          (or displacement-step
              (mat-size-from-dimensions (alexandria:ensure-list dimensions))))
        (displacement (mat-displacement mat))
        (size (mat-size mat)))
    (with-shape-and-displacement (mat)
      (reshape! mat dimensions)
      (loop for d upfrom displacement-start upto (- size displacement-step)
            by displacement-step
            do (displace! mat (+ displacement d))
               (funcall fn mat))))
  mat)

(defun map-mats-into (result-mat fn &rest mats)
  "Like CL:MAP-INTO but for MAT objects. Destructively modifies
  RESULT-MAT to contain the results of applying FN to each element in
  the argument MATS in turn."
  (declare (dynamic-extent mats))
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
  "Unless noted these work efficiently with CUDA."
  (copy-random-state generic-function)
  (uniform-random! function)
  (gaussian-random! function)
  (mv-gaussian-random function)
  (orthogonal-random! function))

(defgeneric copy-random-state (state)
  (:method ((state curand-state))
    (copy-curand-state state))
  (:method ((state random-state))
    (make-random-state state))
  (:documentation "Return a copy of STATE be it a lisp or cuda random
  state."))

(defun uniform-random! (mat &key (limit 1))
  "Fill MAT with random numbers sampled uniformly from the [0,LIMIT)
  interval of MAT's type."
  (let* ((ctype (mat-ctype mat))
         (limit (coerce-to-ctype limit :ctype ctype)))
    (cond ((use-cuda-p mat)
           (curand-uniform *curand-state* mat)
           (unless (= limit 1)
             (scal! limit mat)))
          (t
           (let* ((start (mat-displacement mat))
                  (end (+ start (mat-size mat))))
             (lisp-uniform-random mat start end limit)))))
  mat)

(define-lisp-kernel (lisp-uniform-random)
    ((v :mat :output) (start index) (end index) (limit single-float))
  (loop for i of-type index upfrom start below end
        do (setf (aref v i) (random limit))))

(defun gaussian-random! (mat &key (mean 0) (stddev 1))
  "Fill MAT with independent normally distributed random numbers with
  MEAN and STDDEV."
  (let* ((ctype (mat-ctype mat))
         (mean (coerce-to-ctype mean :ctype ctype))
         (stddev (coerce-to-ctype stddev :ctype ctype)))
    (cond ((use-cuda-p mat)
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
                              (+ mean (* stddev
                                         (coerce-to-ctype (gaussian-random-1)
                                                          :ctype ctype))))))))))
  mat)

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
  distribution defined by MEANS (Nx1) and COVARIANCES (NxN). No CUDA
  implementation."
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

(defun orthogonal-random! (m &key (scale 1))
  "Fill the matrix M with random values in such a way that `M^T * M`
  is the identity matrix (or something close if M is wide). Return M."
  (uniform-random! m)
  (let* ((svd (with-facets ((a (m 'array :direction :input)))
                (lla:svd a :thin)))
         (u (lla:svd-u svd))
         (vt (lla:svd-vt svd)))
    (cond ((equal (mat-dimensions m) (array-dimensions u))
           (replace! m u))
          ((equal (mat-dimensions m) (array-dimensions vt))
           (replace! m vt))
          (t
           (assert nil))))
  (scal! scale m))


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


(defsection @mat-debugging (:title "Debugging")
  "The largest class of bugs has to do with synchronization of facets
  being broken. This is almost always caused by an operation that
  mispecifies the DIRECTION argument of WITH-FACET. For example, the
  matrix argument of SCAL! should be accessed with direciton :IO. But
  if it's :INPUT instead, then subsequent access to the ARRAY facet
  will not see the changes made by AXPY!, and if it's :OUTPUT, then
  any changes made to the ARRAY facet since the last update of the
  CUDA-ARRAY facet will not be copied and from the wrong input SCAL!
  will compute the wrong result.

  Another thing that tends to come up is figuring out where memory is
  used."
  (mat-room function)
  (with-mat-counters macro))

(defun mat-room (&key (stream *standard-output*) (verbose t))
  "Calls FOREIGN-ROOM and CUDA-ROOM."
  (foreign-room :stream stream :verbose verbose)
  (cuda-room :stream stream :verbose verbose))

(defvar *counters* ())

(defmacro with-mat-counters ((&key count n-bytes) &body body)
  "Count all MAT allocations and also the number of bytes they may
  require. _May require_ here really means an upper bound,
  because `(MAKE-MAT (EXPT 2 60))` doesn't actually uses memory until
  one of its facets is accessed (don't simply evaluate it though,
  printing the result will access the ARRAY facet if *PRINT-MAT*).
  Also, while facets today all require the same number of bytes, this
  may change in the future. This is a debugging tool, don't use it in
  production.

  ```cl-transcript
  (with-mat-counters (:count count :n-bytes n-bytes)
    (assert (= count 0))
    (assert (= n-bytes 0))
    (make-mat '(2 3) :ctype :double)
    (assert (= count 1))
    (assert (= n-bytes (* 2 3 8)))
    (with-mat-counters (:n-bytes n-bytes-1 :count count-1)
      (make-mat '7 :ctype :float)
      (assert (= count-1 1))
      (assert (= n-bytes-1 (* 7 4))))
    (assert (= n-bytes (+ (* 2 3 8) (* 7 4))))
    (assert (= count 2)))
  ```"
  (alexandria:with-unique-names (counter)
    `(let* ((,counter (list 0 0))
            (*counters* (cons ,counter *counters*)))
       (symbol-macrolet (,@(when n-bytes
                             `((,n-bytes (first ,counter))))
                         ,@(when count
                             `((,count (second ,counter)))))
         ,@body))))

(defun note-allocation (n)
  (loop for counter in *counters*
        do (incf (first counter) n)
           (incf (second counter))))


(defsection @mat-facet-api (:title "Facet API")
  ""
  (@mat-facets section)
  (@mat-foreign section)
  (@mat-cuda section))

(defsection @mat-facets (:title "Facets")
  "A MAT is a CUBE (see @CUBE-MANUAL) whose facets are different
  representations of numeric arrays. These facets can be accessed with
  WITH-FACETS with one of the following [FACET-NAME][locative]
  locatives:"
  (backing-array facet-name)
  (array facet-name)
  (foreign-array facet-name)
  (cuda-host-array facet-name)
  (cuda-array facet-name)
  "Facets bound by with WITH-FACETS are to be treated as dynamic
  extent: it is not allowed to keep a reference to them beyond the
  dynamic scope of WITH-FACETS.

  For example, to implement the FILL! operation using only the
  BACKING-ARRAY, one could do this:

  ```commonlisp
  (let ((displacement (mat-displacement x))
        (size (mat-size x)))
   (with-facets ((x* (x 'backing-array :direction :output)))
     (fill x* 1 :start displacement :end (+ displacement size))))
  ```
 
  DIRECTION is :OUTPUT because we clobber all values in `X`. Armed
  with this knowledge about the direction, WITH-FACETS will not copy
  data from another facet if the backing array is not up-to-date.

  To transpose a 2d matrix with the ARRAY facet:

  ```commonlisp
  (destructuring-bind (n-rows n-columns) (mat-dimensions x)
    (with-facets ((x* (x 'array :direction :io)))
      (dotimes (row n-rows)
        (dotimes (column n-columns)
          (setf (aref x* row column) (aref x* column row))))))
  ```

  Note that DIRECTION is :IO, because we need the data in this facet
  to be up-to-date (that's the input part) and we are invalidating all
  other facets by changing values (that's the output part).

  To sum the values of a matrix using the [FOREIGN-ARRAY][facet-name]
  facet:

  ```commonlisp
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

  We have finished our introduction to the various facets. It must be
  said though that one can do anything without ever accessing a facet
  directly or even being aware of them as most operations on `MAT`s
  take care of choosing the most appropriate facet behind the scenes.
  In particular, most operations automatically use CUDA, if available
  and initialized. See WITH-CUDA* for detail.")

(export 'with-facet)
(export 'with-facets)
(export 'facet-name)

(define-facet-name backing-array ()
  "The corresponding facet's value is a one dimensional lisp array or
  a static vector that also looks exactly like a lisp array but is
  allocated in foreign memory. See *FOREIGN-ARRAY-STRATEGY*.")

(define-facet-name array ()
  "Same as BACKING-ARRAY if the matrix is one-dimensional, all
  elements are visible (see @MAT-SHAPING), else it's a lisp array
  displaced to the backing array.")

(define-facet-name foreign-array ()
  "The facet's value is a [FOREIGN-ARRAY][class] which is an
  OFFSET-POINTER wrapping a CFFI pointer. See
  *FOREIGN-ARRAY-STRATEGY*.")

(define-facet-name cuda-host-array ()
  "This facet's value is a basically the same as that of
  [FOREIGN-ARRAY][facet-name]. In fact, they share storage. The
  difference is that accessing [CUDA-HOST-ARRAY][facet-name] ensures
  that the foreign memory region is page-locked and registered with
  the CUDA Driver API function cuMemHostRegister(). Copying between
  GPU memory ([CUDA-ARRAY][facet-name]) and registered memory is
  significantly faster than with non-registered memory and also allows
  overlapping copying with computation. See
  WITH-SYNCING-CUDA-FACETS.")

(define-facet-name cuda-array ()
  "The facet's value is a CUDA-ARRAY which is an OFFSET-POINTER
  wrapping a CL-CUDA.DRIVER-API:CU-DEVICE-PTR, allocated with
  CL-CUDA.DRIVER-API:CU-MEM-ALLOC and freed automatically.")

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

(defun vec-facet (facet)
  (cdr (facet-description facet)))

(defun vec-facet-name (facet)
  (facet-name (cdr (facet-description facet))))

(defun make-array-facet (mat vector)
  (if (and (zerop (mat-displacement mat))
           (= (mat-size mat) (mat-max-size mat))
           (= 1 (length (mat-dimensions mat))))
      vector
      (make-array (mat-dimensions mat)
                  :element-type (ctype->lisp (mat-ctype mat))
                  :displaced-to vector
                  :displaced-index-offset (mat-displacement mat))))

(defmethod make-facet* ((mat mat) (facet-name (eql 'array)))
  (let* ((vec (vec mat))
         (vec-facet
           (if (or (member *foreign-array-strategy* '(:static :cuda-host))
                   (find-facet vec 'static-vector))
               (add-facet-reference-by-name (vec mat) 'static-vector)
               (add-facet-reference-by-name (vec mat) 'lisp-vector))))
    ;; FACET-DESCRIPTION will be this cons. VEC is necessary for
    ;; DESTROY-FACET*, because it has no access to MAT.
    (values nil (cons vec vec-facet) nil)))

(defmethod make-facet* ((mat mat) (facet-name (eql 'backing-array)))
  (let* ((vec (vec mat))
         (vec-facet
           (if (or (member *foreign-array-strategy* '(:static :cuda-host))
                   (find-facet vec 'static-vector))
               (add-facet-reference-by-name (vec mat) 'static-vector)
               (add-facet-reference-by-name (vec mat) 'lisp-vector))))
    (values nil (cons vec vec-facet) nil)))

(defmethod make-facet* ((mat mat) (facet-name (eql 'foreign-array)))
  (let* ((vec (vec mat))
         (vec-facet (if (and (use-pinning-p)
                            (not (find-facet vec 'static-vector)))
                       (add-facet-reference-by-name vec 'lisp-vector)
                       (add-facet-reference-by-name vec 'static-vector))))
    (values nil (cons vec vec-facet) nil)))

(defmethod make-facet* ((mat mat) (facet-name (eql 'cuda-array)))
  (let* ((vec (vec mat))
         (vec-facet (add-facet-reference-by-name vec 'cuda-vector)))
    (values nil (cons vec vec-facet) nil)))

(defmethod make-facet* ((mat mat) (facet-name (eql 'cuda-host-array)))
  (let* ((vec (vec mat))
         (vec-facet (add-facet-reference-by-name vec 'static-vector))
         (static-vector (facet-value vec-facet)))
    (values (register-cuda-host-array (static-vector-pointer static-vector)
                                      (vec-n-bytes vec))
            (cons vec vec-facet)
            ;; Ask for a finalizer, because we need to unregister
            ;; memory.
            t)))

(defmethod call-with-facet* ((mat mat) (facet-name (eql 'array))
                             direction fn)
  ;; CALL-NEXT-METHOD ensures that the ARRAY facets exists.
  (call-next-method
   mat facet-name direction
   ;; Delegate to VEC.
   (lambda (facet)
     (declare (ignore facet))
     (let* ((vec (vec mat))
            (facet (find-facet mat 'array))
            (vec-facet (vec-facet facet))
            (vec-facet-name (facet-name vec-facet))
            ;; Create the array lazily (this might be right after
            ;; MAKE-FACET*, or FACET-VALUE was NIL in
            ;; RESHAPE-AND-DISPLACE-FACET*.
            (facet (or (facet-value facet)
                       (setf (facet-value facet)
                             (make-array-facet mat (facet-value vec-facet))))))
       ;; We could call fn with FACET directly, but doing it via
       ;; CALL-WITH-FACET* enables VEC to detect reader/writer
       ;; conflicts.
       (call-with-facet* vec vec-facet-name direction
                         (lambda (vec-facet)
                           (declare (ignore vec-facet))
                           (funcall fn facet)))))))

(defun maybe-rewire-to (mat vec facet-name vec-facet-name)
  (declare (ignore vec vec-facet-name))
  (let ((vec-facet (vec-facet (find-facet mat facet-name))))
    ;; FIXME: we should actually change to VEC-FACET-NAME if exists
    vec-facet))

;;; Just use STATIC-VECTOR or LISP-VECTOR directly.
(defmethod call-with-facet* ((mat mat) (facet-name (eql 'backing-array))
                             direction fn)
  (call-next-method
   mat facet-name direction
   (lambda (facet)
     (declare (ignore facet))
     (let* ((vec (vec mat))
            (vec-facet-name (facet-name
                             (maybe-rewire-to mat vec
                                              facet-name 'static-vector))))
       (call-with-facet* vec vec-facet-name direction fn)))))

;;; Use the pinned LISP-VECTOR if there is no STATIC-VECTOR facet and
;;; pinning is supported, else use STATIC-VECTOR.
(defmethod call-with-facet* ((mat mat) (facet-name (eql 'foreign-array))
                             direction fn)
  (call-next-method
   mat facet-name direction
   (lambda (facet)
     (declare (ignore facet))
     (let* ((vec (vec mat))
            (vec-facet-name (facet-name
                             (maybe-rewire-to mat vec
                                              facet-name 'static-vector))))
       (call-with-facet*
        vec vec-facet-name direction
        (lambda (facet)
          (if (eq facet-name 'lisp-vector)
              (lla::with-pinned-array (lisp-pointer facet)
                (funcall fn (make-instance
                             'foreign-array
                             :base-pointer lisp-pointer
                             :offset (displacement-bytes mat))))
              (funcall fn (make-instance
                           'foreign-array
                           :base-pointer (static-vector-pointer
                                          facet)
                           :offset (displacement-bytes mat))))))))))

(defmethod call-with-facet* ((mat mat) (facet-name (eql 'cuda-array))
                             direction fn)
  (call-next-method
   mat facet-name direction
   (lambda (cuda-array)
     (let* ((vec (vec mat))
            (vec-facet-name (facet-name
                             (maybe-rewire-to mat vec
                                              facet-name 'static-vector))))
       (call-with-facet*
        vec vec-facet-name direction
        (lambda (cuda-vector)
          (unless cuda-array
            (let ((facet (find-facet mat 'cuda-array)))
              (setq cuda-array
                    (make-instance 'cuda-array
                                   :base-pointer (base-pointer cuda-vector)
                                   :offset (displacement-bytes mat)))
              (setf (facet-value facet) cuda-array)))
          (funcall fn cuda-array)))))))

(defmethod call-with-facet* ((mat mat) (facet-name (eql 'cuda-host-array))
                             direction fn)
  (call-next-method
   mat facet-name direction
   (lambda (foreign-array)
     (let* ((vec (vec mat))
            (vec-facet-name (vec-facet-name (find-facet mat facet-name))))
       (call-with-facet*
        vec vec-facet-name direction
        (lambda (static-vector)
          (assert (cffi:pointer-eq (static-vector-pointer static-vector)
                                   (base-pointer foreign-array)))
          (funcall fn foreign-array)))))))

(defmethod copy-facet* ((mat mat) from-facet-name from-facet
                        to-facet-name to-facet)
  (let ((vec-from-facet (vec-facet from-facet))
        (vec-to-facet (vec-facet to-facet)))
    (copy-facet* (vec mat)
                 (facet-name vec-from-facet) vec-from-facet
                 (facet-name vec-to-facet) vec-to-facet)))

(defmethod destroy-facet* ((facet-name (eql 'array)) facet)
  (destructuring-bind (vec . vec-facet) (facet-description facet)
    (remove-facet-reference vec-facet)
    (destroy-facet vec (facet-name vec-facet))))

(defmethod destroy-facet* ((facet-name (eql 'backing-array)) facet)
  (destructuring-bind (vec . vec-facet) (facet-description facet)
    (remove-facet-reference vec-facet)
    (destroy-facet vec (facet-name vec-facet))))

(defmethod destroy-facet* ((facet-name (eql 'foreign-array)) facet)
  (destructuring-bind (vec . vec-facet) (facet-description facet)
    (remove-facet-reference vec-facet)
    (destroy-facet vec (facet-name vec-facet))))

(defmethod destroy-facet* ((facet-name (eql 'cuda-array)) facet)
  (destructuring-bind (vec . vec-facet) (facet-description facet)
    (remove-facet-reference vec-facet)
    (destroy-facet vec (facet-name vec-facet))))

(defmethod destroy-facet* ((facet-name (eql 'cuda-host-array)) facet)
  (destructuring-bind (vec . vec-facet) (facet-description facet)
    (unregister-cuda-host-array
     (facet-value facet) (lambda ()
                           (remove-facet-reference vec-facet)
                           (destroy-facet vec (facet-name vec-facet))))))

(defun displacement-bytes (mat)
  (* (mat-displacement mat) (ctype-size (mat-ctype mat))))

;;; The BACKING-ARRAY, CUDA-HOST-ARRAY are not displaced and
;;; FOREIGN-ARRAY creates and OFFSET-POINTER on the fly.
(defmethod reshape-and-displace-facet* ((mat mat) facet-name facet
                                        dimensions displacement)
  (declare (ignore facet-name facet dimensions displacement)))

(defmethod reshape-and-displace-facet* ((mat mat) (facet-name (eql 'array))
                                        facet dimensions displacement)
  (declare (ignore dimensions displacement))
  (setf (facet-value facet) nil))

(defmethod reshape-and-displace-facet* ((mat mat) (facet-name (eql 'cuda-array))
                                        facet dimensions displacement)
  (declare (ignore dimensions displacement))
  (setf (facet-value facet) nil))

(defmethod select-copy-source-for-facet* ((mat mat) (to-name (eql 'cuda-array))
                                          facet)
  (declare (ignore facet))
  (if (eq *foreign-array-strategy* :cuda-host)
      'cuda-host-array
      (call-next-method)))

(defmethod facet-up-to-date-p* ((mat mat) facet-name facet)
  (facet-up-to-date-p (cdr (facet-description facet))))


(defsection @mat-extensions (:title "Writing Extensions")
  "New operations are usually implemented in lisp, CUDA, or by calling
  a foreign function in, for instance, BLAS, CUBLAS, CURAND."
  (@mat-lisp-extensions section)
  (@mat-cuda-extensions section))
