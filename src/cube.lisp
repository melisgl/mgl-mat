(in-package :mgl-cube)

(defsection @cube-manual (:title "Cube Manual")
  (@cube-links section)
  (@cube-introduction section)
  (@cube-basics section)
  (@cube-synchronization section)
  (@cube-facets section)
  (@cube-facet-extension-api section)
  (@cube-default-call-with-facet* section)
  (@cube-lifetime section))

(defsection @cube-links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-mat) and the [HTML
  documentation](http://melisgl.github.io/mgl-mat/cube-manual.html)
  for the latest version.")

(defsection @cube-introduction (:title "Introduction")
  "This is the library on which \\MGL-MAT (see MGL-MAT:@MAT-MANUAL) is
  built. The idea of automatically translating between various
  representations may be useful for other applications, so this got
  its own package and all ties to \\MGL-MAT has been severed.

  This package defines CUBE, an abstract base class that provides a
  framework for automatic conversion between various representations
  of the same data. To define a cube, CUBE needs to be subclassed and
  the @CUBE-FACET-EXTENSION-API be implemented.

  If you are only interested in how to use cubes in general, read
  @CUBE-BASICS, @CUBE-LIFETIME and @CUBE-FACET-BARRIER.

  If you want to implement a new cube datatype, then see @CUBE-FACETS,
  @CUBE-FACET-EXTENSION-API, and @CUBE-DEFAULT-CALL-WITH-FACET*.")

;;;; Utilities

(defmacro compare-and-swap (place old-value new-value)
  #+sbcl
  (alexandria:with-unique-names (old-val-var)
    `(let ((,old-val-var ,old-value))
       (eq ,old-val-var (sb-ext:compare-and-swap ,place ,old-val-var
                                                 ,new-value))))
  #+ccl
  `(ccl::conditional-store ,place ,old-value ,new-value)
  #+lispworks
  `(system:compare-and-swap ,place ,old-value ,new-value)
  #+allegro
  `(excl:atomic-conditional-setf ,place ,new-value ,old-value)
  #-(or sbcl ccl lispworks allegro)
  (progn
    (format t "WARNING: Using UNSAFE kludge for COMPARE-AND-SWAP.~%")
    `(progn ,old-value (setf ,place ,new-value) t)))

(defmacro without-interrupts (&body body)
  #+sbcl
  `(sb-sys:without-interrupts ,@body)
  #+cmucl
  `(system:without-interrupts ,@body)
  #+allegro
  `(excl:with-delayed-interrupts ,@body)
  #+scl
  `(sys:without-interrupts ,@body)
  #+ccl
  `(ccl:without-interrupts ,@body)
  #+lispworks
  `(mp:without-interrupts ,@body)
  #-(or sbcl cmucl allegro scl scl ccl lispworks)
  `(progn ,@body))


(defsection @cube-basics (:title "Basics")
  "Here we learn what a CUBE is and how to access the data in it with
  WITH-FACET."
  (cube class)
  (with-facet macro)
  (direction type)
  (with-facets macro))

(defsection @cube-synchronization (:title "Synchronization")
  "Cubes keep track of which facets are used, which are up-to-date to
  be able to perform automatic translation between facets. WITH-FACET
  and other operations access and make changes to this metadata so
  thread safety is a concern. In this section, we detail how to relax
  the default thread safety guarantees.

  A related concern is async signal safety which arises most often
  when C-c'ing or killing a thread or when the extremely nasty
  WITH-TIMEOUT macro is used. In a nutshell, changes to cube metadata
  are always made with interrupts disabled so things should be async
  signal safe."
  (synchronization (accessor cube))
  (*default-synchronization* variable)
  (*maybe-synchronize-cube* variable))

(defvar *default-synchronization* :maybe
  "The default value for SYNCHRONIZATION of new cubes.")

(defvar *maybe-synchronize-cube* t
  "Determines whether access the cube metadata is synchronized for
  cubes with SYNCHRONIZATION :MAYBE.")

(defclass cube ()
  ((synchronization
    :initform *default-synchronization*
    :type (member nil :maybe t)
    :initarg :synchronization
    :accessor synchronization
    :documentation "By default, setup and teardown of facets by
    WITH-FACET is performed in a thread safe way. Corrupting internal
    data structures of cubes is not fun, but in the name of
    performance, synchronization can be turned off either dynamically
    or on a per instance basis.

    If T, then access to cube metadata is always synchronized. If NIL,
    then never. If :MAYBE, then whether access is synchronized is
    determined by *MAYBE-SYNCHRONIZE-CUBE* that's true by default.

    The default is the value of *DEFAULT-SYNCHRONIZATION*
    that's :MAYBE by default.

    Note that the body of a WITH-FACET is never synchronized with
    anyone, apart from the implicit reader/writer conflict (see
    DIRECTION).")
   (lock :initform (bordeaux-threads:make-recursive-lock) :accessor lock)
   ;; This is the list of FACET objects of the cube with the twist
   ;; that there is an extra cons at the beginning whose identity
   ;; never changes. Finalizers - which cannot hold a reference to the
   ;; cube itself - hang on to this cons.
   (facets :initform (cons nil nil) :reader %facets)
   (has-finalizer-p :initform nil :accessor has-finalizer-p))
  (:documentation "A datacube that has various representations of the
  same stuff. These representations go by the name `facet'. Clients
  must use WITH-FACET to acquire a dynamic extent reference to a
  facet. With the information provided in the DIRECTION argument of
  WITH-FACET, the cube keeps track of which facets are up-to-date and
  copies data between them as necessary.

  The cube is an abstract class, it does not provide useful behavior
  in itself. One must subclass it and implement the
  @CUBE-FACET-EXTENSION-API.

  Also see @CUBE-LIFETIME and @CUBE-FACET-BARRIER."))

(defmacro with-cube-locked ((cube) &body body)
  (alexandria:with-gensyms (%cube)
    `(without-interrupts
       (let ((,%cube ,cube))
         (flet ((foo ()
                  ,@body))
           (declare (dynamic-extent #'foo))
           (if (synchronize-cube-p ,%cube)
               (bordeaux-threads:with-recursive-lock-held ((lock ,%cube))
                 (foo))
               (foo)))))))

(defun synchronize-cube-p (cube)
  (let ((synchronization (synchronization cube)))
    (or (and (eq synchronization :maybe)
             *maybe-synchronize-cube*)
        (eq synchronization t))))

(defmacro with-facet ((var (cube facet-name &key (direction :io) type))
                      &body body)
  "Find or create the facet with FACET-NAME in CUBE and bind VAR to
  the representation of CUBE's data provided by that facet. This
  representation is called the facet's _value_. The value is to be
  treated as dynamic extent: it is not allowed to keep a reference to
  it. For the description of the DIRECTION parameter, see the type
  DIRECTION.

  If TYPE is specified, then VAR is declared to be of that type."
  `(call-with-facet* ,cube ,facet-name ,direction
                     (lambda (,var)
                       ,@(when type `((declare (type ,type ,var))))
                       ,@body)))

(deftype direction ()
  "Used by WITH-FACET, DIRECTION can be :INPUT, :OUTPUT or :IO.

  - :INPUT promises that the facet will only be read and never
    written. Other up-to-date facets of the same cube remain
    up-to-date. If the facet in question is not up-to-date then data
    is copied to it from one of the up-to-date facets (see
    SELECT-COPY-SOURCE-FOR-FACET*).

  - :OUTPUT promises that _all_ data will be overwritten without
    reading any data. All up-to-date facets become non-up-to-date,
    while this facet is marked as up-to-date. No copying of data takes
    place.

  - :IO promises nothing about the type of access. All up-to-date
    facets become non-up-to-date, while this facet is marked as
    up-to-date. If the facet in question is not up-to-date then data
    is copied to it from one of the up-to-date facets (see
    SELECT-COPY-SOURCE-FOR-FACET*).

  Any number of `WITH-FACET`s with direction :INPUT may be active at
  the same time, but :IO and :OUTPUT cannot coexists with another
  WITH-FACET regardless of the direction. The exception for this rule
  is that an inner WITH-FACET does not conflict with an enclosing
  WITH-FACET if they are for the same facet (but inner `WITH-FACET`s
  for another facet or for the same facet from another thread do).

  See CHECK-NO-WRITERS and CHECK-NO-WATCHERS called by
  @CUBE-DEFAULT-CALL-WITH-FACET*."
  '(member :input :output :io))

(defun expand-with-facets (facet-binding-specs body)
  (if (endp facet-binding-specs)
      `(locally ,@body)
      `(with-facet ,(first facet-binding-specs)
         ,(expand-with-facets (rest facet-binding-specs) body))))

(defmacro with-facets ((&rest facet-binding-specs) &body body)
  "A shorthand for writing nested WITH-FACET calls.

  ```
  (with-facet (f1 (c1 'name1 :direction :input))
    (with-facet (f2 (c2 'name2 :direction :output))
      ...))
  ```

  is equivalent to:

  ```
  (with-facets ((f1 (c1 'name1 :direction :input))
                (f2 (c2 'name2 :direction :output)))
    ...)
  ```"
  (expand-with-facets facet-binding-specs body))


(defsection @cube-facets (:title "Facets")
  "The basic currency for implementing new cube types is the FACET.
  Simply using a cube only involves facet names and values, never
  facets themselves."
  (facets function)
  (find-facet function)
  (facet class)
  (facet-name structure-accessor)
  (facet-value structure-accessor)
  (facet-description structure-accessor)
  (facet-up-to-date-p structure-accessor)
  (facet-n-watchers structure-accessor)
  (facet-watcher-threads structure-accessor)
  (facet-direction structure-accessor))

(defun facets (cube)
  "Return the facets of CUBE."
  (cdr (%facets cube)))

(defun find-facet (cube facet-name)
  "Return the facet of CUBE for the facet with FACET-NAME or NIL if no
  such facet exists."
  (find facet-name (facets cube) :key #'facet-name))

(defstruct facet
  "A cube has facets, as we discussed in @CUBE-BASICS. Facets holds
  the data in a particular representation, this is called the _value_
  of the facet. A facet holds one such value and some metadata
  pertaining to it: its FACET-NAME, whether it's
  up-to-date (FACET-UP-TO-DATE-P), etc. FACET objects are never seen
  when simply using a cube, they are for implementing the
  @CUBE-FACET-EXTENSION-API."
  (name nil :type symbol)
  value
  description
  up-to-date-p
  (n-watchers 0)
  (watcher-threads ())
  (direction nil :type direction)
  (references (make-references)))

(defstruct references
  ;; This is basically the number of references callers of
  ;; ADD-FACET-REFERENCE and REMOVE-FACET-REFERENCE have totalled on
  ;; this facet. If it's non-zero, then this facet is protected
  ;; against DESTROY-FACET. Since we are at the mercy of the callers
  ;; of these functions, we must also make sure that finalizers
  ;; destroy the facet regardless of the number of references. When
  ;; the facet is about to be destroyed we CAS NIL onto the CAR of
  ;; this token.
  (n 0)
  (list ()))

(setf (documentation 'facet-name 'function)
      "A symbol that uniquely identifies the facet within a cube.")

(setf (documentation 'facet-value 'function)
      "This is what's normally exposed by WITH-FACET.")

(setf (documentation 'facet-description 'function)
      "Returned by MAKE-FACET* as its second value, this is an
      arbitrary object in which additional information can be
      stored.")

(setf (documentation 'facet-up-to-date-p 'function)
      "Whether the cube has changed since this facet has been last
      updated. See FACET-UP-TO-DATE-P*.")

(setf (documentation 'facet-n-watchers 'function)
      "The number of active `WITH-FACET`s. Updated by WATCH-FACET and
      UNWATCH-FACET.")

(setf (documentation 'facet-watcher-threads 'function)
      "The threads (one for each watcher) that have active
      `WITH-FACET`s.")

(setf (documentation 'facet-direction 'function)
      "The direction of the last WITH-FACET on this facet.")


(defsection @cube-facet-extension-api (:title "Facet Extension API")
  "Many of the generic functions in this section take FACET arguments.
  FACET is a structure and is not intended to be subclassed. To be
  able to add specialized methods, the name of the
  facet ([FACET-NAME][structure-accessor]) is also passed as the
  argument right in front of the corresponding facet argument.

  In summary, define EQL specializers on facet name arguments, and use
  FACET-DESCRIPTION to associate arbitrary information with facets."
  (make-facet* generic-function)
  (destroy-facet* generic-function)
  (copy-facet* generic-function)
  (call-with-facet* generic-function)
  (facet-up-to-date-p* generic-function)
  (select-copy-source-for-facet* generic-function)
  "PAX integration follows, don't worry about it if you don't use PAX,
  but you really should (see MGL-PAX::@MGL-PAX-MANUAL)."
  (facet-name locative)
  (define-facet-name macro)
  "Also see @CUBE-DEFAULT-CALL-WITH-FACET*.")

(defgeneric make-facet* (cube facet-name)
  (:documentation "Called by WITH-FACET (or more directly WATCH-FACET)
  when there is no facet with FACET-NAME. As the first value, return a
  new object capable of storing CUBE's data in the facet with
  FACET-NAME. As the second value, return a facet description which
  will be available as FACET-DESCRIPTION. As the third value, return a
  generalized boolean indicating whether this facet must be explicitly
  destroyed (in which case a finalizer will be added to CUBE)."))

(defgeneric destroy-facet* (facet-name facet)
  (:documentation "Free the resources associated with FACET with
  FACET-NAME. The cube this facet belongs to is not among the
  parameters because this method can be called from a finalizer on the
  cube (so we can't have a reference to the cube portably) which also
  means that it may run in an unpredictable thread."))

(defgeneric copy-facet* (cube from-facet-name from-facet
                         to-facet-name to-facet)
  (:documentation "Copy the CUBE's data from FROM-FACET with
  FROM-FACET-NAME to TO-FACET with TO-FACET-NAME. Called by
  WITH-FACET (or more directly WATCH-FACET) when necessary. FROM-FACET
  is what SELECT-COPY-SOURCE-FOR-FACET* returned."))

(defgeneric select-copy-source-for-facet* (cube to-name to-facet)
  (:documentation "Called when TO-FACET with TO-NAME is about to be
  updated by copying data from an up-to-date facet. Return the
  facet (or its name) from which data shall be copied. Note that if
  the returned facet is not FACET-UP-TO-DATE-P*, then it will be
  updated first and another SELECT-COPY-SOURCE-FOR-FACET* will take
  place, so be careful not to get into endless recursion. The default
  method simply returns the first up-to-date facet.")
  (:method (cube to-name to-facet)
    (declare (ignore to-name to-facet))
    (find-up-to-date-facet cube)))

(defgeneric facet-up-to-date-p* (cube facet-name facet)
  (:documentation "Check if FACET with FACET-NAME has been updated
  since the latest change to CUBE (that is, since the access to other
  facets with DIRECTION of :IO or :OUTPUT). The default method simply
  calls FACET-UP-TO-DATE-P on FACET.

  One reason to specialize this is when some facets actually share
  common storage, so updating one make the other up-to-date as well.")
  (:method (cube facet-name facet)
    (declare (ignore cube facet-name))
    (facet-up-to-date-p facet)))

(defgeneric call-with-facet* (cube facet-name direction fn)
  (:documentation "Call FN with an up-to-date FACET-VALUE that belongs
  to FACET-NAME of CUBE. WITH-FACET is directly implemented in terms
  of this function. See @CUBE-DEFAULT-CALL-WITH-FACET* for the gory
  details.

  Specializations will most likely want to call the default
  implementation (with CALL-NEXT-METHOD) but with a lambda that
  transforms FACET-VALUE before passing it on to FN."))

(define-symbol-locative-type facet-name ()
  "The FACET-NAME [locative][locative] is the to refer to stuff
  defined with DEFINE-FACET-NAME.")

(define-definer-for-symbol-locative-type define-facet-name facet-name
  "Just a macro to document that SYMBOL refers to a facet name (as in
  the [FACET-NAME][locative]). This is totally confusing, so here is
  an example of how \\MGL-MAT (see MGL-MAT:@MAT-MANUAL) documents the
  MGL-MAT:BACKING-ARRAY facet:

  ```commonlisp
  (define-facet-name backing-array ()
    \"The corresponding facet is a one dimensional lisp array.\")
  ```

  Which makes it possible to refer to this definition (refer as in
  link and `M-.` to) MGL-MAT:BACKING-ARRAY facet-name. See
  MGL-PAX:@MGL-PAX-MANUAL for more.")


(defsection @cube-default-call-with-facet*
    (:title "The Default Implementation of CALL-WITH-FACET*")
  (call-with-facet* (method () (cube t t t)))
  (watch-facet generic-function)
  (unwatch-facet generic-function)
  (*let-input-through-p* variable)
  (*let-output-through-p* variable)
  (check-no-writers function)
  (check-no-watchers function))

(defmethod call-with-facet* ((cube cube) facet-name direction fn)
  "The default implementation of CALL-WITH-FACET* is defined in terms
  of the WATCH-FACET and the UNWATCH-FACET generic functions. These
  can be considered part of the @CUBE-FACET-EXTENSION-API."
  ;; If WATCH-FACET fails, don't unwatch it. Also, disable interrupts
  ;; in an effort to prevent async unwinds (C-c and similar) from
  ;; leaving inconsistent state around.
  (let ((facet nil)
        (facet-watched-p nil))
    (unwind-protect
         (progn
           (with-cube-locked (cube)
             (setq facet (watch-facet cube facet-name direction))
             (setq facet-watched-p t))
           (funcall fn facet))
      ;; The first thing we do in the cleanup is a WITHOUT-INTERRUPTS
      ;; which should minimize the chance for races and may be
      ;; entirely free of races on a good, safepoint based
      ;; implementation.
      (when facet-watched-p
        (with-cube-locked (cube)
          (unwatch-facet cube facet-name))))))

(defgeneric watch-facet (cube facet-name direction)
  (:documentation "This is what the default CALL-WITH-FACET* method,
  in terms of which WITH-FACET is implemented, calls first. The
  default method takes care of creating facets, copying and tracking
  up-to-dateness.

  Calls CHECK-NO-WRITERS (unless *LET-INPUT-THROUGH-P*) and
  CHECK-NO-WATCHERS (unless *LET-OUTPUT-THROUGH-P*) depending on
  DIRECTION to detect situations with a writer being concurrent to
  readers/writers because that would screw up the tracking of
  up-to-dateness.

  The default implementation should suffice most of the time.
  \\MGL-MAT specializes it to override the DIRECTION arg, if
  it's :OUTPUT but not all elements are visible due to reshaping, so
  that invisible elements are still copied over.")
  (:method ((cube cube) facet-name direction)
    (check-type direction direction)
    (let ((facet (ensure-facet cube facet-name direction)))
      (when (and (not (eq direction :output))
                 (not (facet-up-to-date-p* cube facet-name facet))
                 (find-up-to-date-facet cube))
        (let* ((from-facet-or-name
                 (select-copy-source-for-facet* cube facet-name facet))
               (from-facet-name (if (symbolp from-facet-or-name)
                                    from-facet-or-name
                                    (facet-name from-facet-or-name))))
          ;; Make sure FROM-FACET is up-to-date. This may call
          ;; WATCH-FACET recursively.
          (with-facet (a (cube from-facet-name :direction :input))
            (declare (ignore a)))
          (let ((from-facet (find-facet cube from-facet-name)))
            (copy-facet* cube from-facet-name from-facet facet-name facet))))
      (unless (eq direction :input)
        (dolist (facet (facets cube))
          (setf (facet-up-to-date-p facet) nil)))
      (setf (facet-up-to-date-p facet) t)
      (incf (facet-n-watchers facet))
      (push (bordeaux-threads:current-thread) (facet-watcher-threads facet))
      (facet-value facet))))

(defgeneric unwatch-facet (cube facet-name)
  (:documentation "This is what the default CALL-WITH-FACET* method,
  in terms of which WITH-FACET is implemented, calls last. The default
  method takes care of taking down facets. External resource managers
  may want to hook into this to handle unused facets.")
  (:method ((cube cube) facet-name)
    (let ((facet (find-facet cube facet-name)))
      (decf (facet-n-watchers facet))
      (setf (facet-watcher-threads facet)
            (delete (bordeaux-threads:current-thread)
                    (facet-watcher-threads facet)
                    :count 1))
      (assert (<= 0 (facet-n-watchers facet))))))

(defvar *let-input-through-p* nil
  "If true, WITH-FACETS (more precisely, the default implementation of
  CALL-WITH-FACET*) with :DIRECTION :INPUT does not call
  CHECK-NO-WRITERS. This knob is intended to be bound locally for
  debugging purposes.")

(defvar *let-output-through-p* nil
  "If true, WITH-FACETS (more precisely, the default implementation of
  CALL-WITH-FACET*) with :DIRECTION :IO or :OUTPUT does not call
  CHECK-NO-WATCHERS. This knob is intended to be bound locally for
  debugging purposes.")

(defun check-no-writers (cube facet-name message-format &rest message-args)
  "Signal an error if CUBE has facets (with names other than
  FACET-NAME) being written (i.e. direction is :IO or :OUTPUT)."
  (declare (optimize speed)
           (dynamic-extent message-args))
  (assert (every (lambda (facet)
                   (or (eq (facet-name facet) facet-name)
                       (not (has-writers-p facet))))
                 (facets cube))
          () "~@<~? ~:_because ~S ~:_has active writers. ~:_~
             If you are sure that this is a false ~
             alarm, ~:_then consider binding ~:_~
             MGL-CUBE:*LET-INPUT-THROUGH-P* to true.~:@>"
          message-format message-args cube))

(defun check-no-watchers (cube facet-name message-format &rest message-args)
  "Signal an error if CUBE has facets (with names other than
  FACET-NAME) being regardless of the direction."
  (declare (optimize speed)
           (dynamic-extent message-args))
  (assert (every (lambda (facet)
                   (or (eq (facet-name facet) facet-name)
                       (not (has-watchers-p facet))))
                 (facets cube))
          () "~@<~? ~:_because ~S ~:_has active facets. ~:_~
             If you are sure that this is a false ~
             alarm, ~:_then consider binding ~:_~
             MGL-CUBE:*LET-OUTPUT-THROUGH-P* to true.~:@>"
          message-format message-args cube))


(defsection @cube-lifetime (:title "Lifetime")
  "Lifetime management of facets is manual (but facets of garbage
  cubes are freed automatically by a finalizer, see MAKE-FACET*). One
  may destroy a single facet or all facets of a cube with
  DESTROY-FACET and DESTROY-CUBE, respectively. Also see
  @CUBE-FACET-BARRIER."
  (destroy-facet function)
  (destroy-cube function)
  "In some cases it is useful to declare the intent to use a facet in
  the future to prevent its destruction. Hence, every facet has
  reference count which starts from 0. The reference count is
  incremented and decremented by ADD-FACET-REFERENCE-BY-NAME and
  REMOVE-FACET-REFERENCE-BY-NAME, respectively. If it is positive,
  then the facet will not be destroyed by explicit DESTROY-FACET and
  DESTROY-CUBE calls, but it will still be destroyed by the finalizer
  to prevent resource leaks caused by stray references."
  (add-facet-reference-by-name function)
  (remove-facet-reference-by-name function)
  (remove-facet-reference function)
  (@cube-facet-barrier section))

(defun destroy-facet (cube facet-name)
  "Free resources associated with the facet with FACET-NAME and remove
  it from FACETS of CUBE."
  (let ((facet nil))
    (with-cube-locked (cube)
      (check-no-watchers cube nil "Cannot remove facet ~S" facet-name)
      (let ((v (find-facet cube facet-name)))
        (when (and v (get-permission-to-destroy (facet-references v)))
          (setf (cdr (slot-value cube 'facets)) (remove v (facets cube)))
          (deregister-cube-facet cube facet-name)
          (setq facet v))))
    (when facet
      (destroy-facet* facet-name facet)
      (setf (facet-value facet) nil)
      (setf (facet-description facet) nil)
      t)))

(defun destroy-cube (cube)
  "Destroy all facets of CUBE with DESTROY-FACET."
  (loop for facet = (first (facets cube))
        while facet
        do (destroy-facet cube (facet-name facet))))

(defun add-facet-reference-by-name (cube facet-name)
  "Make sure FACET-NAME exists on CUBE and increment its reference
  count. Return the FACET behind FACET-NAME."
  ;; Keep retrying if the facet gets destroyed before the reference
  ;; count is incremented.
  (loop
    (let ((facet (with-cube-locked (cube)
                   (ensure-facet cube facet-name nil))))
      (when (incf-references (facet-references facet) 1)
        (return facet)))))

(defun remove-facet-reference-by-name (cube facet-name)
  "Decrement the reference count of the facet with FACET-NAME of CUBE.
  It is an error if the facet does not exists or if the reference
  count becomes negative."
  (let ((facet (with-cube-locked (cube)
                 ;; This is under the lock only to prevent races with
                 ;; regards to facet creation.
                 (find-facet cube facet-name))))
    (assert facet)
    (assert (not (minusp (incf-references (facet-references facet)
                                          -1))))))

(defun remove-facet-reference (facet)
  "Decrement the reference count of FACET. It is an error if the facet
  is already destroyed or if the reference count becomes negative.
  This function has the same purpose as
  REMOVE-FACET-REFERENCE-BY-NAME, but by having a single FACET
  argument, it's more suited for use in finalizers because it does not
  keep the whole CUBE alive."
  (check-type facet facet)
  (let ((new-n-references (incf-references (facet-references facet) -1)))
    (assert new-n-references ()
            "Can't decrement reference count on a destroyed facet.")
    (assert (not (minusp new-n-references)) ()
            "Reference count became negative: ~S." new-n-references)
    new-n-references))


(defsection @cube-facet-barrier (:title "Facet Barriers")
  "A facility to control lifetime of facets tied to a dynamic extent.
  Also see @CUBE-LIFETIME."
  (with-facet-barrier macro)
  (count-barred-facets function))

(defstruct facet-barrier
  cube-type
  ensures
  destroys
  (cubes-to-barred-facets (tg:make-weak-hash-table :weakness :key)))

(defvar *facet-barriers* ())

(defmacro with-facet-barrier ((cube-type ensures destroys) &body body)
  "When BODY exits, destroy facets which:

  - are of cubes with CUBE-TYPE

  - have a facet name among DESTROYS

  - were created in the dynamic extent of BODY

  Before destroying the facets, it is ensured that facets with names
  among ENSURES are up-to-date. WITH-FACET-BARRIERs can be nested, in
  case of multiple barriers matching the cube's type and the created
  facet's name, the innermost one takes precedence.

  The purpose of this macro is twofold. First, it makes it easy to
  temporarily work with a certain facet of many cubes without leaving
  newly created facets around. Second, it can be used to make sure
  that facets whose extent is tied to some dynamic boundary (such as
  the thread in which they were created) are destroyed."
  (alexandria:with-unique-names (barrier)
    `(let* ((,barrier (make-facet-barrier :cube-type ,cube-type
                                          :ensures ,ensures
                                          :destroys ,destroys))
            (*facet-barriers* (cons ,barrier *facet-barriers*)))
       (unwind-protect
            (progn ,@body)
         (cleanup-facets ,barrier)))))

(defun facet-barred-p (barrier cube facet-name)
  (and (typep cube (facet-barrier-cube-type barrier))
       (member facet-name (facet-barrier-destroys barrier))))

(defun register-cube-facet (cube facet-name)
  (dolist (barrier *facet-barriers*)
    (when (facet-barred-p barrier cube facet-name)
      (let* ((cubes-to-barred-facets
               (facet-barrier-cubes-to-barred-facets barrier))
             (barred (gethash cube cubes-to-barred-facets)))
        (when (not (member facet-name barred))
          (push facet-name (gethash cube cubes-to-barred-facets))
          (return))))))

(defun deregister-cube-facet (cube facet-name)
  (dolist (barrier *facet-barriers*)
    (when (facet-barred-p barrier cube facet-name)
      (let* ((cubes-to-barred-facets
               (facet-barrier-cubes-to-barred-facets barrier))
             (barred (delete facet-name (gethash cube cubes-to-barred-facets))))
        (if barred
            (setf (gethash cube cubes-to-barred-facets) barred)
            (remhash cube cubes-to-barred-facets))))))

(defun cleanup-cube (cube ensures destroys)
  (when (and ensures
             (notany (lambda (facet-name)
                       (let ((facet (find-facet cube facet-name)))
                         (and facet
                              (facet-up-to-date-p* cube facet-name facet))))
                     ensures))
    (with-facet (facet (cube (first ensures) :direction :input))
      (declare (ignore facet))))
  (dolist (name destroys)
    (destroy-facet cube name)))

(defun cleanup-facets (barrier)
  (let ((ensures (facet-barrier-ensures barrier))
        (destroys (facet-barrier-destroys barrier)))
    (maphash (lambda (cube barred-facets)
               (declare (ignore barred-facets))
               ;; KLUDGE: in AllegroCL the weak hash table can contain
               ;; cubes for which the finalizer has run. The destroyed
               ;; facets are left around by the finalizer, and we can
               ;; run into an error here trying to copy data from a
               ;; destroyed facet.
               (#+allegro ignore-errors
                #-allegro progn
                (cleanup-cube cube ensures destroys)))
             (facet-barrier-cubes-to-barred-facets barrier))))

(defun count-barred-facets (facet-name &key (type 'cube))
  "Count facets with FACET-NAME of cubes of TYPE which will be
destroyed by a facet barrier."
  (let ((n 0))
    (dolist (barrier *facet-barriers*)
      (maphash (lambda (cube barred-facets)
                 (when (and (typep cube type)
                            (member facet-name barred-facets))
                   (incf n)))
               (facet-barrier-cubes-to-barred-facets barrier)))
    n))


;;;; Implementation 

(defun get-permission-to-destroy (references &key being-finalized)
  (loop
    (let ((n-references (references-n references)))
      (when (null n-references)
        ;; already destroyed
        (return nil))
      (when (and (not being-finalized) (plusp n-references))
        (return nil))
      ;; The remaining references if any must belong to a garbage
      ;; object.
      (when (compare-and-swap (references-n references) n-references nil)
        (return t)))))

(defun incf-references (references delta)
  (loop
    (let ((n-references (references-n references)))
      (when (null n-references)
        ;; already destroyed
        (return nil))
      (when (compare-and-swap (references-n references) n-references
                              (+ n-references delta))
        (return (+ n-references delta))))))

(defun ensure-cube-finalized (cube)
  (unless (has-finalizer-p cube)
    (setf (has-finalizer-p cube) t)
    (let ((facets (%facets cube)))
      (tg:finalize cube
                   (lambda ()
                     (dolist (facet (cdr facets))
                       (when (get-permission-to-destroy
                              (facet-references facet)
                              :being-finalized t)
                         (destroy-facet* (facet-name facet) facet)
                         (setf (facet-value facet) nil)
                         (setf (facet-description facet) nil))))))))

(defun add-facet (cube name value description direction)
  (register-cube-facet cube name)
  (let ((facet (make-facet :name name :value value :description description
                           :direction direction)))
    (push facet (cdr (slot-value cube 'facets)))
    facet))

(defun has-watchers-p (facet)
  (plusp (facet-n-watchers facet)))

(defun has-writers-p (facet)
  (and (has-watchers-p facet)
       (not (eq :input (facet-direction facet)))))

;;; caller must hold CUBE locked
(defun ensure-facet (cube facet-name direction)
  (let ((facet (find-facet cube facet-name)))
    ;; First check that there are no conflicting facets for other
    ;; facets.
    (cond ((eq direction :input)
           (unless *let-input-through-p*
             (check-no-writers cube facet-name
                               "Cannot create facet for ~S in direction ~S"
                               facet-name direction)))
          (direction
           (unless *let-output-through-p*
             (check-no-watchers cube facet-name
                                "Cannot create facet for ~S in direction ~S"
                                facet-name direction))))
    (cond ((null facet)
           (multiple-value-bind (facet facet-description must-be-destroyed-p)
               (make-facet* cube facet-name)
             (when must-be-destroyed-p
               (ensure-cube-finalized cube))
             (add-facet cube facet-name facet facet-description
                       (or direction :input))))
          (direction
           (let ((watchers (facet-watcher-threads facet))
                 (facet-direction (facet-direction facet)))
             (cond
               ;; If there are no other watchers, we can just reuse
               ;; FACET since there are no conflicting facets either.
               ((endp watchers)
                (setf (facet-direction facet) direction))
               ;; There are watchers but they are :INPUT and we also
               ;; want to create an :INPUT facet. Nothing to do.
               ((and watchers
                     (eq direction :input)
                     (eq facet-direction :input)))
               ;; There are watchers, and at least one of DIRECTION
               ;; and FACET-DIRECTION is :IO or :OUTPUT so we have a
               ;; reader/writer conflict. Still, let the facet be
               ;; shared if the only watcher is the current thread.
               ((and (every (let ((current-thread
                                    (bordeaux-threads:current-thread)))
                              (lambda (watcher)
                                (eq watcher current-thread)))
                            watchers))
                ;; Make sure FACET-DIRECTION is not :INPUT (it doesn't
                ;; matter whether it's :IO or :OUTPUT).
                (setf (facet-direction facet) :io))
               ;; There are no conflicting facets, but there are
               ;; watchers, FACET-DIRECTION is not :INPUT, and other
               ;; threads are watching FACET.
               ((eq direction :input)
                (unless *let-input-through-p*
                  (error "Cannot create nested facet for ~S in direction ~S ~
                      because there are other threads writing the same ~
                      facet." facet-name direction))
                (setf (facet-direction facet) :io))
               ;; There are no conflicting facets, but there are
               ;; watchers, FACET-DIRECTION may be :INPUT, and other
               ;; threads are watching FACET.
               (t
                (unless *let-output-through-p*
                  (error "Cannot create nested facet for ~S in direction ~S ~
                      because there are other threads using the same ~
                      facet." facet-name direction))))
             facet))
          (t facet))))

(defun find-up-to-date-facet (cube)
  (find-if (lambda (facet)
             (facet-up-to-date-p* cube (facet-name facet) facet))
           (facets cube)))
