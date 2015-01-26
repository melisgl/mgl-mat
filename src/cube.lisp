(in-package :mgl-cube)

(defsection @cube-manual (:title "Cube Manual")
  (@cube-introduction section)
  (@cube-basics section)
  (@cube-synchronization section)
  (@facet-extension-api section)
  (@default-call-with-facet* section)
  (@cube-views section)
  (@destruction-of-cubes section)
  (@facet-barrier section))

(defsection @cube-introduction (:title "Introduction")
  "This is the libray on which \\MGL-MAT (see MGL-MAT:@MAT-MANUAL) is
  built. The idea of automatically translating between various
  representations may be useful for other applications, so this got
  its own package and all ties to \\MGL-MAT has been severed.

  This package defines CUBE, an abstract base class that provides a
  framework for automatic conversion between various representations
  of the same data. To define a cube, CUBE needs to be subclassed and
  the @FACET-EXTENSION-API be implemented.

  If you are only interested in how to use cubes in general, read
  @CUBE-BASICS, @DESTRUCTION-OF-CUBES and @FACET-BARRIER.

  If you want to implement a new cube datatype, then see
  @FACET-EXTENSION-API, @DEFAULT-CALL-WITH-FACET* and @CUBE-VIEWS.")

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
  #-(or allegro lispworks ccl sbcl)
  (error "Not supported."))

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
    :documentation "By default setup and teardown of facets by
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
   ;; This is the list of VIEW objects of the cube with the twist that
   ;; there is an extra cons at the beginning whose identity never
   ;; changes. Finalizers - which cannot hold a reference to the cube
   ;; itself - hang on to this cons.
   (views :initform (cons nil nil) :reader %views)
   (has-finalizer-p :initform nil :accessor has-finalizer-p))
  (:documentation "A datacube that has various representations of the
  same stuff. These representations go by the name `facet'. Clients
  must use WITH-FACET to acquire a dynamic extent reference to a
  facet. With the information provided in the DIRECTION argument of
  WITH-FACET, the cube keeps track of which facets are up-to-date and
  copies data between them as necessary.

  The cube is an abstract class, it does not provide useful behavior
  in itself. One must subclass it and implement the
  @FACET-EXTENSION-API.

  Also see @CUBE-VIEWS, @DESTRUCTION-OF-CUBES and @FACET-BARRIER."))

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

(defmacro with-facet ((facet (cube facet-name &key (direction :io) type))
                      &body body)
  "Bind the variable FACET to the facet with FACET-NAME of CUBE. FACET
  is to be treated as dynamic extent: it is not allowed to keep a
  reference to it. For the description of the DIRECTION parameter, see
  the type DIRECTION."
  `(call-with-facet* ,cube ,facet-name ,direction
                     (lambda (,facet)
                       ,@(when type `((declare (type ,type ,facet))))
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

  Any number of [WITH-FACET][]s with direction :INPUT may be active at
  the same time, but :IO and :OUTPUT cannot coexists with any other
  WITH-FACET regardless of the direction. An exception is made for
  nested [WITH-FACET][]s for the same facet: an enclosing WITH-FACET
  never conflicts with an inner WITH-FACET, but [WITH-FACET][]s for
  another facet or for the same facet but from another thread do.

  See CHECK-NO-WRITERS and CHECK-NO-WATCHERS called by
  @DEFAULT-CALL-WITH-FACET*."
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
    ...))
  ```"
  (expand-with-facets facet-binding-specs body))


(defsection @facet-extension-api (:title "Facet extension API")
  (facet-name locative)
  (define-facet-name macro)
  (make-facet* generic-function)
  (destroy-facet* generic-function)
  (copy-facet* generic-function)
  (call-with-facet* generic-function)
  (set-up-to-date-p* generic-function)
  (select-copy-source-for-facet* generic-function)
  "Also see @DEFAULT-CALL-WITH-FACET*.")

(define-symbol-locative-type facet-name ()
  "The FACET-NAME locative is to refer to stuff defined with
  DEFINE-FACET-NAME.")

(define-definer-for-symbol-locative-type define-facet-name facet-name
  "Just a macro to document the symbol FACET-NAME means a facet
  name (as in the [FACET-NAME][locative]).")

(defgeneric make-facet* (cube facet-name)
  (:documentation "As the first value, return a new object capable of
  storing CUBE's data in the facet with FACET-NAME. As the second
  value, return a facet description which is going to be passed to
  DESTROY-FACET*. As the third value, return a generalized boolean
  indicating whether this facet must be explicitly destroyed (in which
  case a finalizer will be added to CUBE). Called by WITH-FACET (or
  more directly WATCH-FACET) when there is no facet with
  FACET-NAME."))

(defgeneric destroy-facet* (facet-name facet facet-description)
  (:documentation "Destroy FACET that belongs to a facet with
  FACET-NAME and FACET-DESCRIPTION. The cube this facet belongs to is
  not among the parameters because this method can be called from a
  finalizer on the cube (so we can't have a reference to the cube
  portably) which also means that it may run in an unpredictable
  thread."))

(defgeneric copy-facet* (cube from-facet-name from-facet
                         to-facet-name to-facet)
  (:documentation "Copy the CUBE's data from FROM-FACET with
  FROM-FACET-NAME to TO-FACET with TO-FACET-NAME. Called by
  WITH-FACET (or more directly WATCH-FACET) when necessary. FROM-FACET
  is what SELECT-COPY-SOURCE-FOR-FACET* returned."))

(defgeneric select-copy-source-for-facet* (cube to-name to-facet)
  (:method (cube to-name to-facet)
    (declare (ignore to-name to-facet))
    (find-up-to-date-facet-name cube))
  (:documentation "Return a facet name selected from the up-to-date
  views from which CUBE's data should be copied to TO-FACET. The
  default method simply returns the first up-to-date view."))

(defgeneric set-up-to-date-p* (cube facet-name view value)
  (:documentation "Set the VIEW-UP-TO-DATE-P slot of VIEW to VALUE.
  The default implementation simply SETFs it. This being a generic
  function allows subclasses to ensure that certain facets which share
  storage are always up-to-date at the same time."))

(defgeneric call-with-facet* (cube facet-name direction fn)
  (:documentation "Ensure that the facet with FACET-NAME exists.
  Depending on DIRECTION and up-to-dateness, maybe copy data. Finally,
  call FN with the facet. The default implementation acquires the
  facet with WATCH-FACET, calls FN with it and finally calls
  UNWATCH-FACET. However, specializations are allowed to create only
  temporary, dynamic extent views without ever calling WATCH-FACET and
  UNWATCH-FACET."))


(defsection @default-call-with-facet*
    (:title "The default implementation of CALL-WITH-FACET*")
  (call-with-facet* (method () (cube t t t)))
  (watch-facet generic-function)
  (unwatch-facet generic-function)
  (*let-input-through-p* variable)
  (*let-output-through-p* variable)
  (check-no-writers function)
  (check-no-watchers function))

;;; This actually belongs @CUBE-VIEWS, but it's defined early so that
;;; accessors can be compiled efficiently.
(defstruct view
  "A cube has facets, as we discussed in @CUBE-BASICS. The object
  which holds the data in a particular representation is the facet. A
  VIEW holds one such facet and some metadata pertaining to it: its
  name (VIEW-FACET-NAME), whether it's up-to-date (VIEW-UP-TO-DATE-P),
  etc. VIEW ojbects are never seen when simply using a cube, they are
  for implementing the @FACET-EXTENSION-API."
  facet-name
  facet
  facet-description
  up-to-date-p
  (n-watchers 0)
  (watcher-threads ())
  (direction nil :type direction)
  ;; Destruction can be initiated by finalizers running in other
  ;; threads, and also by both a FACET-BARRIER and a finalizer on some
  ;; platforms. Hence, we CAS T onto the CAR of this token before
  ;; destroying a facet.
  (permission-to-destroy (cons nil nil)))

(defmethod call-with-facet* ((cube cube) facet-name direction fn)
  "The default implementation of CALL-WITH-FACET* is defined in terms
  of the WATCH-FACET and the UNWATCH-FACET generic functions. These
  can be considered part of the @FACET-EXTENSION-API."
  ;; If WATCH-FACET fails, don't unwatch it. Also, disable interrupts
  ;; in an effort to prevent async unwinds (C-c and similar) from
  ;; leaving inconsistent state around.
  (let ((facet nil))
    (unwind-protect
         (progn
           (with-cube-locked (cube)
             (setq facet (watch-facet cube facet-name direction)))
           (funcall fn facet))
      ;; The first thing we do in the cleanup is a WITHOUT-INTERRUPTS
      ;; which should minimize the chance for races and may be
      ;; entirely free of races on a good, safepoint base
      ;; implementation.
      (with-cube-locked (cube)
        (when facet
          (unwatch-facet cube facet-name))))))

(defgeneric watch-facet (cube facet-name direction)
  (:method ((cube cube) facet-name direction)
    (check-type direction direction)
    (let* ((view (ensure-view cube facet-name direction))
           (facet (view-facet view)))
      (when (and (not (eq direction :output))
                 (not (view-up-to-date-p view))
                 (find-up-to-date-view cube))
        (let ((from (select-copy-source-for-facet* cube facet-name facet)))
          (copy-facet* cube from (view-facet (find-view cube from))
                       facet-name facet)))
      (unless (eq direction :input)
        (dolist (view (views cube))
          (set-up-to-date-p* cube (view-facet-name view) view nil)))
      (set-up-to-date-p* cube facet-name view t)
      (incf (view-n-watchers view))
      (push (bordeaux-threads:current-thread) (view-watcher-threads view))
      (view-facet view)))
  (:documentation "This is what the default CALL-WITH-FACET* method,
  in terms of which WITH-FACET is implemented, calls first. The
  default method takes care of creating views, copying and tracking
  up-to-dateness.

  Calls CHECK-NO-WRITERS (unless *LET-INPUT-THROUGH-P*) and
  CHECK-NO-WATCHERS (unless *LET-OUTPUT-THROUGH-P*) depending on
  DIRECTION to detect situations with a writer being concurrent to
  readers/writers because that would screw up the tracking
  up-to-dateness.

  The default implementation should suffice most of the time. \\MGL-MAT
  specializes it to override the DIRECTION arg if it's :OUTPUT but not
  all elements are visible due to reshaping."))

(defgeneric unwatch-facet (cube facet-name)
  (:method ((cube cube) facet-name)
    (let ((view (find-view cube facet-name)))
      (decf (view-n-watchers view))
      (setf (view-watcher-threads view)
            (delete (bordeaux-threads:current-thread)
                    (view-watcher-threads view)
                    :count 1))
      (assert (<= 0 (view-n-watchers view)))))
  (:documentation "This is what the default CALL-WITH-FACET* method,
  in terms of which WITH-FACET is implemented, calls last. The default
  method takes care of taking down views. External resource managers
  may want to hook into this to handle unused facets."))

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
  (assert (every (lambda (view)
                   (or (eq (view-facet-name view) facet-name)
                       (not (has-writers-p view))))
                 (views cube))
          () "~A because ~
          ~S has active writers. If you are sure that this is a false ~
          alarm then consider binding MGL-CUBE:*LET-INPUT-THROUGH-P* to ~
          true."
          (apply #'format nil message-format message-args) cube))

(defun check-no-watchers (cube facet-name message-format &rest message-args)
  "Signal an error if CUBE has facets (with names other than
  FACET-NAME) being regardless of the direction."
  (assert (every (lambda (view)
                   (or (eq (view-facet-name view) facet-name)
                       (not (has-watchers-p view))))
                 (views cube))
          () "~A because ~
           ~S has active views. If you are sure that this is a false ~
           alarm then consider binding MGL-CUBE:*LET-OUTPUT-THROUGH-P* to ~
           true."
          (apply #'format nil message-format message-args) cube))


(defsection @cube-views (:title "Views")
  "We learn what a VIEW is, how it's related to facets. See VIEWS,
  FIND-VIEW, and the default method of SET-UP-TO-DATE-P*. Views are
  only visible to those implementing the @FACET-EXTENSION-API."
  (view class)
  (view-facet-name structure-accessor)
  (view-facet structure-accessor)
  (view-facet-description structure-accessor)
  (view-up-to-date-p structure-accessor)
  (views function)
  (find-view function))

(defun views (cube)
  "Return the views of CUBE."
  (cdr (%views cube)))

(defun find-view (cube facet-name)
  "Return the view of CUBE for the facet with FACET-NAME or NIL if no
  such view exists."
  (find facet-name (views cube) :key #'view-facet-name))

(defmethod set-up-to-date-p* (cube facet-name view value)
  (declare (ignore cube facet-name))
  (setf (view-up-to-date-p view) value))


(defsection @destruction-of-cubes (:title "Destroying cubes")
  "Lifetime management of facets is manual (but facets of garbage
  cubes are freed automatically by a finalizer, see MAKE-FACET*). One
  may destroy a single facet or all facets of a cube with
  DESTROY-FACET and DESTROY-CUBE, respectively. Also see
  @FACET-BARRIER."
  (destroy-facet function)
  (destroy-cube function))

(defun destroy-facet (cube facet-name)
  "Free resources associated with the facet with FACET-NAME and remove
  it from VIEWS of CUBE."
  (let ((view (remove-view cube facet-name)))
    (when (and view
               (get-permission-to-destroy (view-permission-to-destroy view)))
      (destroy-facet* facet-name (view-facet view)
                      (view-facet-description view))
      (setf (view-facet view) nil))))

(defun destroy-cube (cube)
  "Destroy all facets of CUBE with DESTROY-FACET."
  (loop for view = (first (views cube))
        while view
        do (destroy-facet cube (view-facet-name view))))


(defsection @facet-barrier (:title "Facet barriers")
  "A facility to control lifetime of facets tied to a dynamic extent.
  Also see @DESTRUCTION-OF-CUBES."
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
  (when (and ensures (notany (lambda (facet-name)
                               (let ((view (find-view cube facet-name)))
                                 (and view (view-up-to-date-p view))))
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
               ;; views are left around by the finalizer, and we can
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

(defun get-permission-to-destroy (permission)
  (loop
    (when (car permission)
      (return nil))
    (when (compare-and-swap (car permission) nil t)
      (return t))))

(defun ensure-cube-finalized (cube)
  (unless (has-finalizer-p cube)
    (setf (has-finalizer-p cube) t)
    (let ((views (%views cube)))
      (tg:finalize cube
                   (lambda ()
                     (dolist (view (cdr views))
                       (when (get-permission-to-destroy
                              (view-permission-to-destroy view))
                         (destroy-facet* (view-facet-name view)
                                         (view-facet view)
                                         (view-facet-description view))
                         (setf (view-facet view) nil))))))))

(defun add-view (cube facet-name facet facet-description direction)
  (register-cube-facet cube facet-name)
  (let ((view (make-view :facet-name facet-name :facet facet
                         :facet-description facet-description
                         :direction direction)))
    (push view (cdr (slot-value cube 'views)))
    view))

(defun remove-view (cube facet-name)
  (with-cube-locked (cube)
    (let ((view (find-view cube facet-name)))
      (check-no-watchers cube nil "Cannot remove facet ~S" facet-name)
      (setf (cdr (slot-value cube 'views))
            (remove facet-name (views cube) :key #'view-facet-name))
      (deregister-cube-facet cube facet-name)
      view)))

(defun has-watchers-p (view)
  (plusp (view-n-watchers view)))

(defun has-writers-p (view)
  (and (has-watchers-p view)
       (not (eq :input (view-direction view)))))

(defun ensure-view (cube facet-name direction)
  (let ((view (find-view cube facet-name)))
    ;; First check that there are no conflicting views for other
    ;; facets.
    (if (eq direction :input)
        (unless *let-input-through-p*
          (check-no-writers cube facet-name
                            "Cannot create view for ~S in direction ~S"
                            facet-name direction))
        (unless *let-output-through-p*
          (check-no-watchers cube facet-name
                             "Cannot create view for ~S in direction ~S"
                             facet-name direction)))
    (if (null view)
        (multiple-value-bind (facet facet-description must-be-destroyed-p)
            (make-facet* cube facet-name)
          (when must-be-destroyed-p
            (ensure-cube-finalized cube))
          (add-view cube facet-name facet facet-description direction))
        (let ((watchers (view-watcher-threads view))
              (view-direction (view-direction view)))
          (cond
            ;; If there are no other watchers, we can just reuse VIEW
            ;; since there are no conflicting views either.
            ((endp watchers)
             (setf (view-direction view) direction))
            ;; There are watchers but they are :INPUT and we also want
            ;; to create an :INPUT facet. Nothing to do.
            ((and watchers (eq direction :input) (eq view-direction :input)))
            ;; There are watchers, and at least one of DIRECTION and
            ;; VIEW-DIRECTION is :IO or :OUTPUT so we have a
            ;; reader/writer conflict. Still, let the view be shared
            ;; if the only watcher is the current thread.
            ((and (every (let ((current-thread
                                 (bordeaux-threads:current-thread)))
                           (lambda (watcher)
                             (eq watcher current-thread)))
                         watchers))
             ;; Make sure VIEW-DIRECTION is not :INPUT (it doesn't
             ;; matter whether it's :IO or :OUTPUT).
             (setf (view-direction view) :io))
            ;; There are no conflicting views, but there are watchers,
            ;; VIEW-DIRECTION is not :INPUT, and other threads are
            ;; watching VIEW.
            ((eq direction :input)
             (unless *let-input-through-p*
               (error "Cannot create nested view for ~S in direction ~S ~
                      because there are other threads writing the same ~
                      facet." facet-name direction))
             (setf (view-direction view) :io))
            ;; There are no conflicting views, but there are watchers,
            ;; VIEW-DIRECTION may be :INPUT, and other threads are
            ;; watching VIEW.
            (t
             (unless *let-output-through-p*
               (error "Cannot create nested view for ~S in direction ~S ~
                      because there are other threads using the same ~
                      facet." facet-name direction))))
          view))))

(defun find-up-to-date-view (cube)
  (find-if #'view-up-to-date-p (views cube)))

(defun find-up-to-date-facet-name (cube)
  (let ((view (find-if #'view-up-to-date-p (views cube))))
    (if view
        (view-facet-name view)
        nil)))
