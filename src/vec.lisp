(in-package :mgl-mat)

;;; A VEC is to MAT what an underlying simple vector (also known as
;;; backing array) is to a lisp array: a non-displaced, simple, 1d
;;; storage. It has three independent facets: LISP-VECTOR,
;;; STATIC-VECTOR and CUDA-VECTOR. A MAT object has an internal VEC
;;; and MAT's facets alias facets of VEC. The same VEC may belong to
;;; several MATs.
(defclass vec (cube)
  ((ctype
    :type ctype :initform *default-mat-ctype*
    :initarg :ctype :reader vec-ctype)
   ;; FIXME: support it
   (initial-element
    :initform 0 :initarg :initial-element
    :reader vec-initial-element)
   (size :initarg :size :reader vec-size)
   ;; The number of bytes SIZE number of elements take.
   (n-bytes :reader vec-n-bytes)))

(defmethod initialize-instance :after ((vec vec) &key &allow-other-keys)
  (setf (slot-value vec 'n-bytes)
        (* (vec-size vec) (ctype-size (vec-ctype vec))))
  (when (vec-initial-element vec)
    (setf (slot-value vec 'initial-element)
          (coerce-to-ctype (vec-initial-element vec) :ctype (vec-ctype vec))))
  ;; FIXME?
  #+nil
  (when initial-contents
    (replace! mat initial-contents))
  (note-allocation (vec-n-bytes vec)))

(defvar *print-vec-facets* t)

(defvar *print-vec* nil)

(defmethod print-object ((vec vec) stream)
  (print-unreadable-object (vec stream :type t :identity (not *print-vec*))
    (format stream "~S" (vec-size vec))
    (when *print-vec-facets*
      (write-char #\Space stream)
      (print-vec-facets vec stream))
    (when *print-vec*
      (write-char #\Space stream)
      (let ((*let-input-through-p* t))
        (ignore-errors
         (with-facets ((a (vec 'lisp-vector :direction :input)))
           (write a :stream stream)))))))

(defun vec-view-to-char (vec view)
  (let* ((name (view-facet-name view))
         (char (aref (symbol-name name) 0)))
    (if (up-to-date-p* vec name view)
        (char-upcase char)
        (char-downcase char))))

(defun print-vec-facets (mat stream)
  (let ((chars (mapcar (lambda (view)
                         (vec-view-to-char mat view))
                       (views mat))))
    (if chars
        (format stream "~{~A~}" (sort chars #'char-lessp))
        (format stream "-"))))

(defun will-be-copied-over-p (vec)
  (find-if #'view-up-to-date-p (views vec)))

(defmethod make-facet* ((vec vec) (facet-name (eql 'lisp-vector)))
  (cond ((and (vec-initial-element vec)
              (not (will-be-copied-over-p vec)))
         (make-array (vec-size vec)
                     :element-type (ctype->lisp (vec-ctype vec))
                     :initial-element (vec-initial-element vec)))
        (t
         (make-array (vec-size vec)
                     :element-type (ctype->lisp (vec-ctype vec))))))

(defmethod make-facet* ((vec vec) (facet-name (eql 'static-vector)))
  (let ((vector (alloc-static-vector (vec-ctype vec) (vec-size vec)
                                     (if (will-be-copied-over-p vec)
                                         nil
                                         (vec-initial-element vec)))))
    (values vector nil t)))

(defmethod make-facet* ((vec vec) (facet-name (eql 'cuda-vector)))
  (let ((vector (alloc-cuda-vector (* (vec-size vec)
                                      (ctype-size (vec-ctype vec))))))
    (when (and (vec-initial-element vec)
               (not (will-be-copied-over-p vec)))
      (cuda-fill!-2 (vec-ctype vec) (vec-initial-element vec)
                    vector (vec-size vec)))
    (values vector nil t)))

;;; lisp-vector -> static vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'lisp-vector)) lisp-vector
                        (to-name (eql 'static-vector)) static-vector)
  (let ((static-pointer (static-vectors:static-vector-pointer static-vector)))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (memcpy static-pointer lisp-pointer (vec-n-bytes vec)))
        (lla::copy-array-to-memory lisp-vector static-pointer
                                   (ctype->lla-internal (vec-ctype vec))))))

;;; static-vector -> lisp-vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'static-vector)) static-vector
                        (to-name (eql 'lisp-vector)) lisp-vector)
  (let ((static-pointer (static-vectors:static-vector-pointer static-vector)))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (memcpy lisp-pointer static-pointer (vec-n-bytes vec)))
        (lla::copy-array-from-memory lisp-vector static-pointer
                                     (ctype->lla-internal (vec-ctype vec))))))

;;; static-vector -> cuda-vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'static-vector)) static-vector
                        (to-name (eql 'cuda-vector)) cuda-vector)
  (incf *n-memcpy-host-to-device*)
  #+nil
  (unless *let-input-through-p*
    (break))
  (let ((static-pointer (static-vectors:static-vector-pointer static-vector)))
    (if (eq *cuda-stream* *cuda-copy-stream*)
        (cl-cuda.driver-api:cu-memcpy-host-to-device-async
         (base-pointer cuda-vector) static-pointer (vec-n-bytes vec)
         *cuda-stream*)
        (cl-cuda.driver-api::cu-memcpy-host-to-device
         (base-pointer cuda-vector) static-pointer (vec-n-bytes vec)))))

;;; cuda-vector -> static-vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'cuda-vector)) cuda-vector
                        (to-name (eql 'static-vector)) static-vector)
  (incf *n-memcpy-device-to-host*)
  #+nil
  (unless *let-input-through-p*
    (break))
  (let ((static-pointer (static-vectors:static-vector-pointer static-vector)))
    (if (eq *cuda-stream* *cuda-copy-stream*)
        (cl-cuda.driver-api:cu-memcpy-device-to-host-async
         static-pointer (base-pointer cuda-vector) (vec-n-bytes vec)
         *cuda-stream*)
        (cl-cuda.driver-api::cu-memcpy-device-to-host
         static-pointer (base-pointer cuda-vector) (vec-n-bytes vec)))))

;;; lisp-vector -> cuda-vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'lisp-vector)) lisp-vector
                        (to-name (eql 'cuda-vector)) cuda-vector)
  (incf *n-memcpy-host-to-device*)
  (if (pinning-supported-p)
      (lla::with-pinned-array (lisp-pointer lisp-vector)
        (cl-cuda.driver-api::cu-memcpy-host-to-device
         (base-pointer cuda-vector) lisp-pointer (vec-n-bytes vec)))
      (with-facet (static-vector (vec 'static-vector :direction :input))
        (let ((static-pointer
                (static-vectors:static-vector-pointer static-vector)))
          (cl-cuda.driver-api::cu-memcpy-host-to-device
           (base-pointer cuda-vector) static-pointer (vec-n-bytes vec))))))

;;; cuda-vector -> lisp-vector
(defmethod copy-facet* ((vec vec) (from-name (eql 'cuda-vector)) cuda-vector
                        (to-name (eql 'lisp-vector)) lisp-vector)
  (incf *n-memcpy-device-to-host*)
  (if (pinning-supported-p)
      (lla::with-pinned-array (lisp-pointer lisp-vector)
        (cl-cuda.driver-api::cu-memcpy-device-to-host
         lisp-pointer (base-pointer cuda-vector) (vec-n-bytes vec)))
      (with-facet (static-vector (vec 'static-vector :direction :input))
        (let ((static-pointer
                (static-vectors:static-vector-pointer static-vector)))
          (cl-cuda.driver-api::cu-memcpy-device-to-host
           static-pointer (base-pointer cuda-vector) (vec-n-bytes vec))))))

(defmethod destroy-facet* ((facet-name (eql 'lisp-vector)) vector description)
  (declare (ignore vector description)))

(defmethod destroy-facet* ((facet-name (eql 'static-vector)) vector description)
  (declare (ignore description))
  (free-static-vector vector))

(defmethod destroy-facet* ((facet-name (eql 'cuda-vector)) vector description)
  (declare (ignore facet-name description))
  (free-cuda-vector vector))
