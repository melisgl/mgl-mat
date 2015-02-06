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

(defun vec-facet-to-char (vec facet)
  (let* ((name (facet-name facet))
         (char (aref (symbol-name name) 0)))
    (if (facet-up-to-date-p* vec name facet)
        (char-upcase char)
        (char-downcase char))))

(defun print-vec-facets (mat stream)
  (let ((chars (mapcar (lambda (facet)
                         (vec-facet-to-char mat facet))
                       (facets mat))))
    (if chars
        (format stream "~{~A~}" (sort chars #'char-lessp))
        (format stream "-"))))

(defun will-be-copied-over-p (vec)
  (find-if #'facet-up-to-date-p (facets vec)))

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
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'lisp-vector)) lisp-vector-facet
                        (to-name (eql 'static-vector)) static-vector-facet)
  (let ((lisp-vector (facet-value lisp-vector-facet))
        (static-pointer (static-vectors:static-vector-pointer
                         (facet-value static-vector-facet))))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (memcpy static-pointer lisp-pointer (vec-n-bytes vec)))
        (lla::copy-array-to-memory lisp-vector static-pointer
                                   (ctype->lla-internal (vec-ctype vec))))))

;;; static-vector -> lisp-vector
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'static-vector)) static-vector-facet
                        (to-name (eql 'lisp-vector)) lisp-vector-facet)
  (let ((static-pointer (static-vectors:static-vector-pointer
                         (facet-value static-vector-facet)))
        (lisp-vector (facet-value lisp-vector-facet)))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (memcpy lisp-pointer static-pointer (vec-n-bytes vec)))
        (lla::copy-array-from-memory lisp-vector static-pointer
                                     (ctype->lla-internal (vec-ctype vec))))))

;;; static-vector -> cuda-vector
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'static-vector)) static-vector-facet
                        (to-name (eql 'cuda-vector)) cuda-vector-facet)
  (incf *n-memcpy-host-to-device*)
  #+nil
  (unless *let-input-through-p*
    (break))
  (let ((static-pointer (static-vectors:static-vector-pointer
                         (facet-value static-vector-facet)))
        (cuda-vector (facet-value cuda-vector-facet)))
    (if (eq *cuda-stream* *cuda-copy-stream*)
        (cl-cuda.driver-api:cu-memcpy-host-to-device-async
         (base-pointer cuda-vector) static-pointer (vec-n-bytes vec)
         *cuda-stream*)
        (cl-cuda.driver-api::cu-memcpy-host-to-device
         (base-pointer cuda-vector) static-pointer (vec-n-bytes vec)))))

;;; cuda-vector -> static-vector
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'cuda-vector)) cuda-vector-facet
                        (to-name (eql 'static-vector)) static-vector-facet)
  (incf *n-memcpy-device-to-host*)
  #+nil
  (unless *let-input-through-p*
    (break))
  (let ((cuda-vector (facet-value cuda-vector-facet))
        (static-pointer (static-vectors:static-vector-pointer
                         (facet-value static-vector-facet))))
    (if (eq *cuda-stream* *cuda-copy-stream*)
        (cl-cuda.driver-api:cu-memcpy-device-to-host-async
         static-pointer (base-pointer cuda-vector) (vec-n-bytes vec)
         *cuda-stream*)
        (cl-cuda.driver-api::cu-memcpy-device-to-host
         static-pointer (base-pointer cuda-vector) (vec-n-bytes vec)))))

;;; lisp-vector -> cuda-vector
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'lisp-vector)) lisp-vector-facet
                        (to-name (eql 'cuda-vector)) cuda-vector-facet)
  (incf *n-memcpy-host-to-device*)
  (let ((lisp-vector (facet-value lisp-vector-facet))
        (cuda-vector (facet-value cuda-vector-facet)))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (cl-cuda.driver-api::cu-memcpy-host-to-device
           (base-pointer cuda-vector) lisp-pointer (vec-n-bytes vec)))
        (with-facet (static-vector (vec 'static-vector :direction :input))
          (let ((static-pointer
                  (static-vectors:static-vector-pointer static-vector)))
            (cl-cuda.driver-api::cu-memcpy-host-to-device
             (base-pointer cuda-vector) static-pointer (vec-n-bytes vec)))))))

;;; cuda-vector -> lisp-vector
(defmethod copy-facet* ((vec vec)
                        (from-name (eql 'cuda-vector)) cuda-vector-facet
                        (to-name (eql 'lisp-vector)) lisp-vector-facet)
  (incf *n-memcpy-device-to-host*)
  (let ((cuda-vector (facet-value cuda-vector-facet))
        (lisp-vector (facet-value lisp-vector-facet)))
    (if (pinning-supported-p)
        (lla::with-pinned-array (lisp-pointer lisp-vector)
          (cl-cuda.driver-api::cu-memcpy-device-to-host
           lisp-pointer (base-pointer cuda-vector) (vec-n-bytes vec)))
        (with-facet (static-vector (vec 'static-vector :direction :input))
          (let ((static-pointer
                  (static-vectors:static-vector-pointer static-vector)))
            (cl-cuda.driver-api::cu-memcpy-device-to-host
             static-pointer (base-pointer cuda-vector) (vec-n-bytes vec)))))))

(defmethod destroy-facet* ((facet-name (eql 'lisp-vector)) facet)
  (declare (ignore facet)))

(defmethod destroy-facet* ((facet-name (eql 'static-vector)) facet)
  (free-static-vector (facet-value facet)))

(defmethod destroy-facet* ((facet-name (eql 'cuda-vector)) facet)
  (free-cuda-vector (facet-value facet)))
