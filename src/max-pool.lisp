(in-package :mgl-mat)

(define-lisp-kernel (lisp-max-pool-2d)
    ((x :mat :input) (start-x index) (x-height index) (x-width index)
     (w-height index) (w-width index)
     (y :mat :io) (start-y index) (y-height index) (y-width index)
     (start-0 index) (start-1 index)
     (stride-0 index) (stride-1 index)
     (anchor-0 index) (anchor-1 index)
     (batch-size index))
  (loop
    for batch below batch-size
    do ;; Loop over the rows of Y and rows of the anchor points in X.
       (loop
         for xi-0 of-type index upfrom start-0 by stride-0
         for yi-0 below y-height
         do (let (;; Calculate the start index of the row into X and
                  ;; Y, taking into account the displacement and the
                  ;; batch index. These are incremented by the inner
                  ;; loop so that they point to the index of the
                  ;; current element.
                  (xi (+ start-x
                         (the! index (* batch
                                        (the! index (* x-height x-width))))
                         (the! index (* xi-0 x-width))))
                  (yi (+ start-y
                         (the! index (* batch
                                        (the! index (* y-height y-width))))
                         (the! index (* yi-0 y-width)))))
              (declare (type index xi yi))
              ;; Loop over the columns of Y and columns of the anchor
              ;; points in X.
              (loop
                for xi-1 of-type index upfrom start-1 by stride-1
                for yi-1 below y-width
                do (let ((max 0.0)
                         (found-max-p nil))
                     ;; Looping over the rows of W.
                     (loop
                       for wi-0 below w-height
                       for d-row = (the! fixnum (- wi-0 anchor-0))
                       do ;; Check that adding XI-OFFSET-0, the row
                          ;; offset, results in a valid row.
                          (when (let ((xi-0* (+ xi-0 d-row)))
                                  (and (<= 0 xi-0*)
                                       (< xi-0* x-height)))
                            (let ((xi-offset-0 (the! index (* d-row x-width))))
                              (loop
                                for wi-1 below w-width
                                for d-column = (the! fixnum (- wi-1 anchor-1))
                                do ;; Check that adding XI-OFFSET-1,
                                   ;; the column offset, results in a
                                   ;; valid row.
                                   (when (let ((xi-1* (+ xi-1 d-column)))
                                           (and (<= 0 xi-1*)
                                                (< xi-1* x-width)))
                                     (let ((xe (aref x
                                                     (+ xi
                                                        (the! fixnum
                                                              (+ xi-offset-0
                                                                 d-column))))))
                                       (when (or (not found-max-p) (< max xe))
                                         (setq max xe)
                                         (setq found-max-p t))))))))
                     (setf (aref y yi) max))
                   (incf xi stride-1)
                   (incf yi))))))

(define-cuda-kernel (cuda-max-pool-2d)
    (void ((x :mat :input) (x-height int) (x-width int)
           (w-height int) (w-width int)
           (y :mat :io) (y-height int) (y-width int)
           (start-0 int) (start-1 int)
           (stride-0 int) (stride-1 int)
           (anchor-0 int) (anchor-1 int)
           (batch-size int)))
  (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
        (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
        ;; batch is axis Z
        (id-z (+ (* block-dim-z block-idx-z) thread-idx-z))
        (stride-x (* block-dim-x grid-dim-x))
        (stride-y (* block-dim-y grid-dim-y))
        (stride-z (* block-dim-z grid-dim-z)))
    ;; Iterate over the batch, and the elements of Y in the three
    ;; nested DO loops.
    (do ((batch-index id-z (+ batch-index stride-z)))
        ((>= batch-index batch-size))
      (do ((row id-y (+ row stride-y)))
          ((>= row y-height))
        (let* (;; the index of the element being calculated in Y
               ;; (incremented)
               (yi (* (+ (* batch-index y-height) row) y-width))
               ;; the row of the anchor point in X
               (xi-0 (+ start-0 (* row stride-0)))
               ;; the column of the anchor point in X (incremented)
               (xi-1 start-1)
               ;; the index of anchor point in X (incremented)
               (xi (+ (* (+ (* batch-index x-height) xi-0)
                         x-width)
                      xi-1)))
          (do ((column id-x (1+ column)))
              ((>= column y-width))
            (let ((max 0.0)
                  (found-max-p 0))
              ;; Loop over the rows of W.
              (do ((wi-0 0 (1+ wi-0)))
                  ((>= wi-0 w-height))
                (let* ((d-row (- wi-0 anchor-0))
                       (xi-0- (+ xi-0 d-row)))
                  ;; Check that XI-0- is a valid row.
                  (when (<= 0 xi-0-)
                    (when (< xi-0- x-height)
                      (let* ((w-offset-i (* wi-0 w-width))
                             (wi w-offset-i)
                             (xi-offset-0 (* d-row x-width)))
                        ;; Loop over columns of W.
                        (do ((wi-1 0 (1+ wi-1)))
                            ((>= wi-1 w-width))
                          (let* ((d-column (- wi-1 anchor-1))
                                 (xi-1- (+ xi-1 d-column)))
                            ;; Check that XI-1- is a valid column.
                            (when (<= 0 xi-1-)
                              (when (< xi-1- x-width)
                                (let ((xe (aref x
                                                (+ xi
                                                   (+ xi-offset-0
                                                      d-column)))))
                                  (if (= 0 found-max-p)
                                      (progn
                                        (set max xe)
                                        (set found-max-p 1))
                                      (when (< max xe)
                                        (set max xe)))))))))))))
              (setf (aref y yi) max))
            (incf yi)
            (incf xi stride-1)
            (incf xi-1 stride-1)))))))

(defun max-pool! (x y &key start stride anchor batched pool-dimensions)
  ""
  (let* ((x-dimensions (if batched
                           (mat-dimensions x)
                           (cons 1 (mat-dimensions x))))
         (y-dimensions (if batched
                           (mat-dimensions y)
                           (cons 1 (mat-dimensions y))))
         (n-dimensions (1- (length x-dimensions))))
    (assert (equal (1- (length x-dimensions)) (length pool-dimensions)))
    (check-dimensions x-dimensions pool-dimensions y-dimensions
                      start stride anchor)
    (case n-dimensions
      ((2)
       (destructuring-bind (batch-size x-height x-width) x-dimensions
         (destructuring-bind (batch-size2 y-height y-width) y-dimensions
           (destructuring-bind (w-height w-width) pool-dimensions
             (assert (= batch-size batch-size2))
             (if (use-cuda-p)
                 ;; FIXME: if STRIDE is one, then use the npp
                 ;; implementation.
                 (multiple-value-bind (block-dim grid-dim)
                     (choose-3d-block-and-grid (mat-dimensions y) 4)
                   (cuda-max-pool-2d x x-height x-width
                                     w-height w-width
                                     y y-height y-width
                                     (elt start 0) (elt start 1)
                                     (elt stride 0) (elt stride 1)
                                     (elt anchor 0) (elt anchor 1)
                                     batch-size
                                     :grid-dim grid-dim
                                     :block-dim block-dim))
                 (lisp-max-pool-2d x (mat-displacement x) x-height x-width
                                   w-height w-width
                                   y (mat-displacement y) y-height y-width
                                   (elt start 0) (elt start 1)
                                   (elt stride 0) (elt stride 1)
                                   (elt anchor 0) (elt anchor 1)
                                   batch-size))))))
      (t
       (error "Convolution not implemented for 1 + ~S dimensions."
              n-dimensions)))
    y))


;;;; Derivative

(define-lisp-kernel (lisp-derive-max-pool-2d)
    ((x :mat :input) (start-x index) (x-height index) (x-width index)
     (xd :mat :io) (start-xd index)
     (w-height index) (w-width index)
     (y :mat :input) (start-y index) (y-height index) (y-width index)
     (yd :mat :input) (start-yd index)
     (start-0 index) (start-1 index)
     (stride-0 index) (stride-1 index)
     (anchor-0 index) (anchor-1 index)
     (batch-size index))
  (let ((x-xd-start-diff (- start-xd start-x))
        (y-yd-start-diff (- start-yd start-y)))
    (loop
      for batch below batch-size
      do ;; Loop over the rows of Y and rows of the anchor points in X.
         (loop
           for xi-0 of-type index upfrom start-0 by stride-0
           for yi-0 below y-height
           do (let ( ;; Calculate the start index of the row into X and
                    ;; Y, taking into account the displacement and the
                    ;; batch index. These are incremented by the inner
                    ;; loop so that they point to the index of the
                    ;; current element.
                    (xi (+ start-x
                           (the! index (* batch
                                          (the! index (* x-height x-width))))
                           (the! index (* xi-0 x-width))))
                    (yi (+ start-y
                           (the! index (* batch
                                          (the! index (* y-height y-width))))
                           (the! index (* yi-0 y-width)))))
                (declare (type index xi yi))
                ;; Loop over the columns of Y and columns of the anchor
                ;; points in X.
                (loop
                  for xi-1 of-type index upfrom start-1 by stride-1
                  for yi-1 below y-width
                  do (let ((ye (aref y yi))
                           (yde (aref yd (+ yi y-yd-start-diff))))
                       ;; Looping over the rows of W.
                       (loop
                         for wi-0 below w-height
                         for d-row = (the! fixnum (- wi-0 anchor-0))
                         do ;; Check that adding XI-OFFSET-0, the row
                            ;; offset, results in a valid row.
                            (when (let ((xi-0* (+ xi-0 d-row)))
                                    (and (<= 0 xi-0*)
                                         (< xi-0* x-height)))
                              (let ((xi-offset-0
                                      (the! index (* d-row x-width))))
                                (loop
                                  for wi-1 below w-width
                                  for d-column = (the! fixnum (- wi-1 anchor-1))
                                  do ;; Check that adding XI-OFFSET-1,
                                     ;; the column offset, results in a
                                     ;; valid row.
                                     (when (let ((xi-1* (+ xi-1 d-column)))
                                             (and (<= 0 xi-1*)
                                                  (< xi-1* x-width)))
                                       (let ((xii (+ xi
                                                     (the! fixnum
                                                           (+ xi-offset-0
                                                              d-column)))))
                                         (when (= (aref x xii) ye)
                                           (incf (aref xd
                                                       (+ xii x-xd-start-diff))
                                                 yde)))))))))
                     (incf xi stride-1)
                     (incf yi)))))))

(define-cuda-kernel (cuda-derive-max-pool-2d)
    (void ((x :mat :input) (x-height int) (x-width int)
           (xd :mat :io)
           (w-height int) (w-width int)
           (y :mat :input) (y-height int) (y-width int)
           (yd :mat :input)
           (start-0 int) (start-1 int)
           (stride-0 int) (stride-1 int)
           (anchor-0 int) (anchor-1 int)
           (batch-size int)))
  (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
        (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
        ;; batch is axis Z
        (id-z (+ (* block-dim-z block-idx-z) thread-idx-z))
        (stride-x (* block-dim-x grid-dim-x))
        (stride-y (* block-dim-y grid-dim-y))
        (stride-z (* block-dim-z grid-dim-z)))
    ;; Iterate over the batch, and the elements of Y in the three
    ;; nested DO loops.
    (do ((batch-index id-z (+ batch-index stride-z)))
        ((>= batch-index batch-size))
      (do ((row id-y (+ row stride-y)))
          ((>= row y-height))
        (let* (;; the index of the element being calculated in Y
               ;; (incremented)
               (yi (* (+ (* batch-index y-height) row) y-width))
               ;; the row of the anchor point in X
               (xi-0 (+ start-0 (* row stride-0)))
               ;; the column of the anchor point in X (incremented)
               (xi-1 start-1)
               ;; the index of anchor point in X (incremented)
               (xi (+ (* (+ (* batch-index x-height) xi-0)
                         x-width)
                      xi-1)))
          (do ((column id-x (1+ column)))
              ((>= column y-width))
            (let ((ye (aref y yi))
                  (yde (aref yd yi)))
              ;; Loop over the rows of W.
              (do ((wi-0 0 (1+ wi-0)))
                  ((>= wi-0 w-height))
                (let* ((d-row (- wi-0 anchor-0))
                       (xi-0- (+ xi-0 d-row)))
                  ;; Check that XI-0- is a valid row.
                  (when (<= 0 xi-0-)
                    (when (< xi-0- x-height)
                      (let ((xi-offset-0 (* d-row x-width)))
                        ;; Loop over columns of W.
                        (do ((wi-1 0 (1+ wi-1)))
                            ((>= wi-1 w-width))
                          (let* ((d-column (- wi-1 anchor-1))
                                 (xi-1- (+ xi-1 d-column)))
                            ;; Check that XI-1- is a valid column.
                            (when (<= 0 xi-1-)
                              (when (< xi-1- x-width)
                                (let ((xii (+ xi xi-offset-0 d-column)))
                                  (when (= (aref x xii) ye)
                                    (incf (aref xd xii) yde)))))))))))))
            (incf yi)
            (incf xi stride-1)
            (incf xi-1 stride-1)))))))

(defun derive-max-pool! (x xd y yd &key start stride anchor batched
                         pool-dimensions)
  "Add the dF/dX to XD and and dF/dW to WD where YD is dF/dY for some
  function F where Y is the result of MAX-POOL! with the same
  arguments. "
  (let* ((x-dimensions (if batched
                           (mat-dimensions x)
                           (cons 1 (mat-dimensions x))))
         (y-dimensions (if batched
                           (mat-dimensions y)
                           (cons 1 (mat-dimensions y))))
         (n-dimensions (1- (length x-dimensions))))
    (check-dimensions x-dimensions pool-dimensions y-dimensions
                      start stride anchor)
    (assert (equal (mat-dimensions x) (mat-dimensions xd)))
    (assert (equal (mat-dimensions y) (mat-dimensions yd)))
    (assert (equal (1- (length x-dimensions)) (length pool-dimensions)))
    (case n-dimensions
      ((2)
       (destructuring-bind (batch-size x-height x-width) x-dimensions
         (destructuring-bind (batch-size2 y-height y-width) y-dimensions
           (destructuring-bind (w-height w-width) pool-dimensions
             (assert (= batch-size batch-size2))
             (if (use-cuda-p)
                 ;; FIXME: if STRIDE is one, then use the npp
                 ;; implementation.
                 (multiple-value-bind (block-dim grid-dim)
                     (choose-3d-block-and-grid (mat-dimensions yd) 4)
                   (cuda-derive-max-pool-2d x x-height x-width
                                            xd
                                            w-height w-width
                                            y y-height y-width
                                            yd
                                            (elt start 0) (elt start 1)
                                            (elt stride 0) (elt stride 1)
                                            (elt anchor 0) (elt anchor 1)
                                            batch-size
                                            :grid-dim grid-dim
                                            :block-dim block-dim))
                 (lisp-derive-max-pool-2d
                  x (mat-displacement x) x-height x-width
                  xd (mat-displacement xd)
                  w-height w-width
                  y (mat-displacement y) y-height y-width
                  yd (mat-displacement yd)
                  (elt start 0) (elt start 1)
                  (elt stride 0) (elt stride 1)
                  (elt anchor 0) (elt anchor 1)
                  batch-size))))))
      (t
       (error "Convolution not implemented for 1 + ~S dimensions."
              n-dimensions)))))
