(in-package :mgl-mat)

(define-lisp-kernel (lisp-convolve-2d)
    ((x :mat :input) (start-x index) (x-height index) (x-width index)
     (w :mat :input) (start-w index) (w-height index) (w-width index)
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
                do (let ((sum 0.0))
                     ;; Looping over the rows of W.
                     (loop
                       for wi-0 below w-height
                       for d-row = (the! fixnum (- wi-0 anchor-0))
                       do ;; Check that adding XI-OFFSET-0, the row
                          ;; offset, results in a valid row.
                          (when (let ((xi-0* (+ xi-0 d-row)))
                                  (and (<= 0 xi-0*)
                                       (< xi-0* x-height)))
                            (let* ((w-offset-i (the! index (* wi-0 w-width)))
                                   (wi (+ start-w w-offset-i))
                                   (xi-offset-0 (the! index (* d-row x-width))))
                              (declare (type index w-offset-i wi))
                              (loop
                                for wi-1 below w-width
                                for d-column = (the! fixnum (- wi-1 anchor-1))
                                do ;; Check that adding XI-OFFSET-1,
                                   ;; the column offset, results in a
                                   ;; valid row.
                                   (when (let ((xi-1* (+ xi-1 d-column)))
                                           (and (<= 0 xi-1*)
                                                (< xi-1* x-width)))
                                     (incf sum
                                           (* (aref w wi)
                                              (aref x
                                                    (+ xi
                                                       (the! fixnum
                                                             (+ xi-offset-0
                                                                d-column)))))))
                                   (incf w-offset-i)
                                   (incf wi)))))
                     (incf (aref y yi) sum))
                   (incf xi stride-1)
                   (incf yi))))))

(define-cuda-kernel (cuda-convolve-2d)
    (void ((x :mat :input) (x-height int) (x-width int)
           (w :mat :input) (w-height int) (w-width int)
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
            (let ((sum 0.0))
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
                                (incf sum
                                      (* (aref w wi)
                                         (aref x
                                               (+ xi
                                                  (+ xi-offset-0
                                                     d-column))))))))
                          (incf w-offset-i)
                          (incf wi)))))))
              (incf (aref y yi) sum))
            (incf yi)
            (incf xi stride-1)
            (incf xi-1 stride-1)))))))

(defun valid-multi-dimensional-index-p (index-list dimensions)
  (every (lambda (index limit)
           (and (<= 0 index)
                (< index limit)))
         index-list dimensions))

(defun check-dimensions (x-dimensions w-dimensions y-dimensions
                         start stride anchor)
  (let ((n-dimensions (1- (length x-dimensions))))
    (assert (= n-dimensions (length start)))
    (assert (= n-dimensions (length stride)))
    (assert (= n-dimensions (length anchor)))
    (assert (valid-multi-dimensional-index-p anchor w-dimensions))
    (assert (valid-multi-dimensional-index-p start x-dimensions))
    (loop for i upfrom 0
          for x-dim in (rest x-dimensions)
          for y-dim in (rest y-dimensions)
          for start in start
          for stride in stride
          do (let ((n-application-points (1+ (floor (- x-dim start 1) stride))))
               (assert (= y-dim n-application-points) ()
                       "Dimension ~S of Y is ~S while there are ~S ~
                        application points."
                       i y-dim n-application-points)))))

(defun convolve! (x w y &key start stride anchor batched)
  "Y = Y + conv(X, W) and return Y. If BATCHED, then the first
  dimension of X and Y is the number of elements in the batch (B),
  else B is assumed to be 1. The rest of the dimensions encode the
  input (X) and output (Y} N dimensional feature maps. START, STRIDE
  and ANCHOR are lists of length N. START is the multi-dimensional
  index of the first element of the input feature map (for each
  element in the batch) for which the convolution must be computed.
  Then (ELT STRIDE (- N 1)) is added to the last element of START and
  so on until (ARRAY-DIMENSION X 1) is reached. Then the last element
  of START is reset, (ELT STRIDE (- N 2)) is added to the first but
  last element of START and we scan the last dimension again. Take a
  2d example, START is (0 0), STRIDE is (1 2), and X is a B*2x7
  matrix.

  W is:

      1 2 1
      2 4 2
      1 2 1

  and ANCHOR is (1 1) which refers to the element of W whose value is
  4. This anchor point of W is placed over elements of X whose multi
  dimensional index is in numbers in this figure (only one element in
  the batch is shown):

      0,0 . 0,2 . 0,4 . 0,6
      1,0 . 1,2 . 1,4 . 1,6

  When applying W at position P of X, the convolution is the sum of
  the products of overlapping elements of X and W when W's ANCHOR is
  placed at P. Elements of W over the edges of X are multiplied with 0
  so are effectively ignored. The order of application of W to
  positions defined by START, STRIDE and ANCHOR is undefined.

  Y must be a B*2x4 (or 2x4 if not BATCHED) matrix in this example,
  just large enough to hold the results of the convolutions."
  (let* ((x-dimensions (if batched
                           (mat-dimensions x)
                           (cons 1 (mat-dimensions x))))
         (y-dimensions (if batched
                           (mat-dimensions y)
                           (cons 1 (mat-dimensions y))))
         (w-dimensions (mat-dimensions w))
         (n-dimensions (1- (length x-dimensions))))
    (check-dimensions x-dimensions w-dimensions y-dimensions
                      start stride anchor)
    (case n-dimensions
      ((2)
       (destructuring-bind (batch-size x-height x-width) x-dimensions
         (destructuring-bind (batch-size2 y-height y-width) y-dimensions
           (destructuring-bind (w-height w-width) w-dimensions
             (assert (= batch-size batch-size2))
             (if (use-cuda-p)
                 ;; FIXME: if STRIDE is one, then use the npp
                 ;; implementation.
                 (multiple-value-bind (block-dim grid-dim)
                     (choose-3d-block-and-grid (mat-dimensions y) 4)
                   (cuda-convolve-2d x x-height x-width
                                     w w-height w-width
                                     y y-height y-width
                                     (elt start 0) (elt start 1)
                                     (elt stride 0) (elt stride 1)
                                     (elt anchor 0) (elt anchor 1)
                                     batch-size
                                     :grid-dim grid-dim
                                     :block-dim block-dim))
                 (lisp-convolve-2d x (mat-displacement x) x-height x-width
                                   w (mat-displacement w) w-height w-width
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

(define-lisp-kernel (lisp-derive-convolve-2d)
    ((x :mat :input) (start-x index) (x-height index) (x-width index)
     (xd :mat :io) (start-xd index)
     (w :mat :input) (start-w index) (w-height index) (w-width index)
     (wd :mat :io) (start-wd index)
     (yd :mat :input) (start-yd index) (y-height index) (y-width index)
     (start-0 index) (start-1 index)
     (stride-0 index) (stride-1 index)
     (anchor-0 index) (anchor-1 index)
     (batch-size index))
  (let ((w-wd-start-diff (- start-wd start-w))
        (x-xd-start-diff (- start-xd start-x)))
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
                    (yi (+ start-yd
                           (the! index (* batch
                                          (the! index (* y-height y-width))))
                           (the! index (* yi-0 y-width)))))
                (declare (type index xi yi))
                ;; Loop over the columns of Y and columns of the anchor
                ;; points in X.
                (loop
                  for xi-1 of-type index upfrom start-1 by stride-1
                  for yi-1 below y-width
                  do (let ((yde (aref yd yi)))
                       ;; Looping over the rows of W.
                       (loop
                         for wi-0 below w-height
                         for d-row = (the! fixnum (- wi-0 anchor-0))
                         do ;; Check that adding XI-OFFSET-0, the row
                            ;; offset, results in a valid row.
                            (when (let ((xi-0* (+ xi-0 d-row)))
                                    (and (<= 0 xi-0*)
                                         (< xi-0* x-height)))
                              (let* ((w-offset-i (the! index (* wi-0 w-width)))
                                     (wi (+ start-w w-offset-i))
                                     (xi-offset-0
                                       (the! index (* d-row x-width))))
                                (declare (type index w-offset-i wi))
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
                                         (incf (aref wd (+ wi w-wd-start-diff))
                                               (* yde (aref x xii)))
                                         (incf (aref xd (+ xii x-xd-start-diff))
                                               (* yde (aref w wi)))))
                                     (incf w-offset-i)
                                     (incf wi))))))
                     (incf xi stride-1)
                     (incf yi)))))))

(define-cuda-kernel (cuda-derive-convolve-2d)
    (void ((x :mat :input) (x-height int) (x-width int)
           (xd :mat :io)
           (w :mat :input) (w-height int) (w-width int)
           (wd :mat :io)
           (yd :mat :input) (y-height int) (y-width int)
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
            (let ((yde (aref yd yi)))
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
                                (let ((xii (+ xi xi-offset-0 d-column)))
                                  (incf (aref wd wi) (* yde (aref x xii)))
                                  (incf (aref xd xii) (* yde (aref w wi)))))))
                          (incf w-offset-i)
                          (incf wi))))))))
            (incf yi)
            (incf xi stride-1)
            (incf xi-1 stride-1)))))))

(defun derive-convolve! (x xd w wd yd &key start stride anchor batched)
  "Add the dF/dX to XD and and dF/dW to WD where YD is dF/dY for some
function F where Y is the result of convolution with the same
arguments. "
  (let* ((x-dimensions (if batched
                           (mat-dimensions x)
                           (cons 1 (mat-dimensions x))))
         (y-dimensions (if batched
                           (mat-dimensions yd)
                           (cons 1 (mat-dimensions yd))))
         (w-dimensions (mat-dimensions w))
         (n-dimensions (1- (length x-dimensions))))
    (check-dimensions x-dimensions w-dimensions y-dimensions
                      start stride anchor)
    (assert (equal (mat-dimensions x) (mat-dimensions xd)))
    (assert (equal (mat-dimensions w) (mat-dimensions wd)))
    (case n-dimensions
      ((2)
       (destructuring-bind (batch-size x-height x-width) x-dimensions
         (destructuring-bind (batch-size2 y-height y-width) y-dimensions
           (destructuring-bind (w-height w-width) w-dimensions
             (assert (= batch-size batch-size2))
             (if (use-cuda-p)
                 ;; FIXME: if STRIDE is one, then use the npp
                 ;; implementation.
                 (multiple-value-bind (block-dim grid-dim)
                     (choose-3d-block-and-grid (mat-dimensions yd) 4)
                   (cuda-derive-convolve-2d x x-height x-width
                                            xd
                                            w w-height w-width
                                            wd
                                            yd y-height y-width
                                            (elt start 0) (elt start 1)
                                            (elt stride 0) (elt stride 1)
                                            (elt anchor 0) (elt anchor 1)
                                            batch-size
                                            :grid-dim grid-dim
                                            :block-dim block-dim))
                 (lisp-derive-convolve-2d
                  x (mat-displacement x) x-height x-width
                  xd (mat-displacement xd)
                  w (mat-displacement w) w-height w-width
                  wd (mat-displacement wd)
                  yd (mat-displacement yd) y-height y-width
                  (elt start 0) (elt start 1)
                  (elt stride 0) (elt stride 1)
                  (elt anchor 0) (elt anchor 1)
                  batch-size))))))
      (t
       (error "Convolution not implemented for 1 + ~S dimensions."
              n-dimensions)))))
