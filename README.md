<a name='x-28MGL-MAT-3A-40MAT-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# MAT manual

## Table of Contents

- [1 mgl-mat ASDF System Details][85d5]
- [2 Introduction][b0b5]
    - [2.1 What's MGL-MAT?][01b0]
    - [2.2 What kind of matrices are supported?][dd58]
    - [2.3 Installation][d56c]
- [3 Basics][f966]
- [4 Element types][00a6]
- [5 Printing][6ccf]
- [6 Shaping][8866]
- [7 Assembling][8816]
- [8 Caching][e8e7]
- [9 Foreign arrays][4d1e]
- [10 CUDA][f291]
    - [10.1 CUBLAS][afa0]
    - [10.2 CURAND][caa5]
- [11 BLAS][0386]
- [12 Destructive API][e71c]
- [13 Non-destructive API][9984]
- [14 Mappings][7388]
- [15 Random numbers][ef83]
- [16 I/O][78d7]
- [17 Extension API][8b4f]
- [18 Debugging][fe72]

###### \[in package MGL-MAT\]
<a name='x-28-22mgl-mat-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 mgl-mat ASDF System Details

- Version: 0.0.1
- Description: MAT is library for working with multi-dimensional
  arrays which supports efficient interfacing to foreign and CUDA
  code. BLAS and CUBLAS bindings are available.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://quotenil.com](http://quotenil.com)

<a name='x-28MGL-MAT-3A-40MAT-INTRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 2 Introduction

<a name='x-28MGL-MAT-3A-40MAT-WHAT-IS-IT-20MGL-PAX-3ASECTION-29'></a>

### 2.1 What's MGL-MAT?

MGL-MAT is library for working with multi-dimensional arrays
which supports efficient interfacing to foreign and CUDA code with
automatic translations between cuda, foreign and lisp storage. BLAS
and CUBLAS bindings are available.

<a name='x-28MGL-MAT-3A-40MAT-WHAT-KIND-OF-MATRICES-20MGL-PAX-3ASECTION-29'></a>

### 2.2 What kind of matrices are supported?

Currently only row-major single and double float matrices are
supported, but it would be easy to add single and double precision
complex types too. Other numeric types, such as byte and native
integer, can be added too, but they are not supported by CUBLAS.
There are no restrictions on the number of dimensions, and reshaping
is possible. The CUBLAS functions operate on the visible portion of
the matrix (which is subject to displacement and shaping), invisible
elements are not affected.

<a name='x-28MGL-MAT-3A-40MAT-INSTALLATION-20MGL-PAX-3ASECTION-29'></a>

### 2.3 Installation

All dependencies are in quicklisp except for cl-cuda and some of
its dependencies: cl-pattern and cl-reexport which you need to get
from github.

<a name='x-28MGL-MAT-3A-40MAT-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 3 Basics

A [`MAT`][773f] is a [`CUBE`][9fcc] (see [Cube manual][7de8]) whose facets are different
representations of numeric arrays. These facets can be accessed with
[`WITH-FACETS`][b61b] with one of the following [`FACET-NAME`][21a4]s:

<a name='x-28MGL-MAT-3ABACKING-ARRAY-20MGL-CUBE-3AFACET-NAME-29'></a>

- [facet-name] **BACKING-ARRAY**

    The corresponding facet is a one dimensional lisp array.

<a name='x-28ARRAY-20MGL-CUBE-3AFACET-NAME-29'></a>

- [facet-name] **ARRAY**

    Same as [`BACKING-ARRAY`][e3f0] if the matrix is one-dimensional, all
    elements are visible (see [Shaping][8866]), else it's a lisp array
    displaced to the backing array.

<a name='x-28MGL-MAT-3AFOREIGN-ARRAY-20MGL-CUBE-3AFACET-NAME-29'></a>

- [facet-name] **FOREIGN-ARRAY**

    The facet is a [`FOREIGN-ARRAY`][7043] which is an `OFFSET-POINTER`
    wrapping a CFFI:POINTER. See [`*FOREIGN-ARRAY-STRATEGY*`][373b].

<a name='x-28MGL-MAT-3ACUDA-ARRAY-20MGL-CUBE-3AFACET-NAME-29'></a>

- [facet-name] **CUDA-ARRAY**

    The facet is [`CUDA-ARRAY`][84c3] which is an `OFFSET-POINTER` wrapping a
    `CL-CUDA.DRIVER-API:CU-DEVICE-PTR`, allocated with CU-MEM-ALLOC and
    freed automatically.
    
    Facets bound by with [`WITH-FACETS`][b61b] are to be treated as dynamic
    extent: it is not allowed to keep a reference to them beyond the
    dynamic scope of [`WITH-FACETS`][b61b].
    
    For example, to fill matrix X of [`CTYPE`][867d] `:DOUBLE` with ones it most
    convenient to work with the one dimensional [`BACKING-ARRAY`][e3f0]:
    
    ```
    (let ((displacement (mat-displacement x))
          (size (mat-size x)))
     (with-facets ((x* (x 'backing-array :direction :output)))
       (fill x* 1d0 :start displacement :end (+ displacement size))))
    ```
    
    [`DIRECTION`][298b] is `:OUTPUT` because we clobber all values in X. Armed with
    this knowledge about the direction, [`WITH-FACETS`][b61b] will not copy data
    from another facet if the backing array is not up-to-date.
    
    To transpose a 2d matrix with the [`ARRAY`][ba88] facet:
    
    ```
    (destructuring-bind (n-rows n-columns) (mat-dimensions x)
      (with-facets ((x* (x 'array :direction :io)))
        (dotimes (row n-rows)
          (dotimes (column n-columns)
            (setf (aref x* row column) (aref x* column row))))))
    ```
    
    Note that [`DIRECTION`][298b] is `:IO`, because we need the data in this facet
    to be up-to-date (that's the input part) and we are invalidating all
    other facets by changing values (that's the output part).
    
    To sum the values of a matrix using the `FOREIGN-ARRAY`([`0`][7043] [`1`][12c9]) facet:
    
    ```
    (let ((sum 0))
      (with-facets ((x* (x 'foreign-array :direction :input)))
        (let ((pointer (offset-pointer x*)))
          (loop for index below (mat-size x)
                do (incf sum (cffi:mem-aref pointer (mat-ctype x) index)))))
      sum)
    ```
    
    See [`DIRECTION`][298b] for a complete description of `:INPUT`, `:OUTPUT` and `:IO`.
    For [`MAT`][773f] objects, that needs to be refined. If a [`MAT`][773f] is reshaped
    and/or displaced in a way that not all elements are visible then
    those elements are always kept intact and copied around. This is
    accomplished by turning `:OUTPUT` into `:IO` automatically on such MATs.
    
    Most operations automatically use CUDA, if available and
    initialized. See [`WITH-CUDA*`][c00b] for detail.

<a name='x-28MGL-MAT-3AMAT-20CLASS-29'></a>

- [class] **MAT** *CUBE*

    A `MAT` is a data [`CUBE`][9fcc] that is much like a lisp
    array, it supports `DISPLACEMENT`, arbitrary `DIMENSIONS` and
    `INITIAL-ELEMENT` with the usual semantics. However, a `MAT` supports
    different representations of the same data. See [Basics][f966] for a
    tuturialish treatment.

<a name='x-28MGL-MAT-3AMAT-CTYPE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-CTYPE** *MAT* *(:CTYPE = \*DEFAULT-MAT-CTYPE\*)*

    One of [`*SUPPORTED-CTYPES*`][4586]. The matrix can hold
    only values of this type.

<a name='x-28MGL-MAT-3AMAT-DISPLACEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-DISPLACEMENT** *MAT* *(:DISPLACEMENT = 0)*

    A value in the [0,MAX-SIZE][] interval. This is like
    the DISPLACED-INDEX-OFFSET of a lisp array.

<a name='x-28MGL-MAT-3AMAT-DIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-DIMENSIONS** *MAT* *(:DIMENSIONS)*

    Like `ARRAY-DIMENSIONS`. It holds a list of
    dimensions, but it is allowed to pass in scalars too.

<a name='x-28MGL-MAT-3AMAT-DIMENSION-20FUNCTION-29'></a>

- [function] **MAT-DIMENSION** *MAT AXIS-NUMBER*

    Return the dimension along `AXIS-NUMBER`. Similar to
    `ARRAY-DIMENSION`.

<a name='x-28MGL-MAT-3AMAT-INITIAL-ELEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-INITIAL-ELEMENT** *MAT* *(:INITIAL-ELEMENT = 0)*

    If non-nil, then when a facet is created, it is
    filled with `INITIAL-ELEMENT` coerced to the appropriate numeric
    type. If `NIL`, then no initialization is performed.

<a name='x-28MGL-MAT-3AMAT-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-SIZE** *MAT*

    The number of elements in the visible portion of
    the array. This is always the product of the elements
    [`MAT-DIMENSIONS`][f5c1] and is similar to `ARRAY-TOTAL-SIZE`.

<a name='x-28MGL-MAT-3AMAT-MAX-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29'></a>

- [reader] **MAT-MAX-SIZE** *MAT* *(:MAX-SIZE)*

    The total size can be larger than [`MAT-SIZE`][1caf], but
    cannot change. Also `DISPLACEMENT` + `SIZE` must not exceed it. This
    is not

<a name='x-28MGL-MAT-3AMAKE-MAT-20FUNCTION-29'></a>

- [function] **MAKE-MAT** *DIMENSIONS &REST ARGS &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*) (DISPLACEMENT 0) MAX-SIZE (INITIAL-ELEMENT 0) INITIAL-CONTENTS (SYNCHRONIZATION \*DEFAULT-SYNCHRONIZATION\*)*

    Return a new matrix. If `INITIAL-CONTENTS` is given then the matrix
    contents are copied with [`REPLACE!`][c07a]. See class [`MAT`][773f] for the description
    of the rest of the parameters. This is exactly what (`MAKE-INSTANCE`
    '[`MAT`][773f] ...) does except `DIMENSIONS` is not a keyword argument so
    [`MAKE-MAT`][4cc3] looks more like `MAKE-ARRAY`. Also see
    [Synchronization][688d].

<a name='x-28MGL-MAT-3AARRAY-TO-MAT-20FUNCTION-29'></a>

- [function] **ARRAY-TO-MAT** *ARRAY &KEY CTYPE (SYNCHRONIZATION \*DEFAULT-SYNCHRONIZATION\*)*

    Create a [`MAT`][773f] that's equivalent to `ARRAY`. Displacement of the
    created array will be 0 and the size will be equal to
    `ARRAY-TOTAL-SIZE`. If `CTYPE` is non-nil, then it will be the ctype of
    the new matrix. Else `ARRAY`'s type is converted to a ctype. If there
    is no corresponding ctype, then [`*DEFAULT-MAT-CTYPE*`][02a7] is used.
    Elements of `ARRAY` are coerced to `CTYPE`.
    
    Also see [Synchronization][688d].

<a name='x-28MGL-MAT-3AMAT-TO-ARRAY-20FUNCTION-29'></a>

- [function] **MAT-TO-ARRAY** *MAT*

<a name='x-28MGL-MAT-3AREPLACE-21-20FUNCTION-29'></a>

- [function] **REPLACE!** *MAT SEQ-OF-SEQS*

    Replace the contents of `MAT` with the elements of `SEQ-OF-SEQS`.
    `SEQ-OF-SEQS` is a nested sequence of sequences similar to the
    `INITIAL-CONTENTS` argument of `MAKE-ARRAY`. The total number of
    elements must match the size of `MAT`. Returns `MAT`.
    
    `SEQ-OF-SEQS` may contain multi-dimensional arrays as *leafs*, so the
    following is legal:
    
    ```common-lisp
    (replace! (make-mat '(1 2 3)) '(#2A((1 2 3) (4 5 6))))
    ==> #<MAT 1x2x3 AB #3A(((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0)))>
    ```


<a name='x-28MGL-MAT-3AMREF-20FUNCTION-29'></a>

- [function] **MREF** *MAT &REST INDICES*

    Like `AREF` for arrays. Don't use this if you care about performance
    at all. SETFable. When set, the value is coerced to the ctype of `MAT`
    with [`COERCE-TO-CTYPE`][ad5b]. Note that currently [`MREF`][28eb] always operates on
    the [`BACKING-ARRAY`][e3f0] facet so it can trigger copying of facets. When
    it's `SETF`'ed, however, it will update the [`CUDA-ARRAY`][84c3] if cuda is
    enabled and it is up-to-date or there are no views at all.

<a name='x-28MGL-MAT-3AROW-MAJOR-MREF-20FUNCTION-29'></a>

- [function] **ROW-MAJOR-MREF** *MAT INDEX*

    Like `ROW-MAJOR-AREF` for arrays. Don't use this if you care about
    performance at all. SETFable. When set, the value is coerced to the
    ctype of `MAT` with [`COERCE-TO-CTYPE`][ad5b]. Note that currently
    [`ROW-MAJOR-MREF`][0ee2] always operates on the [`BACKING-ARRAY`][e3f0] facet so it can
    trigger copying of facets. When it's `SETF`'ed, however, it will
    update the [`CUDA-ARRAY`][84c3] if cuda is enabled and it is up-to-date or
    there are no views at all.

<a name='x-28MGL-MAT-3A-40MAT-CTYPES-20MGL-PAX-3ASECTION-29'></a>

## 4 Element types

<a name='x-28MGL-MAT-3A-2ASUPPORTED-CTYPES-2A-20VARIABLE-29'></a>

- [variable] **\*SUPPORTED-CTYPES\*** *(:FLOAT :DOUBLE)*

<a name='x-28MGL-MAT-3ACTYPE-20TYPE-29'></a>

- [type] **CTYPE**

<a name='x-28MGL-MAT-3A-2ADEFAULT-MAT-CTYPE-2A-20VARIABLE-29'></a>

- [variable] **\*DEFAULT-MAT-CTYPE\*** *:DOUBLE*

    By default MATs are created with this ctype. One of `:FLOAT`
    or `:DOUBLE` (the default).

<a name='x-28MGL-MAT-3ACOERCE-TO-CTYPE-20FUNCTION-29'></a>

- [function] **COERCE-TO-CTYPE** *X &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*)*

    Coerce the scalar `X` to the lisp type corresponding to `CTYPE`.

<a name='x-28MGL-MAT-3A-40MAT-PRINTING-20MGL-PAX-3ASECTION-29'></a>

## 5 Printing

<a name='x-28MGL-MAT-3A-2APRINT-MAT-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-MAT\*** *T*

    Controls whether the contents of a [`MAT`][773f] object are printed as an
    array (subject to the standard printer control variables).

<a name='x-28MGL-MAT-3A-2APRINT-MAT-FACETS-2A-20VARIABLE-29'></a>

- [variable] **\*PRINT-MAT-FACETS\*** *T*

    Controls whether a summary of existing and up-to-date views is
    printed whe a [`MAT`][773f] object is printed. The summary that looks like
    `ABcf` indicates that all four facets ([`ARRAY`][ba88], [`BACKING-ARRAY`][e3f0],
    [`CUDA-ARRAY`][84c3], `FOREIGN-ARRAY`([`0`][7043] [`1`][12c9])) are present and the first two are
    up-to-date. A summary of a single #- indicates that there are no
    facets.

<a name='x-28MGL-MAT-3A-40MAT-SHAPING-20MGL-PAX-3ASECTION-29'></a>

## 6 Shaping

Reshaping and displacement of [`MAT`][773f] objects works somewhat similarly
to lisp arrays. The key difference is that they are destructive
operations. See [`RESHAPE-AND-DISPLACE!`][0f32], [`RESHAPE!`][58e7], [`DISPLACE!`][4802],
[`RESHAPE-TO-ROW-MATRIX!`][58bb] and [`WITH-SHAPE-AND-DISPLACEMENT`][c246]. [`ADJUST!`][52cb] is
the odd one out, it may create a new [`MAT`][773f].

Existing facets are adjusted by all operations. For LISP-ARRAY
facets, this means creating a new lisp array displaced to the
backing array. The backing array stays the same, clients are
supposed to observe [`MAT-DISPLACEMENT`][0521], [`MAT-DIMENSIONS`][f5c1] or [`MAT-SIZE`][1caf].
The `FOREIGN-ARRAY`([`0`][7043] [`1`][12c9]) and [`CUDA-ARRAY`][84c3] facets are `OFFSET-POINTER`'s so
displacement is done by changing the offset. Clients need to observe
[`MAT-DIMENSIONS`][f5c1] in any case.

<a name='x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-21-20FUNCTION-29'></a>

- [function] **RESHAPE-AND-DISPLACE!** *MAT DIMENSIONS DISPLACEMENT*

    Change the visible (or active) portion of `MAT` by altering its
    displacement offset and dimensions. Future operations will only
    affect this visible portion as if the rest of the elements were not
    there. Return `MAT`.
    
    `DISPLACEMENT` + the new size must not exceed [`MAT-MAX-SIZE`][3cf7].
    Furthermore, there must be no facets being viewed (with [`WITH-FACETS`][b61b])
    when calling this function as the identity of the facets is not
    stable.

<a name='x-28MGL-MAT-3ARESHAPE-21-20FUNCTION-29'></a>

- [function] **RESHAPE!** *MAT DIMENSIONS*

    Like [`RESHAPE-AND-DISPLACE!`][0f32] but only alters the dimensions.

<a name='x-28MGL-MAT-3ADISPLACE-21-20FUNCTION-29'></a>

- [function] **DISPLACE!** *MAT DISPLACEMENT*

    Like [`RESHAPE-AND-DISPLACE!`][0f32] but only alters the displacement.

<a name='x-28MGL-MAT-3ARESHAPE-TO-ROW-MATRIX-21-20FUNCTION-29'></a>

- [function] **RESHAPE-TO-ROW-MATRIX!** *MAT ROW*

    Reshape the 2d `MAT` to make only a single `ROW` visible. This is made
    possible by the row-major layout, hence no column counterpart.
    Return `MAT`.

<a name='x-28MGL-MAT-3AWITH-SHAPE-AND-DISPLACEMENT-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-SHAPE-AND-DISPLACEMENT** *(MAT &OPTIONAL (DIMENSIONS NIL DIMENSIONSP) (DISPLACEMENT NIL DISPLACEMENTP)) &BODY BODY*

    Reshape and displace `MAT` if `DIMENSIONS` and/or `DISPLACEMENT` is given
    and restore the original shape and displacement after `BODY` is
    executed. If neither is specificed, then nothing will be changed,
    but `BODY` is still allowed to alter the shape and displacement.

<a name='x-28MGL-MAT-3AADJUST-21-20FUNCTION-29'></a>

- [function] **ADJUST!** *MAT DIMENSIONS DISPLACEMENT &KEY (DESTROY-OLD-P T)*

    Like [`RESHAPE-AND-DISPLACE!`][0f32] but creates a new matrix if `MAT` isn't
    large enough. If a new matrix is created, the contents are not
    copied over and the old matrix is destroyed with [`DESTROY-CUBE`][2cb4] if
    `DESTROY-OLD-P`.

<a name='x-28MGL-MAT-3A-40MAT-ASSEMBLING-20MGL-PAX-3ASECTION-29'></a>

## 7 Assembling

The functions here assemble a single [`MAT`][773f] from a number of
[`MAT`][773f]s.

<a name='x-28MGL-MAT-3ASTACK-21-20FUNCTION-29'></a>

- [function] **STACK!** *AXIS MATS MAT*

    Stack `MATS` along `AXIS` into `MAT` and return `MAT`. If `AXIS` is 0, place
    `MATS` into `MAT` below each other starting from the top. If `AXIS` is 1,
    place `MATS` side by side starting from the left. Higher `AXIS` are also
    supported. All dimensions except for `AXIS` must be the same for all
    `MATS`.

<a name='x-28MGL-MAT-3ASTACK-20FUNCTION-29'></a>

- [function] **STACK** *AXIS MATS &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*)*

    Like [`STACK!`][552a] but return a new [`MAT`][773f] of `CTYPE`.
    
    ```commonlisp
    (stack 1 (list (make-mat '(3 2) :initial-element 0)
                   (make-mat '(3 1) :initial-element 1)))
    ==> #<MAT 3x3 B #2A((0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0))>
    ```


<a name='x-28MGL-MAT-3A-40MAT-CACHING-20MGL-PAX-3ASECTION-29'></a>

## 8 Caching

Allocating and initializing a [`MAT`][773f] object and its necessary facets
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
particularly efficient but at least it's safe.

<a name='x-28MGL-MAT-3AWITH-THREAD-CACHED-MAT-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-THREAD-CACHED-MAT** *(VAR DIMENSIONS &REST ARGS &KEY (PLACE :SCRATCH) (CTYPE '\*DEFAULT-MAT-CTYPE\*) (DISPLACEMENT 0) MAX-SIZE (INITIAL-ELEMENT 0) INITIAL-CONTENTS) &BODY BODY*

    Bind `VAR` to a matrix of `DIMENSIONS`, `CTYPE`, etc. Cache this matrix,
    and possibly reuse it later by reshaping it. When `BODY` exits the
    cached object is updated with the binding of `VAR` which `BODY` may
    change.

<a name='x-28MGL-MAT-3AWITH-ONES-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-ONES** *(VAR DIMENSIONS &KEY (CTYPE '\*DEFAULT-MAT-CTYPE\*) (PLACE :ONES)) &BODY BODY*

    Bind `VAR` to a matrix of `DIMENSIONS` whose every element is 1. The
    matrix is cached for efficiency.

<a name='x-28MGL-MAT-3A-40MAT-FOREIGN-20MGL-PAX-3ASECTION-29'></a>

## 9 Foreign arrays

One facet of [`MAT`][773f] objects is [`FOREIGN-ARRAY`][12c9] which is
backed by a memory area that can be pinned or is allocated in
foreign memory depending on [`*FOREIGN-ARRAY-STRATEGY*`][373b].

<a name='x-28MGL-MAT-3AFOREIGN-ARRAY-20CLASS-29'></a>

- [class] **FOREIGN-ARRAY** *OFFSET-POINTER*

    [`FOREIGN-ARRAY`][7043] wraps a foreign pointer (in
    the sense of `CFFI:POINTERP`). That is, both `OFFSET-POINTER` and
    `BASE-POINTER` return a foreign pointer. There are no other public
    operations that work with [`FOREIGN-ARRAY`][7043] objects, their sole
    purpose is represent facets of [`MAT`][773f] objects.

<a name='x-28MGL-MAT-3A-2AFOREIGN-ARRAY-STRATEGY-2A-20-28VARIABLE-20-22-see-20below--22-29-29'></a>

- [variable] **\*FOREIGN-ARRAY-STRATEGY\*** *"-see below-"*

    One of `:PIN-BACKING-ARRAY`, `:STATIC-BACKING-ARRAY` and :ALLOCATE (see
    type [`FOREIGN-ARRAY-STRATEGY`][c65e]). This variable controls how foreign
    arrays are handled and it can be changed at any time.
    
    If it's `:PIN-BACKING-ARRAY` (only supported if ([`PINNING-SUPPORTED-P`][1227]),
    then no separate storage is allocated for the foreign array, instead
    it aliases the lisp array (via the [`BACKING-ARRAY`][e3f0] facet).
    
    If it's `:STATIC-BACKING-ARRAY`, then the lisp backing arrays are
    allocated statically via the static-vectors library. On some
    implementations, explicit freeing of static vectors is necessary,
    this is taken care of by finalizers or can be controlled with
    [`WITH-FACET-BARRIER`][99b9].
    
    If it's `:DYNAMIC`, then each time the foreign array is needed, it's
    allocated and freed dynamically.
    
    The default is `:PIN-BACKING-ARRAY` if available, because it's the most
    effecient. If pinning is not available, then
    it's `:STATIC-BACKING-ARRAY`.

<a name='x-28MGL-MAT-3AFOREIGN-ARRAY-STRATEGY-20TYPE-29'></a>

- [type] **FOREIGN-ARRAY-STRATEGY**

    One of `:PIN-BACKING-ARRAY`, `:STATIC-BACKING-ARRAY`, `:DYNAMIC`. See
    [`*FOREIGN-ARRAY-STRATEGY*`][373b] for their semantics.

<a name='x-28MGL-MAT-3APINNING-SUPPORTED-P-20FUNCTION-29'></a>

- [function] **PINNING-SUPPORTED-P** 

    Return true iff the lisp implementation efficiently supports
    pinning lisp arrays. Pinning ensures that the garbage collector
    doesn't move the array in memory. Currently this is only supported on
    SBCL gencgc platforms.

<a name='x-28MGL-MAT-3A-40MAT-CUDA-20MGL-PAX-3ASECTION-29'></a>

## 10 CUDA

<a name='x-28MGL-MAT-3ACUDA-AVAILABLE-P-20FUNCTION-29'></a>

- [function] **CUDA-AVAILABLE-P** *&KEY (DEVICE-ID 0)*

    Check a cuda context is already in initialized in the current
    thread or a device with `DEVICE-ID` is available.

<a name='x-28MGL-MAT-3AWITH-CUDA-2A-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-CUDA\*** *(&KEY (ENABLED '\*CUDA-ENABLED\*) (DEVICE-ID \*CUDA-DEFAULT-DEVICE-ID\*) (RANDOM-SEED \*CUDA-DEFAULT-RANDOM-SEED\*) (N-RANDOM-STATES \*CUDA-DEFAULT-N-RANDOM-STATES\*) (OVERRIDE-ARCH-P T)) &BODY BODY*

    Initializes cuda with with all bells and whistles before `BODY` and
    deinitializes it after. Simply wrapping [`WITH-CUDA*`][c00b] around a piece
    code is enough to make use of the first available cuda device or
    fall back on blas and lisp kernels if there is none.
    
    If cuda is already initialized, then it sets up a facet barrier
    which destroys [`CUDA-ARRAY`][84c3] facets after ensuring that the [`ARRAY`][ba88] facet
    is up-to-date.
    
    Else, if cuda is available and `ENABLED`, then in addition to the
    facet barrier, a cuda context is set up, [`*N-MEMCPY-HOST-TO-DEVICE*`][ee37],
    [`*N-MEMCPY-DEVICE-TO-HOST*`][5d9b] are bound to zero, the highest possible
    -arch option for the device is added to *CL-CUDA:NVCC-OPTIONS* (if
    `OVERRIDE-ARCH-P`), a cublas handle created, and [`*CURAND-STATE*`][e868] is
    bound to a [`CURAND-XORWOW-STATE`][fa6c] with `N-RANDOM-STATES`, seeded with
    `RANDOM-SEED`.
    
    Else - that is, if cuda is not available -, `BODY` is simply
    executed.

<a name='x-28MGL-MAT-3ACALL-WITH-CUDA-20FUNCTION-29'></a>

- [function] **CALL-WITH-CUDA** *FN &KEY ((:ENABLED \*CUDA-ENABLED\*) \*CUDA-ENABLED\*) (DEVICE-ID \*CUDA-DEFAULT-DEVICE-ID\*) (RANDOM-SEED \*CUDA-DEFAULT-RANDOM-SEED\*) (N-RANDOM-STATES \*CUDA-DEFAULT-N-RANDOM-STATES\*) (OVERRIDE-ARCH-P T)*

    Like [`WITH-CUDA*`][c00b], but takes a no argument function instead of the
    macro's `BODY`.

<a name='x-28MGL-MAT-3A-2ACUDA-ENABLED-2A-20VARIABLE-29'></a>

- [variable] **\*CUDA-ENABLED\*** *T*

    Set or bind this to false to disable all use of cuda. If this is
    done from within `WITH-CUDA`*, then cuda becomes temporarily disabled.
    If this is done from outside `WITH-CUDA`*, then it changes the default
    values of the `ENABLED` argument of any future [`WITH-CUDA*`][c00b]s which
    turns off cuda initialization entirely.

<a name='x-28MGL-MAT-3AUSE-CUDA-P-20FUNCTION-29'></a>

- [function] **USE-CUDA-P** 

    Return true if cuda is enabled ([`*CUDA-ENABLED*`][5feb]) and it's
    initialized. [`MAT`][773f] operations use this to decide whether to go for the
    cuda implementation or BLAS/Lisp. It's provided for implementing new
    operations.

<a name='x-28MGL-MAT-3A-2AN-MEMCPY-HOST-TO-DEVICE-2A-20VARIABLE-29'></a>

- [variable] **\*N-MEMCPY-HOST-TO-DEVICE\*** *0*

    Incremented each time a host to device copy is performed. Bound to
    0 by [`WITH-CUDA*`][c00b]. Useful for tracking down performance problems.

<a name='x-28MGL-MAT-3A-2AN-MEMCPY-DEVICE-TO-HOST-2A-20VARIABLE-29'></a>

- [variable] **\*N-MEMCPY-DEVICE-TO-HOST\*** *0*

    Incremented each time a device to host copy is performed. Bound to
    0 by [`WITH-CUDA*`][c00b]. Useful for tracking down performance problems.

<a name='x-28MGL-MAT-3ACHOOSE-1D-BLOCK-AND-GRID-20FUNCTION-29'></a>

- [function] **CHOOSE-1D-BLOCK-AND-GRID** *N MAX-N-WARPS-PER-BLOCK*

    Return two values, one suitable as the `:BLOCK-DIM`, the other as
    the `:GRID-DIM` argument for a cuda kernel call where both are
    one-dimensional (only the first element may be different from 1).
    
    The number of threads in a block is a multiple of `*CUDA-WARP-SIZE*`.
    The number of blocks is between 1 and and `*CUDA-MAX-N-BLOCKS*`. This
    means that the kernel must be able handle any number of elements in
    each thread. For example, a strided kernel that adds a constant to
    each element of a length `N` vector looks like this:
    
    ```
    (let ((stride (* block-dim-x grid-dim-x)))
      (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
              (+ i stride)))
          ((>= i n))
        (set (aref x i) (+ (aref x i) alpha))))
    ```
    
    It is often the most efficient to have `MAX-N-WARPS-PER-BLOCK` around
    4. Note that the maximum number of threads per block is limited by
    hardware (512 for compute capability < 2.0, 1024 for later
    versions), so `*CUDA-MAX-N-BLOCKS*` times `MAX-N-WARPS-PER-BLOCK` must
    not exceed that limit.

<a name='x-28MGL-MAT-3ACHOOSE-2D-BLOCK-AND-GRID-20FUNCTION-29'></a>

- [function] **CHOOSE-2D-BLOCK-AND-GRID** *DIMENSIONS MAX-N-WARPS-PER-BLOCK*

    Return two values, one suitable as the `:BLOCK-DIM`, the other as
    the `:GRID-DIM` argument for a cuda kernel call where both are
    two-dimensional (only the first two elements may be different from
    1).
    
    The number of threads in a block is a multiple of `*CUDA-WARP-SIZE*`.
    The number of blocks is between 1 and and `*CUDA-MAX-N-BLOCKS*`.
    Currently - but this may change - the `BLOCK-DIM-X` is always
    `*CUDA-WARP-SIZE*` and `GRID-DIM-X` is always 1.
    
    This means that the kernel must be able handle any number of
    elements in each thread. For example, a strided kernel that adds a
    constant to each element of a HEIGHT\*WIDTH matrix looks like this:
    
    ```
    (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
          (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
          (stride-x (* block-dim-x grid-dim-x))
          (stride-y (* block-dim-y grid-dim-y)))
      (do ((row id-y (+ row stride-y)))
          ((>= row height))
        (let ((i (* row width)))
          (do ((column id-x (+ column stride-x)))
              ((>= column width))
            (set (aref x i) (+ (aref x i) alpha))
            (incf i stride-x)))))
    ```


<a name='x-28MGL-MAT-3ACHOOSE-3D-BLOCK-AND-GRID-20FUNCTION-29'></a>

- [function] **CHOOSE-3D-BLOCK-AND-GRID** *DIMENSIONS MAX-N-WARPS-PER-BLOCK*

    Return two values, one suitable as the `:BLOCK-DIM`, the other as
    the `:GRID-DIM` argument for a cuda kernel call where both are
    two-dimensional (only the first two elements may be different from
    1).
    
    The number of threads in a block is a multiple of `*CUDA-WARP-SIZE*`.
    The number of blocks is between 1 and and `*CUDA-MAX-N-BLOCKS*`.
    Currently - but this may change - the `BLOCK-DIM-X` is always
    `*CUDA-WARP-SIZE*` and `GRID-DIM-X` is always 1.
    
    This means that the kernel must be able handle any number of
    elements in each thread. For example, a strided kernel that adds a
    constant to each element of a `THICKNESS` \* `HEIGHT` \* `WIDTH` 3d array
    looks like this:
    
    ```
    (let ((id-x (+ (* block-dim-x block-idx-x) thread-idx-x))
          (id-y (+ (* block-dim-y block-idx-y) thread-idx-y))
          (id-z (+ (* block-dim-z block-idx-z) thread-idx-z))
          (stride-x (* block-dim-x grid-dim-x))
          (stride-y (* block-dim-y grid-dim-y))
          (stride-z (* block-dim-z grid-dim-z)))
      (do ((plane id-z (+ plane stride-z)))
          ((>= plane thickness))
        (do ((row id-y (+ row stride-y)))
            ((>= row height))
          (let ((i (* (+ (* plane height) row)
                      width)))
            (do ((column id-x (+ column stride-x)))
                ((>= column width))
              (set (aref x i) (+ (aref x i) alpha))
              (incf i stride-x))))))
    ```


<a name='x-28MGL-MAT-3ACUDA-OUT-OF-MEMORY-20CONDITION-29'></a>

- [condition] **CUDA-OUT-OF-MEMORY** *STORAGE-CONDITION*

<a name='x-28MGL-MAT-3A-2ACUDA-DEFAULT-DEVICE-ID-2A-20VARIABLE-29'></a>

- [variable] **\*CUDA-DEFAULT-DEVICE-ID\*** *0*

    The default value of [`WITH-CUDA*`][c00b]'s `:DEVICE-ID` argument.

<a name='x-28MGL-MAT-3A-2ACUDA-DEFAULT-RANDOM-SEED-2A-20VARIABLE-29'></a>

- [variable] **\*CUDA-DEFAULT-RANDOM-SEED\*** *1234*

    The default value of [`WITH-CUDA*`][c00b]'s `:RANDOM-SEED` argument.

<a name='x-28MGL-MAT-3A-2ACUDA-DEFAULT-N-RANDOM-STATES-2A-20VARIABLE-29'></a>

- [variable] **\*CUDA-DEFAULT-N-RANDOM-STATES\*** *4096*

    The default value of [`WITH-CUDA*`][c00b]'s `:N-RANDOM-STATES` argument.

<a name='x-28MGL-MAT-3A-40MAT-CUBLAS-20MGL-PAX-3ASECTION-29'></a>

### 10.1 CUBLAS

[`WITH-CUDA*`][c00b] should take of everything. No need to use these at all
unless you have a very good reason to bypass it.

<a name='x-28MGL-MAT-3ACUBLAS-ERROR-20CONDITION-29'></a>

- [condition] **CUBLAS-ERROR** *ERROR*

<a name='x-28MGL-MAT-3ACUBLAS-ERROR-FUNCTION-NAME-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29'></a>

- [reader] **CUBLAS-ERROR-FUNCTION-NAME** *CUBLAS-ERROR* *(:FUNCTION-NAME)*

<a name='x-28MGL-MAT-3ACUBLAS-ERROR-STATUS-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29'></a>

- [reader] **CUBLAS-ERROR-STATUS** *CUBLAS-ERROR* *(:STATUS)*

<a name='x-28MGL-MAT-3A-2ACUBLAS-HANDLE-2A-20VARIABLE-29'></a>

- [variable] **\*CUBLAS-HANDLE\*** *"-unbound-"*

<a name='x-28MGL-MAT-3ACUBLAS-CREATE-20FUNCTION-29'></a>

- [function] **CUBLAS-CREATE** *HANDLE*

<a name='x-28MGL-MAT-3ACUBLAS-DESTROY-20FUNCTION-29'></a>

- [function] **CUBLAS-DESTROY** *&KEY (HANDLE \*CUBLAS-HANDLE\*)*

<a name='x-28MGL-MAT-3AWITH-CUBLAS-HANDLE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-CUBLAS-HANDLE** *NIL &BODY BODY*

<a name='x-28MGL-MAT-3ACUBLAS-GET-VERSION-20FUNCTION-29'></a>

- [function] **CUBLAS-GET-VERSION** *VERSION &KEY (HANDLE \*CUBLAS-HANDLE\*)*

<a name='x-28MGL-MAT-3A-40MAT-CURAND-20MGL-PAX-3ASECTION-29'></a>

### 10.2 CURAND

This the low level CURAND API.

<a name='x-28MGL-MAT-3AWITH-CURAND-STATE-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-CURAND-STATE** *(STATE) &BODY BODY*

<a name='x-28MGL-MAT-3A-2ACURAND-STATE-2A-20VARIABLE-29'></a>

- [variable] **\*CURAND-STATE\*** *"-unbound-"*

<a name='x-28MGL-MAT-3ACURAND-XORWOW-STATE-20CLASS-29'></a>

- [class] **CURAND-XORWOW-STATE**

<a name='x-28MGL-MAT-3AN-STATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29'></a>

- [reader] **N-STATES** *CURAND-XORWOW-STATE* *(:N-STATES)*

<a name='x-28MGL-MAT-3ASTATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29'></a>

- [reader] **STATES** *CURAND-XORWOW-STATE* *(:STATES)*

<a name='x-28MGL-MAT-3A-40MAT-BLAS-20MGL-PAX-3ASECTION-29'></a>

## 11 BLAS

Only some BLAS functions are implemented, but it should be easy to
add more as needed. All of them default to using CUDA, if it is
initialized and enabled (see [`USE-CUDA-P`][51e4]).

Level 1 BLAS operations

<a name='x-28MGL-MAT-3AASUM-20FUNCTION-29'></a>

- [function] **ASUM** *X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Return the l1 norm of `X`, that is, sum of the absolute values of its
    elements.

<a name='x-28MGL-MAT-3AAXPY-21-20FUNCTION-29'></a>

- [function] **AXPY!** *ALPHA X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Set `Y` to `ALPHA` \* `X` + `Y`. Return `Y`.

<a name='x-28MGL-MAT-3ACOPY-21-20FUNCTION-29'></a>

- [function] **COPY!** *X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Copy `X` into `Y`. Return `Y`.

<a name='x-28CL-CUDA-2ELANG-2EBUILT-IN-3ADOT-20FUNCTION-29'></a>

- [function] **DOT** *X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Return the dot product of `X` and `Y`.

<a name='x-28MGL-MAT-3ANRM2-20FUNCTION-29'></a>

- [function] **NRM2** *X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Return the l2 norm of `X`, which is the square root of the sum of the
    squares of its elements.

<a name='x-28MGL-MAT-3ASCAL-21-20FUNCTION-29'></a>

- [function] **SCAL!** *ALPHA X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Set `X` to `ALPHA` \* `X`. Return `X`.

Level 3 BLAS operations

<a name='x-28MGL-MAT-3AGEMM-21-20FUNCTION-29'></a>

- [function] **GEMM!** *ALPHA A B BETA C &KEY TRANSPOSE-A? TRANSPOSE-B? M N K LDA LDB LDC*

    Basically `C` = `ALPHA` \* `A`' \* `B`' + `BETA` \* `C`. `A`' is `A` or its transpose
    depending on `TRANSPOSE-A?`. `B`' is `B` or its transpose depending on
    `TRANSPOSE-B?`. Returns `C`.
    
    `A`' is an MxK matrix. `B`' is a KxN matrix. `C` is an MxN matrix.
    
    `LDA` is the width of the matrix `A` (not of `A`'). If `A` is not transposed,
    then `K` <= `LDA`, if it's transposed then `M` <= `LDA`.
    
    `LDB` is the width of the matrix `B` (not of `B`'). If `B` is not transposed,
    then `N` <= `LDB`, if it's transposed then `K` <= `LDB`.
    
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
          ++++++  ++++


<a name='x-28MGL-MAT-3A-40MAT-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29'></a>

## 12 Destructive API

<a name='x-28MGL-MAT-3A-2ESQUARE-21-20FUNCTION-29'></a>

- [function] **.SQUARE!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise square. Return `X`.

<a name='x-28MGL-MAT-3A-2ESQRT-21-20FUNCTION-29'></a>

- [function] **.SQRT!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise square root. Return `X`.

<a name='x-28MGL-MAT-3A-2ELOG-21-20FUNCTION-29'></a>

- [function] **.LOG!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise natural logarithm. Return `X`.

<a name='x-28MGL-MAT-3A-2EEXP-21-20FUNCTION-29'></a>

- [function] **.EXP!** *X &KEY (N (MAT-SIZE X))*

    Apply `EXP` elementwise to `X` in a destructive manner. Return `X`.

<a name='x-28MGL-MAT-3A-2ELOGISTIC-21-20FUNCTION-29'></a>

- [function] **.LOGISTIC!** *X &KEY (N (MAT-SIZE X))*

    Destructively apply the logistic function to `X` in an elementwise
    manner. Return `X`.

<a name='x-28MGL-MAT-3A-2E-2B-21-20FUNCTION-29'></a>

- [function] **.+!** *ALPHA X*

    Add the scalar `ALPHA` to each element of `X` destructively modifying
    `X`. Return `X`.

<a name='x-28MGL-MAT-3A-2E-2A-21-20FUNCTION-29'></a>

- [function] **.\*!** *X Y*

<a name='x-28MGL-MAT-3AGEEM-21-20FUNCTION-29'></a>

- [function] **GEEM!** *ALPHA A B BETA C*

    Like [`GEMM!`][acfb], but multiplication is elementwise. This is not a
    standard BLAS routine.

<a name='x-28MGL-MAT-3AGEERV-21-20FUNCTION-29'></a>

- [function] **GEERV!** *ALPHA A X BETA B*

    GEneric Elementwise Row - Vector multiplication. `B` = beta \* `B` + alpha \* a
    \* diag(x). In other words, perform elementwise multiplication on
    each row of `A` with the vector `X` and add the scaled result to the
    corresponding row of `B`. Return `B`. This is not a standard BLAS
    routine.

<a name='x-28MGL-MAT-3A-2E-3C-21-20FUNCTION-29'></a>

- [function] **.\<!** *X Y*

    For each element of `X` and `Y` set `Y` to 1 if the element in `Y` is
    greater than the element in `X`, and to 0 otherwise. Return `Y`.

<a name='x-28MGL-MAT-3A-2EMIN-21-20FUNCTION-29'></a>

- [function] **.MIN!** *ALPHA X*

    Set each element of `X` to `ALPHA` if it's greater than `ALPHA`. Return
    `X`.

<a name='x-28MGL-MAT-3A-2EMAX-21-20FUNCTION-29'></a>

- [function] **.MAX!** *ALPHA X*

    Set each element of `X` to `ALPHA` if it's less than `ALPHA`. Return `X`.

<a name='x-28MGL-MAT-3AADD-SIGN-21-20FUNCTION-29'></a>

- [function] **ADD-SIGN!** *ALPHA A BETA B*

    Add the elementwise sign (-1, 0 or 1 for negative, zero and
    positive numbers respectively) of `A` times `ALPHA` to `BETA` \* `B`. Return
    `B`.

<a name='x-28MGL-MAT-3AFILL-21-20FUNCTION-29'></a>

- [function] **FILL!** *ALPHA X &KEY (N (MAT-SIZE X))*

    Fill matrix `X` with `ALPHA`. Return `X`.

<a name='x-28MGL-MAT-3ASUM-21-20FUNCTION-29'></a>

- [function] **SUM!** *X Y &KEY AXIS (ALPHA 1) (BETA 0)*

    Sum matrix `X` along `AXIS` and add `ALPHA` \* SUMS to `BETA` \* `Y`
    destructively modifying `Y`. Return `Y`. On a 2d matrix (nothing else is
    supported currently), if `AXIS` is 0, then columns are summed, if `AXIS`
    is 1 then rows are summed.

<a name='x-28MGL-MAT-3ASCALE-ROWS-21-20FUNCTION-29'></a>

- [function] **SCALE-ROWS!** *SCALES A B*

Finally, some neural network operations.

<a name='x-28MGL-MAT-3ACONVOLVE-21-20FUNCTION-29'></a>

- [function] **CONVOLVE!** *X W Y &KEY START STRIDE ANCHOR BATCHED*

    `Y` = `Y` + conv(`X`, `W`) and return `Y`. If `BATCHED`, then the first
    dimension of `X` and `Y` is the number of elements in the batch (B),
    else B is assumed to be 1. The rest of the dimensions encode the
    input (`X`) and output (Y} N dimensional feature maps. `START`, `STRIDE`
    and `ANCHOR` are lists of length N. `START` is the multi-dimensional
    index of the first element of the input feature map (for each
    element in the batch) for which the convolution must be computed.
    Then (`ELT` `STRIDE` (- N 1)) is added to the last element of `START` and
    so on until (`ARRAY-DIMENSION` `X` 1) is reached. Then the last element
    of `START` is reset, (`ELT` `STRIDE` (- N 2)) is added to the first but
    last element of `START` and we scan the last dimension again. Take a
    2d example, `START` is (0 0), `STRIDE` is (1 2), and `X` is a B\*2x7
    matrix.
    
    `W` is:
    
        1 2 1
        2 4 2
        1 2 1
    
    and `ANCHOR` is (1 1) which refers to the element of `W` whose value is
    4. This anchor point of `W` is placed over elements of `X` whose multi
    dimensional index is in numbers in this figure (only one element in
    the batch is shown):
    
        0,0 . 0,2 . 0,4 . 0,6
        1,0 . 1,2 . 1,4 . 1,6
    
    When applying `W` at position P of `X`, the convolution is the sum of
    the products of overlapping elements of `X` and `W` when `W`'s `ANCHOR` is
    placed at P. Elements of `W` over the edges of `X` are multiplied with 0
    so are effectively ignored. The order of application of `W` to
    positions defined by `START`, `STRIDE` and `ANCHOR` is undefined.
    
    `Y` must be a B\*2x4 (or 2x4 if not `BATCHED`) matrix in this example,
    just large enough to hold the results of the convolutions.

<a name='x-28MGL-MAT-3ADERIVE-CONVOLVE-21-20FUNCTION-29'></a>

- [function] **DERIVE-CONVOLVE!** *X XD W WD YD &KEY START STRIDE ANCHOR BATCHED*

    Add the dF/dX to `XD` and and dF/dW to `WD` where `YD` is dF/dY for some
    function F where Y is the result of convolution with the same
    arguments. 

<a name='x-28MGL-MAT-3AMAX-POOL-21-20FUNCTION-29'></a>

- [function] **MAX-POOL!** *X Y &KEY START STRIDE ANCHOR BATCHED POOL-DIMENSIONS*



<a name='x-28MGL-MAT-3ADERIVE-MAX-POOL-21-20FUNCTION-29'></a>

- [function] **DERIVE-MAX-POOL!** *X XD Y YD &KEY START STRIDE ANCHOR BATCHED POOL-DIMENSIONS*

    Add the dF/dX to `XD` and and dF/dW to WD where `YD` is dF/dY for some
    function F where `Y` is the result of [`MAX-POOL!`][36b8] with the same
    arguments. 

<a name='x-28MGL-MAT-3A-40MAT-NON-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29'></a>

## 13 Non-destructive API

<a name='x-28MGL-MAT-3ACOPY-MAT-20FUNCTION-29'></a>

- [function] **COPY-MAT** *A*

    Return a copy of the active portion with regards to displacement
    and shape of `A`. 

<a name='x-28MGL-MAT-3ACOPY-ROW-20FUNCTION-29'></a>

- [function] **COPY-ROW** *A ROW*

    Return `ROW` of `A` as a new 1d matrix.

<a name='x-28MGL-MAT-3ACOPY-COLUMN-20FUNCTION-29'></a>

- [function] **COPY-COLUMN** *A COLUMN*

    Return `COLUMN` of `A` as a new 1d matrix.

<a name='x-28MGL-MAT-3AMAT-AS-SCALAR-20FUNCTION-29'></a>

- [function] **MAT-AS-SCALAR** *A*

    Return the first element of `A`. `A` must be of size 1.

<a name='x-28MGL-MAT-3ASCALAR-AS-MAT-20FUNCTION-29'></a>

- [function] **SCALAR-AS-MAT** *X &KEY (CTYPE (LISP-\>CTYPE (TYPE-OF X)))*

    Return a matrix of one dimension and one element: `X`. `CTYPE`, the
    type of the matrix, defaults to the ctype corresponding to the type
    of `X`.

<a name='x-28MGL-MAT-3AM-3D-20FUNCTION-29'></a>

- [function] **M=** *A B*

    Check whether `A` and `B`, which must be matrices of the same size, are
    elementwise equal.

<a name='x-28MGL-MAT-3ATRANSPOSE-20FUNCTION-29'></a>

- [function] **TRANSPOSE** *A*

    Return the transpose of `A`.

<a name='x-28MGL-MAT-3AM-2A-20FUNCTION-29'></a>

- [function] **M\*** *A B &KEY TRANSPOSE-A? TRANSPOSE-B?*

    Compute op(`A`) \* op(`B`). Where op is either the identity or the
    transpose operation depending on `TRANSPOSE-A?` and `TRANSPOSE-B?`.

<a name='x-28MGL-MAT-3AMM-2A-20FUNCTION-29'></a>

- [function] **MM\*** *M &REST ARGS*

    Convenience function to multiply several matrices. 
    
    (mm\* a b c) => a \* b \* c

<a name='x-28MGL-MAT-3AM--20FUNCTION-29'></a>

- [function] **M-** *A B*

    Return `A` - `B`.

<a name='x-28MGL-MAT-3AM-2B-20FUNCTION-29'></a>

- [function] **M+** *A B*

    Return `A` + `B`.

<a name='x-28MGL-MAT-3AINVERT-20FUNCTION-29'></a>

- [function] **INVERT** *A*

    Return the inverse of `A`.

<a name='x-28MGL-MAT-3ALOGDET-20FUNCTION-29'></a>

- [function] **LOGDET** *MAT*

    Logarithm of the determinant of a matrix. Return -1, 1 or 0 (or
    equivalent) to correct for the sign, as a second value.

<a name='x-28MGL-MAT-3A-40MAT-MAPPINGS-20MGL-PAX-3ASECTION-29'></a>

## 14 Mappings

<a name='x-28MGL-MAT-3AMAP-CONCAT-20FUNCTION-29'></a>

- [function] **MAP-CONCAT** *FN MATS MAT &KEY KEY PASS-RAW-P*

    Call `FN` with each element of `MATS` and `MAT` temporarily reshaped to
    the dimensions of the current element of `MATS` and return `MAT`. For
    the next element the displacement is increased so that there is no
    overlap.
    
    `MATS` is keyed by `KEY` just like the CL sequence functions. Normally,
    `FN` is called with the matrix returned by `KEY`. However, if
    `PASS-RAW-P`, then the matrix returned by `KEY` is only used to
    calculate dimensions and the element of `MATS` that was passed to `KEY`
    is passed to `FN`, too.
    
    ```
    (map-concat #'copy! (list (make-mat 2) (make-mat 4 :initial-element 1))
                (make-mat '(2 3)))
    ==> #<MAT 2x3 AB #2A((0.0d0 0.0d0 1.0d0) (1.0d0 1.0d0 1.0d0))>
    ```


<a name='x-28MGL-MAT-3AMAP-DISPLACEMENTS-20FUNCTION-29'></a>

- [function] **MAP-DISPLACEMENTS** *FN MAT DIMENSIONS &KEY (DISPLACEMENT-START 0) DISPLACEMENT-STEP*

    Call `FN` with `MAT` reshaped to `DIMENSIONS`, first displaced by
    `DISPLACEMENT-START` that's incremented by `DISPLACEMENT-STEP` each
    iteration while there are enough elements left for `DIMENSIONS` at the
    current displacement. Returns `MAT`.
    
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
    ```


<a name='x-28MGL-MAT-3AMAP-MATS-INTO-20FUNCTION-29'></a>

- [function] **MAP-MATS-INTO** *RESULT-MAT FN &REST MATS*

    Like `CL:MAP-INTO` but for [`MAT`][773f] objects. Destructively modifies
    `RESULT-MAT` to contain the results of applying `FN` to each element in
    the argument `MATS` in turn.

<a name='x-28MGL-MAT-3A-40MAT-RANDOM-20MGL-PAX-3ASECTION-29'></a>

## 15 Random numbers

This is rather experimental.

<a name='x-28MGL-MAT-3AMV-GAUSSIAN-RANDOM-20FUNCTION-29'></a>

- [function] **MV-GAUSSIAN-RANDOM** *&KEY MEANS COVARIANCES*

    Return a column vector of samples from the multivariate normal
    distribution defined by `MEANS` (Nx1) and `COVARIANCES` (NxN).

<a name='x-28MGL-MAT-3ACOPY-RANDOM-STATE-20GENERIC-FUNCTION-29'></a>

- [generic-function] **COPY-RANDOM-STATE** *STATE*

<a name='x-28MGL-MAT-3AUNIFORM-RANDOM-21-20FUNCTION-29'></a>

- [function] **UNIFORM-RANDOM!** *MAT &KEY (LIMIT 1)*

    Fill `MAT` with random numbers sampled uniformly from the [0,LIMIT)
    interval of `MAT`'s type.

<a name='x-28MGL-MAT-3AGAUSSIAN-RANDOM-21-20FUNCTION-29'></a>

- [function] **GAUSSIAN-RANDOM!** *MAT &KEY (MEAN 0) (STDDEV 1)*

    Fill `MAT` with independent normally distributed random numbers with
    `MEAN` and `STDDEV`.

<a name='x-28MGL-MAT-3AORTHOGONAL-RANDOM-21-20FUNCTION-29'></a>

- [function] **ORTHOGONAL-RANDOM!** *M &KEY (SCALE 1)*

    Fill the matrix `M` with random values in such a way that `M^T * M`
    is the identity matrix (or something close if `M` is wide). Return `M`.

<a name='x-28MGL-MAT-3A-40MAT-IO-20MGL-PAX-3ASECTION-29'></a>

## 16 I/O

<a name='x-28MGL-MAT-3AWRITE-MAT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **WRITE-MAT** *MAT STREAM*

    Write `MAT` to `STREAM` in portable binary format.
    Displacement and size are taken into account, only visible elements
    are written.

<a name='x-28MGL-MAT-3AREAD-MAT-20GENERIC-FUNCTION-29'></a>

- [generic-function] **READ-MAT** *MAT STREAM*

    Destructively modify the visible portion (with
    regards to displacement and shape) of `MAT` by reading [`MAT-SIZE`][1caf] number
    of elements from `STREAM`. No sanity checks are performed, [`READ-MAT`][dc10]
    may return without error even if `STREAM` contains garbage.

<a name='x-28MGL-MAT-3A-40MAT-EXTENSION-API-20MGL-PAX-3ASECTION-29'></a>

## 17 Extension API

Macros for defining cuda and lisp kernels. Typically operations
have a cuda and a lisp implementations and decide which to use with
[`USE-CUDA-P`][51e4]. These are provided to help writing new operations.

<a name='x-28MGL-MAT-3ADEFINE-LISP-KERNEL-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFINE-LISP-KERNEL** *(NAME &KEY (CTYPES '(:FLOAT :DOUBLE))) (&REST PARAMS) &BODY BODY*

    This is an extended `CL-CUDA:DEFKERNEL` macro. It knows how to deal
    with [`MAT`][773f] objects and can define the same function for multiple
    `CTYPES`. Example:
    
        (define-lisp-kernel (lisp-.+!)
            ((alpha single-float) (x :mat :input) (start-x index) (n index))
          (loop for xi of-type index upfrom start-x
                  below (the! index (+ start-x n))
                do (incf (aref x xi) alpha)))
    
    Parameters are either of the form `(<NAME> <LISP-TYPE)`
    or `(<NAME> :MAT <DIRECTION>)`. In the latter case, the appropriate
    CFFI:POINTER is passed to the kernel. `<DIRECTION>` is passed on to
    the [`WITH-FACET`][f66e] that's used to acquire the foreign array. Note that
    the return type is not declared.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `CTYPES` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this only means that one needs to write only
    one kernel for `SINGLE-FLOAT` and `DOUBLE-FLOAT`. All such functions get
    the declaration from [`*DEFAULT-LISP-KERNEL-DECLARATIONS*`][fd9c].
    
    Finally, a dispatcher function with `NAME` is defined which determines
    the ctype of the [`MAT`][773f] objects passed for `:MAT` typed parameters. It's
    an error if they are not of the same type. Scalars declared
    `SINGLE-FLOAT` are coerced to that type and the appropriate kernel is
    called.

<a name='x-28MGL-MAT-3A-2ADEFAULT-LISP-KERNEL-DECLARATIONS-2A-20VARIABLE-29'></a>

- [variable] **\*DEFAULT-LISP-KERNEL-DECLARATIONS\*** *((OPTIMIZE SPEED (SB-C::INSERT-ARRAY-BOUNDS-CHECKS 0)))*

    These declarations are added automatically to kernel functions.

<a name='x-28MGL-MAT-3ADEFINE-CUDA-KERNEL-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFINE-CUDA-KERNEL** *(NAME &KEY (CTYPES '(:FLOAT :DOUBLE))) (RETURN-TYPE PARAMS) &BODY BODY*

    This is an extended `CL-CUDA:DEFKERNEL` macro. It knows how to deal
    with [`MAT`][773f] objects and can define the same function for multiple
    `CTYPES`. Example:
    
        (define-cuda-kernel (cuda-.+!)
            (void ((alpha float) (x :mat :input) (n int)))
          (let ((stride (* block-dim-x grid-dim-x)))
            (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
                    (+ i stride)))
                ((>= i n))
              (set (aref x i) (+ (aref x i) alpha)))))
    
    The signature looks pretty much like in `CL-CUDA:DEFKERNEL`, but
    parameters can take the form of `(<NAME> :MAT <DIRECTION>)` too, in
    which case the appropriate `CL-CUDA.DRIVER-API:CU-DEVICE-PTR` is
    passed to the kernel. `<DIRECTION>` is passed on to the [`WITH-FACET`][f66e]
    that's used to acquire the cuda array.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `CTYPES` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this only means that one needs to write only
    one kernel for `FLOAT` and `DOUBLE`.
    
    Finally, a dispatcher function with `NAME` is defined which determines
    the ctype of the [`MAT`][773f] objects passed for `:MAT` typed parameters. It's
    an error if they are not of the same type. Scalars declared `FLOAT`
    are coerced to that type and the appropriate kernel is called.

<a name='x-28MGL-MAT-3A-40MAT-DEBUGGING-20MGL-PAX-3ASECTION-29'></a>

## 18 Debugging

The largest class of bugs has to do with synchronization of facets
being broken. This is almost always caused by an operation that
mispecifies the [`DIRECTION`][298b] argument of [`WITH-FACET`][f66e]. For example, the
matrix argument of [`SCAL!`][4c84] should be accessed with direciton `:IO`. But
if it's `:INPUT` instead, then subsequent access to the [`ARRAY`][ba88] facet
will not see the changes made by [`AXPY!`][9221], and if it's `:OUTPUT`, then
any changes made to the [`ARRAY`][ba88] facet since the last update of the
[`CUDA-ARRAY`][84c3] facet will not be copied and from the wrong input [`SCAL!`][4c84]
will compute the wrong result.

Another thing that tends to come up is figuring out where memory is
used.

<a name='x-28MGL-MAT-3AWITH-MAT-COUNTERS-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-MAT-COUNTERS** *(&KEY COUNT N-BYTES) &BODY BODY*

    Count all [`MAT`][773f] allocations and also the number of bytes they may
    require. *May require* here really means an upper bound,
    because `(MAKE-MAT (EXPT 2 60))` doesn't actually uses memory until
    one of its facets is accessed (don't simply evaluate it though,
    printing the result will access the [`ARRAY`][ba88] facet if [`*PRINT-MAT*`][81d0]).
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
    
    ```

<a name='x-28MGL-CUBE-3A-40CUBE-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Cube manual

## Table of Contents

- [1 Introduction][0752]
- [2 Basics][1164]
- [3 Synchronization][688d]
- [4 Facet extension API][eb06]
- [5 The default implementation of CALL-WITH-FACET*][b64e]
- [6 Views][692d]
- [7 Destroying cubes][2fa1]
- [8 Facet barriers][8520]

###### \[in package MGL-CUBE\]
<a name='x-28MGL-CUBE-3A-40CUBE-INTRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 1 Introduction

This is the libray on which MGL-MAT (see [MAT manual][2629]) is
built. The idea of automatically translating between various
representations may be useful for other applications, so this got
its own package and all ties to MGL-MAT has been severed.

This package defines [`CUBE`][9fcc], an abstract base class that provides a
framework for automatic conversion between various representations
of the same data. To define a cube, [`CUBE`][9fcc] needs to be subclassed and
the [Facet extension API][eb06] be implemented.

If you are only interested in how to use cubes in general, read
[Basics][1164], [Destroying cubes][2fa1] and [Facet barriers][8520].

If you want to implement a new cube datatype, then see
[Facet extension API][eb06], [The default implementation of CALL-WITH-FACET\*][b64e] and [Views][692d].

<a name='x-28MGL-CUBE-3A-40CUBE-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 2 Basics

Here we learn what a [`CUBE`][9fcc] is and how to access the data in it with
[`WITH-FACET`][f66e].

<a name='x-28MGL-CUBE-3ACUBE-20CLASS-29'></a>

- [class] **CUBE**

    A datacube that has various representations of the
    same stuff. These representations go by the name \`facet'. Clients
    must use [`WITH-FACET`][f66e] to acquire a dynamic extent reference to a
    facet. With the information provided in the [`DIRECTION`][298b] argument of
    [`WITH-FACET`][f66e], the cube keeps track of which facets are up-to-date and
    copies data between them as necessary.
    
    The cube is an abstract class, it does not provide useful behavior
    in itself. One must subclass it and implement the
    [Facet extension API][eb06].
    
    Also see [Views][692d], [Destroying cubes][2fa1] and [Facet barriers][8520].

<a name='x-28MGL-CUBE-3AWITH-FACET-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-FACET** *(FACET (CUBE FACET-NAME &KEY (DIRECTION :IO) TYPE)) &BODY BODY*

    Bind the variable `FACET` to the facet with `FACET-NAME` of `CUBE`. `FACET`
    is to be treated as dynamic extent: it is not allowed to keep a
    reference to it. For the description of the `DIRECTION` parameter, see
    the type `DIRECTION`.

<a name='x-28MGL-CUBE-3ADIRECTION-20TYPE-29'></a>

- [type] **DIRECTION**

    Used by [`WITH-FACET`][f66e], `DIRECTION` can be `:INPUT`, `:OUTPUT` or `:IO`.
    
    - `:INPUT` promises that the facet will only be read and never
      written. Other up-to-date facets of the same cube remain
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      [`SELECT-COPY-SOURCE-FOR-FACET*`][6056]).
    
    - `:OUTPUT` promises that *all* data will be overwritten without
      reading any data. All up-to-date facets become non-up-to-date,
      while this facet is marked as up-to-date. No copying of data takes
      place.
    
    - `:IO` promises nothing about the type of access. All up-to-date
      facets become non-up-to-date, while this facet is marked as
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      [`SELECT-COPY-SOURCE-FOR-FACET*`][6056]).
    
    Any number of [`WITH-FACET`][f66e]s with direction `:INPUT` may be active at
    the same time, but `:IO` and `:OUTPUT` cannot coexists with any other
    [`WITH-FACET`][f66e] regardless of the direction. An exception is made for
    nested [`WITH-FACET`][f66e]s for the same facet: an enclosing [`WITH-FACET`][f66e]
    never conflicts with an inner [`WITH-FACET`][f66e], but [`WITH-FACET`][f66e]s for
    another facet or for the same facet but from another thread do.
    
    See [`CHECK-NO-WRITERS`][edce] and [`CHECK-NO-WATCHERS`][b9c1] called by
    [The default implementation of CALL-WITH-FACET\*][b64e].

<a name='x-28MGL-CUBE-3AWITH-FACETS-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-FACETS** *(&REST FACET-BINDING-SPECS) &BODY BODY*

    A shorthand for writing nested [`WITH-FACET`][f66e] calls.
    
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
    ```


<a name='x-28MGL-CUBE-3A-40CUBE-SYNCHRONIZATION-20MGL-PAX-3ASECTION-29'></a>

## 3 Synchronization

Cubes keep track of which facets are used, which are up-to-date to
be able to perform automatic translation between facets. [`WITH-FACET`][f66e]
and other operations access and make changes to this metadata so
thread safety is a concern. In this section, we detail how to relax
the default thread safety guarantees.

A related concern is async signal safety which arises most often
when C-c'ing or killing a thread or when the extremely nasty
WITH-TIMEOUT macro is used. In a nutshell, changes to cube metadata
are always made with interrupts disabled so things should be async
signal safe.

<a name='x-28MGL-CUBE-3ASYNCHRONIZATION-20-28MGL-PAX-3AACCESSOR-20MGL-CUBE-3ACUBE-29-29'></a>

- [accessor] **SYNCHRONIZATION** *CUBE* *(:SYNCHRONIZATION = \*DEFAULT-SYNCHRONIZATION\*)*

    By default setup and teardown of facets by
    [`WITH-FACET`][f66e] is performed in a thread safe way. Corrupting internal
    data structures of cubes is not fun, but in the name of
    performance, synchronization can be turned off either dynamically
    or on a per instance basis.
    
    If `T`, then access to cube metadata is always synchronized. If `NIL`,
    then never. If `:MAYBE`, then whether access is synchronized is
    determined by [`*MAYBE-SYNCHRONIZE-CUBE*`][478c] that's true by default.
    
    The default is the value of [`*DEFAULT-SYNCHRONIZATION*`][19b9]
    that's `:MAYBE` by default.
    
    Note that the body of a [`WITH-FACET`][f66e] is never synchronized with
    anyone, apart from the implicit reader/writer conflict (see
    [`DIRECTION`][298b]).

<a name='x-28MGL-CUBE-3A-2ADEFAULT-SYNCHRONIZATION-2A-20VARIABLE-29'></a>

- [variable] **\*DEFAULT-SYNCHRONIZATION\*** *:MAYBE*

    The default value for [`SYNCHRONIZATION`][923f] of new cubes.

<a name='x-28MGL-CUBE-3A-2AMAYBE-SYNCHRONIZE-CUBE-2A-20VARIABLE-29'></a>

- [variable] **\*MAYBE-SYNCHRONIZE-CUBE\*** *T*

    Determines whether access the cube metadata is synchronized for
    cubes with [`SYNCHRONIZATION`][923f] `:MAYBE`.

<a name='x-28MGL-CUBE-3A-40FACET-EXTENSION-API-20MGL-PAX-3ASECTION-29'></a>

## 4 Facet extension API

<a name='x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ALOCATIVE-29'></a>

- [locative] **FACET-NAME**

    The `FACET-NAME` locative is to refer to stuff defined with
    [`DEFINE-FACET-NAME`][5192].

<a name='x-28MGL-CUBE-3ADEFINE-FACET-NAME-20MGL-PAX-3AMACRO-29'></a>

- [macro] **DEFINE-FACET-NAME** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    Just a macro to document the symbol [`FACET-NAME`][21a4] means a facet
    name (as in the [`FACET-NAME`][21a4]).

<a name='x-28MGL-CUBE-3AMAKE-FACET-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **MAKE-FACET\*** *CUBE FACET-NAME*

    As the first value, return a new object capable of
    storing `CUBE`'s data in the facet with `FACET-NAME`. As the second
    value, return a facet description which is going to be passed to
    [`DESTROY-FACET*`][16c3]. As the third value, return a generalized boolean
    indicating whether this facet must be explicitly destroyed (in which
    case a finalizer will be added to `CUBE`). Called by [`WITH-FACET`][f66e] (or
    more directly [`WATCH-FACET`][a238]) when there is no facet with
    `FACET-NAME`.

<a name='x-28MGL-CUBE-3ADESTROY-FACET-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **DESTROY-FACET\*** *FACET-NAME FACET FACET-DESCRIPTION*

    Destroy `FACET` that belongs to a facet with
    `FACET-NAME` and `FACET-DESCRIPTION`. The cube this facet belongs to is
    not among the parameters because this method can be called from a
    finalizer on the cube (so we can't have a reference to the cube
    portably) which also means that it may run in an unpredictable
    thread.

<a name='x-28MGL-CUBE-3ACOPY-FACET-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **COPY-FACET\*** *CUBE FROM-FACET-NAME FROM-FACET TO-FACET-NAME TO-FACET*

    Copy the `CUBE`'s data from `FROM-FACET` with
    `FROM-FACET-NAME` to `TO-FACET` with `TO-FACET-NAME`. Called by
    [`WITH-FACET`][f66e] (or more directly [`WATCH-FACET`][a238]) when necessary. `FROM-FACET`
    is what [`SELECT-COPY-SOURCE-FOR-FACET*`][6056] returned.

<a name='x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **CALL-WITH-FACET\*** *CUBE FACET-NAME DIRECTION FN*

    Ensure that the facet with `FACET-NAME` exists.
    Depending on `DIRECTION` and up-to-dateness, maybe copy data. Finally,
    call `FN` with the facet. The default implementation acquires the
    facet with [`WATCH-FACET`][a238], calls `FN` with it and finally calls
    [`UNWATCH-FACET`][ee90]. However, specializations are allowed to create only
    temporary, dynamic extent views without ever calling [`WATCH-FACET`][a238] and
    [`UNWATCH-FACET`][ee90].

<a name='x-28MGL-CUBE-3ASET-UP-TO-DATE-P-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SET-UP-TO-DATE-P\*** *CUBE FACET-NAME VIEW VALUE*

    Set the [`VIEW-UP-TO-DATE-P`][9e0b] slot of `VIEW` to `VALUE`.
    The default implementation simply SETFs it. This being a generic
    function allows subclasses to ensure that certain facets which share
    storage are always up-to-date at the same time.

<a name='x-28MGL-CUBE-3ASELECT-COPY-SOURCE-FOR-FACET-2A-20GENERIC-FUNCTION-29'></a>

- [generic-function] **SELECT-COPY-SOURCE-FOR-FACET\*** *CUBE TO-NAME TO-FACET*

    Return a facet name selected from the up-to-date
    views from which `CUBE`'s data should be copied to `TO-FACET`. The
    default method simply returns the first up-to-date view.

Also see [The default implementation of CALL-WITH-FACET\*][b64e].

<a name='x-28MGL-CUBE-3A-40DEFAULT-CALL-WITH-FACET-2A-20MGL-PAX-3ASECTION-29'></a>

## 5 The default implementation of CALL-WITH-FACET*

<a name='x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20-28METHOD-20NIL-20-28MGL-CUBE-3ACUBE-20T-20T-20T-29-29-29'></a>

- [method] **CALL-WITH-FACET\*** *(CUBE CUBE) FACET-NAME DIRECTION FN*

    The default implementation of [`CALL-WITH-FACET*`][46c5] is defined in terms
    of the [`WATCH-FACET`][a238] and the [`UNWATCH-FACET`][ee90] generic functions. These
    can be considered part of the [Facet extension API][eb06].

<a name='x-28MGL-CUBE-3AWATCH-FACET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **WATCH-FACET** *CUBE FACET-NAME DIRECTION*

    This is what the default [`CALL-WITH-FACET*`][46c5] method,
    in terms of which [`WITH-FACET`][f66e] is implemented, calls first. The
    default method takes care of creating views, copying and tracking
    up-to-dateness.
    
    Calls [`CHECK-NO-WRITERS`][edce] (unless [`*LET-INPUT-THROUGH-P*`][03fc]) and
    [`CHECK-NO-WATCHERS`][b9c1] (unless [`*LET-OUTPUT-THROUGH-P*`][8719]) depending on
    `DIRECTION` to detect situations with a writer being concurrent to
    readers/writers because that would screw up the tracking
    up-to-dateness.
    
    The default implementation should suffice most of the time. MGL-MAT
    specializes it to override the `DIRECTION` arg if it's `:OUTPUT` but not
    all elements are visible due to reshaping.

<a name='x-28MGL-CUBE-3AUNWATCH-FACET-20GENERIC-FUNCTION-29'></a>

- [generic-function] **UNWATCH-FACET** *CUBE FACET-NAME*

    This is what the default [`CALL-WITH-FACET*`][46c5] method,
    in terms of which [`WITH-FACET`][f66e] is implemented, calls last. The default
    method takes care of taking down views. External resource managers
    may want to hook into this to handle unused facets.

<a name='x-28MGL-CUBE-3A-2ALET-INPUT-THROUGH-P-2A-20VARIABLE-29'></a>

- [variable] **\*LET-INPUT-THROUGH-P\*** *NIL*

    If true, [`WITH-FACETS`][b61b] (more precisely, the default implementation of
    [`CALL-WITH-FACET*`][46c5]) with `:DIRECTION` `:INPUT` does not call
    [`CHECK-NO-WRITERS`][edce]. This knob is intended to be bound locally for
    debugging purposes.

<a name='x-28MGL-CUBE-3A-2ALET-OUTPUT-THROUGH-P-2A-20VARIABLE-29'></a>

- [variable] **\*LET-OUTPUT-THROUGH-P\*** *NIL*

    If true, [`WITH-FACETS`][b61b] (more precisely, the default implementation of
    [`CALL-WITH-FACET*`][46c5]) with `:DIRECTION` `:IO` or `:OUTPUT` does not call
    [`CHECK-NO-WATCHERS`][b9c1]. This knob is intended to be bound locally for
    debugging purposes.

<a name='x-28MGL-CUBE-3ACHECK-NO-WRITERS-20FUNCTION-29'></a>

- [function] **CHECK-NO-WRITERS** *CUBE FACET-NAME MESSAGE-FORMAT &REST MESSAGE-ARGS*

    Signal an error if `CUBE` has facets (with names other than
    `FACET-NAME`) being written (i.e. direction is `:IO` or `:OUTPUT`).

<a name='x-28MGL-CUBE-3ACHECK-NO-WATCHERS-20FUNCTION-29'></a>

- [function] **CHECK-NO-WATCHERS** *CUBE FACET-NAME MESSAGE-FORMAT &REST MESSAGE-ARGS*

    Signal an error if `CUBE` has facets (with names other than
    `FACET-NAME`) being regardless of the direction.

<a name='x-28MGL-CUBE-3A-40CUBE-VIEWS-20MGL-PAX-3ASECTION-29'></a>

## 6 Views

We learn what a [`VIEW`][776e] is, how it's related to facets. See [`VIEWS`][c62e],
[`FIND-VIEW`][d95a], and the default method of [`SET-UP-TO-DATE-P*`][59d0]. Views are
only visible to those implementing the [Facet extension API][eb06].

<a name='x-28MGL-CUBE-3AVIEW-20CLASS-29'></a>

- [class] **VIEW** *STRUCTURE-OBJECT*

    A cube has facets, as we discussed in [Basics][1164]. The object
    which holds the data in a particular representation is the facet. A
    `VIEW` holds one such facet and some metadata pertaining to it: its
    name ([`VIEW-FACET-NAME`][9ecd]), whether it's up-to-date ([`VIEW-UP-TO-DATE-P`][9e0b]),
    etc. `VIEW` ojbects are never seen when simply using a cube, they are
    for implementing the [Facet extension API][eb06].

<a name='x-28MGL-CUBE-3AVIEW-FACET-NAME-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29'></a>

- [structure-accessor] **VIEW-FACET-NAME**

<a name='x-28MGL-CUBE-3AVIEW-FACET-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29'></a>

- [structure-accessor] **VIEW-FACET**

<a name='x-28MGL-CUBE-3AVIEW-FACET-DESCRIPTION-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29'></a>

- [structure-accessor] **VIEW-FACET-DESCRIPTION**

<a name='x-28MGL-CUBE-3AVIEW-UP-TO-DATE-P-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29'></a>

- [structure-accessor] **VIEW-UP-TO-DATE-P**

<a name='x-28MGL-CUBE-3AVIEWS-20FUNCTION-29'></a>

- [function] **VIEWS** *CUBE*

    Return the views of `CUBE`.

<a name='x-28MGL-CUBE-3AFIND-VIEW-20FUNCTION-29'></a>

- [function] **FIND-VIEW** *CUBE FACET-NAME*

    Return the view of `CUBE` for the facet with `FACET-NAME` or `NIL` if no
    such view exists.

<a name='x-28MGL-CUBE-3A-40DESTRUCTION-OF-CUBES-20MGL-PAX-3ASECTION-29'></a>

## 7 Destroying cubes

Lifetime management of facets is manual (but facets of garbage
cubes are freed automatically by a finalizer, see [`MAKE-FACET*`][2a64]). One
may destroy a single facet or all facets of a cube with
[`DESTROY-FACET`][bdd6] and [`DESTROY-CUBE`][2cb4], respectively. Also see
[Facet barriers][8520].

<a name='x-28MGL-CUBE-3ADESTROY-FACET-20FUNCTION-29'></a>

- [function] **DESTROY-FACET** *CUBE FACET-NAME*

    Free resources associated with the facet with `FACET-NAME` and remove
    it from [`VIEWS`][c62e] of `CUBE`.

<a name='x-28MGL-CUBE-3ADESTROY-CUBE-20FUNCTION-29'></a>

- [function] **DESTROY-CUBE** *CUBE*

    Destroy all facets of `CUBE` with [`DESTROY-FACET`][bdd6].

<a name='x-28MGL-CUBE-3A-40FACET-BARRIER-20MGL-PAX-3ASECTION-29'></a>

## 8 Facet barriers

A facility to control lifetime of facets tied to a dynamic extent.
Also see [Destroying cubes][2fa1].

<a name='x-28MGL-CUBE-3AWITH-FACET-BARRIER-20MGL-PAX-3AMACRO-29'></a>

- [macro] **WITH-FACET-BARRIER** *(CUBE-TYPE ENSURES DESTROYS) &BODY BODY*

    When `BODY` exits, destroy facets which:
    
    - are of cubes with `CUBE-TYPE`
    
    - have a facet name among `DESTROYS`
    
    - were created in the dynamic extent of `BODY`
    
    Before destroying the facets, it is ensured that facets with names
    among `ENSURES` are up-to-date. WITH-FACET-BARRIERs can be nested, in
    case of multiple barriers matching the cube's type and the created
    facet's name, the innermost one takes precedence.
    
    The purpose of this macro is twofold. First, it makes it easy to
    temporarily work with a certain facet of many cubes without leaving
    newly created facets around. Second, it can be used to make sure
    that facets whose extent is tied to some dynamic boundary (such as
    the thread in which they were created) are destroyed.

<a name='x-28MGL-CUBE-3ACOUNT-BARRED-FACETS-20FUNCTION-29'></a>

- [function] **COUNT-BARRED-FACETS** *FACET-NAME &KEY (TYPE 'CUBE)*

    Count facets with `FACET-NAME` of cubes of `TYPE` which will be
    destroyed by a facet barrier.

  [00a6]: #x-28MGL-MAT-3A-40MAT-CTYPES-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-CTYPES MGL-PAX:SECTION)"
  [01b0]: #x-28MGL-MAT-3A-40MAT-WHAT-IS-IT-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-WHAT-IS-IT MGL-PAX:SECTION)"
  [02a7]: #x-28MGL-MAT-3A-2ADEFAULT-MAT-CTYPE-2A-20VARIABLE-29 "(MGL-MAT:*DEFAULT-MAT-CTYPE* VARIABLE)"
  [0386]: #x-28MGL-MAT-3A-40MAT-BLAS-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-BLAS MGL-PAX:SECTION)"
  [03fc]: #x-28MGL-CUBE-3A-2ALET-INPUT-THROUGH-P-2A-20VARIABLE-29 "(MGL-CUBE:*LET-INPUT-THROUGH-P* VARIABLE)"
  [0521]: #x-28MGL-MAT-3AMAT-DISPLACEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "(MGL-MAT:MAT-DISPLACEMENT (MGL-PAX:READER MGL-MAT:MAT))"
  [0752]: #x-28MGL-CUBE-3A-40CUBE-INTRODUCTION-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@CUBE-INTRODUCTION MGL-PAX:SECTION)"
  [0ee2]: #x-28MGL-MAT-3AROW-MAJOR-MREF-20FUNCTION-29 "(MGL-MAT:ROW-MAJOR-MREF FUNCTION)"
  [0f32]: #x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-21-20FUNCTION-29 "(MGL-MAT:RESHAPE-AND-DISPLACE! FUNCTION)"
  [1164]: #x-28MGL-CUBE-3A-40CUBE-BASICS-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@CUBE-BASICS MGL-PAX:SECTION)"
  [1227]: #x-28MGL-MAT-3APINNING-SUPPORTED-P-20FUNCTION-29 "(MGL-MAT:PINNING-SUPPORTED-P FUNCTION)"
  [12c9]: #x-28MGL-MAT-3AFOREIGN-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "(MGL-MAT:FOREIGN-ARRAY MGL-CUBE:FACET-NAME)"
  [16c3]: #x-28MGL-CUBE-3ADESTROY-FACET-2A-20GENERIC-FUNCTION-29 "(MGL-CUBE:DESTROY-FACET* GENERIC-FUNCTION)"
  [19b9]: #x-28MGL-CUBE-3A-2ADEFAULT-SYNCHRONIZATION-2A-20VARIABLE-29 "(MGL-CUBE:*DEFAULT-SYNCHRONIZATION* VARIABLE)"
  [1caf]: #x-28MGL-MAT-3AMAT-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "(MGL-MAT:MAT-SIZE (MGL-PAX:READER MGL-MAT:MAT))"
  [21a4]: #x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ALOCATIVE-29 "(MGL-CUBE:FACET-NAME MGL-PAX:LOCATIVE)"
  [2629]: #x-28MGL-MAT-3A-40MAT-MANUAL-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-MANUAL MGL-PAX:SECTION)"
  [28eb]: #x-28MGL-MAT-3AMREF-20FUNCTION-29 "(MGL-MAT:MREF FUNCTION)"
  [298b]: #x-28MGL-CUBE-3ADIRECTION-20TYPE-29 "(MGL-CUBE:DIRECTION TYPE)"
  [2a64]: #x-28MGL-CUBE-3AMAKE-FACET-2A-20GENERIC-FUNCTION-29 "(MGL-CUBE:MAKE-FACET* GENERIC-FUNCTION)"
  [2cb4]: #x-28MGL-CUBE-3ADESTROY-CUBE-20FUNCTION-29 "(MGL-CUBE:DESTROY-CUBE FUNCTION)"
  [2fa1]: #x-28MGL-CUBE-3A-40DESTRUCTION-OF-CUBES-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@DESTRUCTION-OF-CUBES MGL-PAX:SECTION)"
  [36b8]: #x-28MGL-MAT-3AMAX-POOL-21-20FUNCTION-29 "(MGL-MAT:MAX-POOL! FUNCTION)"
  [373b]: #x-28MGL-MAT-3A-2AFOREIGN-ARRAY-STRATEGY-2A-20-28VARIABLE-20-22-see-20below--22-29-29 "(MGL-MAT:*FOREIGN-ARRAY-STRATEGY* (VARIABLE \"-see below-\"))"
  [3cf7]: #x-28MGL-MAT-3AMAT-MAX-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "(MGL-MAT:MAT-MAX-SIZE (MGL-PAX:READER MGL-MAT:MAT))"
  [4586]: #x-28MGL-MAT-3A-2ASUPPORTED-CTYPES-2A-20VARIABLE-29 "(MGL-MAT:*SUPPORTED-CTYPES* VARIABLE)"
  [46c5]: #x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20GENERIC-FUNCTION-29 "(MGL-CUBE:CALL-WITH-FACET* GENERIC-FUNCTION)"
  [478c]: #x-28MGL-CUBE-3A-2AMAYBE-SYNCHRONIZE-CUBE-2A-20VARIABLE-29 "(MGL-CUBE:*MAYBE-SYNCHRONIZE-CUBE* VARIABLE)"
  [4802]: #x-28MGL-MAT-3ADISPLACE-21-20FUNCTION-29 "(MGL-MAT:DISPLACE! FUNCTION)"
  [4c84]: #x-28MGL-MAT-3ASCAL-21-20FUNCTION-29 "(MGL-MAT:SCAL! FUNCTION)"
  [4cc3]: #x-28MGL-MAT-3AMAKE-MAT-20FUNCTION-29 "(MGL-MAT:MAKE-MAT FUNCTION)"
  [4d1e]: #x-28MGL-MAT-3A-40MAT-FOREIGN-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-FOREIGN MGL-PAX:SECTION)"
  [5192]: #x-28MGL-CUBE-3ADEFINE-FACET-NAME-20MGL-PAX-3AMACRO-29 "(MGL-CUBE:DEFINE-FACET-NAME MGL-PAX:MACRO)"
  [51e4]: #x-28MGL-MAT-3AUSE-CUDA-P-20FUNCTION-29 "(MGL-MAT:USE-CUDA-P FUNCTION)"
  [52cb]: #x-28MGL-MAT-3AADJUST-21-20FUNCTION-29 "(MGL-MAT:ADJUST! FUNCTION)"
  [552a]: #x-28MGL-MAT-3ASTACK-21-20FUNCTION-29 "(MGL-MAT:STACK! FUNCTION)"
  [58bb]: #x-28MGL-MAT-3ARESHAPE-TO-ROW-MATRIX-21-20FUNCTION-29 "(MGL-MAT:RESHAPE-TO-ROW-MATRIX! FUNCTION)"
  [58e7]: #x-28MGL-MAT-3ARESHAPE-21-20FUNCTION-29 "(MGL-MAT:RESHAPE! FUNCTION)"
  [59d0]: #x-28MGL-CUBE-3ASET-UP-TO-DATE-P-2A-20GENERIC-FUNCTION-29 "(MGL-CUBE:SET-UP-TO-DATE-P* GENERIC-FUNCTION)"
  [5d9b]: #x-28MGL-MAT-3A-2AN-MEMCPY-DEVICE-TO-HOST-2A-20VARIABLE-29 "(MGL-MAT:*N-MEMCPY-DEVICE-TO-HOST* VARIABLE)"
  [5feb]: #x-28MGL-MAT-3A-2ACUDA-ENABLED-2A-20VARIABLE-29 "(MGL-MAT:*CUDA-ENABLED* VARIABLE)"
  [6056]: #x-28MGL-CUBE-3ASELECT-COPY-SOURCE-FOR-FACET-2A-20GENERIC-FUNCTION-29 "(MGL-CUBE:SELECT-COPY-SOURCE-FOR-FACET* GENERIC-FUNCTION)"
  [688d]: #x-28MGL-CUBE-3A-40CUBE-SYNCHRONIZATION-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@CUBE-SYNCHRONIZATION MGL-PAX:SECTION)"
  [692d]: #x-28MGL-CUBE-3A-40CUBE-VIEWS-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@CUBE-VIEWS MGL-PAX:SECTION)"
  [6ccf]: #x-28MGL-MAT-3A-40MAT-PRINTING-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-PRINTING MGL-PAX:SECTION)"
  [7043]: #x-28MGL-MAT-3AFOREIGN-ARRAY-20CLASS-29 "(MGL-MAT:FOREIGN-ARRAY CLASS)"
  [7388]: #x-28MGL-MAT-3A-40MAT-MAPPINGS-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-MAPPINGS MGL-PAX:SECTION)"
  [773f]: #x-28MGL-MAT-3AMAT-20CLASS-29 "(MGL-MAT:MAT CLASS)"
  [776e]: #x-28MGL-CUBE-3AVIEW-20CLASS-29 "(MGL-CUBE:VIEW CLASS)"
  [78d7]: #x-28MGL-MAT-3A-40MAT-IO-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-IO MGL-PAX:SECTION)"
  [7de8]: #x-28MGL-CUBE-3A-40CUBE-MANUAL-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@CUBE-MANUAL MGL-PAX:SECTION)"
  [81d0]: #x-28MGL-MAT-3A-2APRINT-MAT-2A-20VARIABLE-29 "(MGL-MAT:*PRINT-MAT* VARIABLE)"
  [84c3]: #x-28MGL-MAT-3ACUDA-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "(MGL-MAT:CUDA-ARRAY MGL-CUBE:FACET-NAME)"
  [8520]: #x-28MGL-CUBE-3A-40FACET-BARRIER-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@FACET-BARRIER MGL-PAX:SECTION)"
  [85d5]: #x-28-22mgl-mat-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"mgl-mat\" ASDF/SYSTEM:SYSTEM)"
  [867d]: #x-28MGL-MAT-3ACTYPE-20TYPE-29 "(MGL-MAT:CTYPE TYPE)"
  [8719]: #x-28MGL-CUBE-3A-2ALET-OUTPUT-THROUGH-P-2A-20VARIABLE-29 "(MGL-CUBE:*LET-OUTPUT-THROUGH-P* VARIABLE)"
  [8816]: #x-28MGL-MAT-3A-40MAT-ASSEMBLING-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-ASSEMBLING MGL-PAX:SECTION)"
  [8866]: #x-28MGL-MAT-3A-40MAT-SHAPING-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-SHAPING MGL-PAX:SECTION)"
  [8b4f]: #x-28MGL-MAT-3A-40MAT-EXTENSION-API-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-EXTENSION-API MGL-PAX:SECTION)"
  [9221]: #x-28MGL-MAT-3AAXPY-21-20FUNCTION-29 "(MGL-MAT:AXPY! FUNCTION)"
  [923f]: #x-28MGL-CUBE-3ASYNCHRONIZATION-20-28MGL-PAX-3AACCESSOR-20MGL-CUBE-3ACUBE-29-29 "(MGL-CUBE:SYNCHRONIZATION (MGL-PAX:ACCESSOR MGL-CUBE:CUBE))"
  [9984]: #x-28MGL-MAT-3A-40MAT-NON-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-NON-DESTRUCTIVE-API MGL-PAX:SECTION)"
  [99b9]: #x-28MGL-CUBE-3AWITH-FACET-BARRIER-20MGL-PAX-3AMACRO-29 "(MGL-CUBE:WITH-FACET-BARRIER MGL-PAX:MACRO)"
  [9e0b]: #x-28MGL-CUBE-3AVIEW-UP-TO-DATE-P-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "(MGL-CUBE:VIEW-UP-TO-DATE-P MGL-PAX:STRUCTURE-ACCESSOR)"
  [9ecd]: #x-28MGL-CUBE-3AVIEW-FACET-NAME-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "(MGL-CUBE:VIEW-FACET-NAME MGL-PAX:STRUCTURE-ACCESSOR)"
  [9fcc]: #x-28MGL-CUBE-3ACUBE-20CLASS-29 "(MGL-CUBE:CUBE CLASS)"
  [a238]: #x-28MGL-CUBE-3AWATCH-FACET-20GENERIC-FUNCTION-29 "(MGL-CUBE:WATCH-FACET GENERIC-FUNCTION)"
  [acfb]: #x-28MGL-MAT-3AGEMM-21-20FUNCTION-29 "(MGL-MAT:GEMM! FUNCTION)"
  [ad5b]: #x-28MGL-MAT-3ACOERCE-TO-CTYPE-20FUNCTION-29 "(MGL-MAT:COERCE-TO-CTYPE FUNCTION)"
  [afa0]: #x-28MGL-MAT-3A-40MAT-CUBLAS-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-CUBLAS MGL-PAX:SECTION)"
  [b0b5]: #x-28MGL-MAT-3A-40MAT-INTRODUCTION-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-INTRODUCTION MGL-PAX:SECTION)"
  [b61b]: #x-28MGL-CUBE-3AWITH-FACETS-20MGL-PAX-3AMACRO-29 "(MGL-CUBE:WITH-FACETS MGL-PAX:MACRO)"
  [b64e]: #x-28MGL-CUBE-3A-40DEFAULT-CALL-WITH-FACET-2A-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@DEFAULT-CALL-WITH-FACET* MGL-PAX:SECTION)"
  [b9c1]: #x-28MGL-CUBE-3ACHECK-NO-WATCHERS-20FUNCTION-29 "(MGL-CUBE:CHECK-NO-WATCHERS FUNCTION)"
  [ba88]: #x-28ARRAY-20MGL-CUBE-3AFACET-NAME-29 "(ARRAY MGL-CUBE:FACET-NAME)"
  [bdd6]: #x-28MGL-CUBE-3ADESTROY-FACET-20FUNCTION-29 "(MGL-CUBE:DESTROY-FACET FUNCTION)"
  [c00b]: #x-28MGL-MAT-3AWITH-CUDA-2A-20MGL-PAX-3AMACRO-29 "(MGL-MAT:WITH-CUDA* MGL-PAX:MACRO)"
  [c07a]: #x-28MGL-MAT-3AREPLACE-21-20FUNCTION-29 "(MGL-MAT:REPLACE! FUNCTION)"
  [c246]: #x-28MGL-MAT-3AWITH-SHAPE-AND-DISPLACEMENT-20MGL-PAX-3AMACRO-29 "(MGL-MAT:WITH-SHAPE-AND-DISPLACEMENT MGL-PAX:MACRO)"
  [c62e]: #x-28MGL-CUBE-3AVIEWS-20FUNCTION-29 "(MGL-CUBE:VIEWS FUNCTION)"
  [c65e]: #x-28MGL-MAT-3AFOREIGN-ARRAY-STRATEGY-20TYPE-29 "(MGL-MAT:FOREIGN-ARRAY-STRATEGY TYPE)"
  [caa5]: #x-28MGL-MAT-3A-40MAT-CURAND-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-CURAND MGL-PAX:SECTION)"
  [d56c]: #x-28MGL-MAT-3A-40MAT-INSTALLATION-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-INSTALLATION MGL-PAX:SECTION)"
  [d95a]: #x-28MGL-CUBE-3AFIND-VIEW-20FUNCTION-29 "(MGL-CUBE:FIND-VIEW FUNCTION)"
  [dc10]: #x-28MGL-MAT-3AREAD-MAT-20GENERIC-FUNCTION-29 "(MGL-MAT:READ-MAT GENERIC-FUNCTION)"
  [dd58]: #x-28MGL-MAT-3A-40MAT-WHAT-KIND-OF-MATRICES-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-WHAT-KIND-OF-MATRICES MGL-PAX:SECTION)"
  [e3f0]: #x-28MGL-MAT-3ABACKING-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "(MGL-MAT:BACKING-ARRAY MGL-CUBE:FACET-NAME)"
  [e71c]: #x-28MGL-MAT-3A-40MAT-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-DESTRUCTIVE-API MGL-PAX:SECTION)"
  [e868]: #x-28MGL-MAT-3A-2ACURAND-STATE-2A-20VARIABLE-29 "(MGL-MAT:*CURAND-STATE* VARIABLE)"
  [e8e7]: #x-28MGL-MAT-3A-40MAT-CACHING-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-CACHING MGL-PAX:SECTION)"
  [eb06]: #x-28MGL-CUBE-3A-40FACET-EXTENSION-API-20MGL-PAX-3ASECTION-29 "(MGL-CUBE:@FACET-EXTENSION-API MGL-PAX:SECTION)"
  [edce]: #x-28MGL-CUBE-3ACHECK-NO-WRITERS-20FUNCTION-29 "(MGL-CUBE:CHECK-NO-WRITERS FUNCTION)"
  [ee37]: #x-28MGL-MAT-3A-2AN-MEMCPY-HOST-TO-DEVICE-2A-20VARIABLE-29 "(MGL-MAT:*N-MEMCPY-HOST-TO-DEVICE* VARIABLE)"
  [ee90]: #x-28MGL-CUBE-3AUNWATCH-FACET-20GENERIC-FUNCTION-29 "(MGL-CUBE:UNWATCH-FACET GENERIC-FUNCTION)"
  [ef83]: #x-28MGL-MAT-3A-40MAT-RANDOM-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-RANDOM MGL-PAX:SECTION)"
  [f291]: #x-28MGL-MAT-3A-40MAT-CUDA-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-CUDA MGL-PAX:SECTION)"
  [f5c1]: #x-28MGL-MAT-3AMAT-DIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "(MGL-MAT:MAT-DIMENSIONS (MGL-PAX:READER MGL-MAT:MAT))"
  [f66e]: #x-28MGL-CUBE-3AWITH-FACET-20MGL-PAX-3AMACRO-29 "(MGL-CUBE:WITH-FACET MGL-PAX:MACRO)"
  [f966]: #x-28MGL-MAT-3A-40MAT-BASICS-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-BASICS MGL-PAX:SECTION)"
  [fa6c]: #x-28MGL-MAT-3ACURAND-XORWOW-STATE-20CLASS-29 "(MGL-MAT:CURAND-XORWOW-STATE CLASS)"
  [fd9c]: #x-28MGL-MAT-3A-2ADEFAULT-LISP-KERNEL-DECLARATIONS-2A-20VARIABLE-29 "(MGL-MAT:*DEFAULT-LISP-KERNEL-DECLARATIONS* VARIABLE)"
  [fe72]: #x-28MGL-MAT-3A-40MAT-DEBUGGING-20MGL-PAX-3ASECTION-29 "(MGL-MAT:@MAT-DEBUGGING MGL-PAX:SECTION)"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
