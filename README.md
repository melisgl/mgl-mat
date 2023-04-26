<a id="x-28MGL-MAT-3A-40MAT-MANUAL-20MGL-PAX-3ASECTION-29"></a>
# MAT Manual

## Table of Contents

- [1 The `MGL-MAT` ASDF System][caca]
- [2 Links][54ea]
- [3 Introduction][7e60]
    - [3.1 What's MGL-MAT?][ff55]
    - [3.2 What kind of matrices are supported?][2ea4]
    - [3.3 Where to Get it?][c96c]
- [4 Tutorial][1b99]
- [5 Basics][086b]
- [6 Element types][e798]
- [7 Printing][43e9]
- [8 Shaping][7bf8]
    - [8.1 Comparison to Lisp Arrays][2cbb]
    - [8.2 Functional Shaping][aa5b]
    - [8.3 Destructive Shaping][b6a7]
- [9 Assembling][46e6]
- [10 Caching][e678]
- [11 BLAS Operations][4958]
- [12 Destructive API][545d]
- [13 Non-destructive API][3e36]
- [14 Mappings][1600]
- [15 Random numbers][c669]
- [16 I/O][7feb]
- [17 Debugging][ac8d]
- [18 Facet API][8eda]
    - [18.1 Facets][7ecb]
    - [18.2 Foreign arrays][36b5]
    - [18.3 CUDA][5212]
        - [18.3.1 CUDA Memory Management][4e65]
- [19 Writing Extensions][8f0e]
    - [19.1 Lisp Extensions][d3b8]
    - [19.2 CUDA Extensions][93e1]
        - [19.2.1 CUBLAS][b116]
        - [19.2.2 CURAND][3993]

###### \[in package MGL-MAT\]
<a id="x-28-22mgl-mat-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 1 The `MGL-MAT` ASDF System

- Version: 0.1.0
- Description: [`MAT`][6d14] is library for working with multi-dimensional
  arrays which supports efficient interfacing to foreign and CUDA
  code. BLAS and CUBLAS bindings are available.
- Licence: MIT, see COPYING.
- Author: Gábor Melis <mega@retes.hu>
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-mat](http://melisgl.github.io/mgl-mat)
- Bug tracker: [https://github.com/melisgl/mgl-mat/issues](https://github.com/melisgl/mgl-mat/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-mat.git)

<a id="x-28MGL-MAT-3A-40MAT-LINKS-20MGL-PAX-3ASECTION-29"></a>
## 2 Links

Here is the [official
repository](https://github.com/melisgl/mgl-mat) and the [HTML
documentation](http://melisgl.github.io/mgl-mat/mat-manual.html)
for the latest version.

<a id="x-28MGL-MAT-3A-40MAT-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
## 3 Introduction

<a id="x-28MGL-MAT-3A-40MAT-WHAT-IS-IT-20MGL-PAX-3ASECTION-29"></a>
### 3.1 What's MGL-MAT?

MGL-MAT is library for working with multi-dimensional arrays
which supports efficient interfacing to foreign and CUDA code with
automatic translations between cuda, foreign and lisp storage. BLAS
and CUBLAS bindings are available.

<a id="x-28MGL-MAT-3A-40MAT-WHAT-KIND-OF-MATRICES-20MGL-PAX-3ASECTION-29"></a>
### 3.2 What kind of matrices are supported?

Currently only row-major single and double float matrices are
supported, but it would be easy to add single and double precision
complex types too. Other numeric types, such as byte and native
integer, can be added too, but they are not supported by CUBLAS.
There are no restrictions on the number of dimensions, and reshaping
is possible. All functions operate on the visible portion of the
matrix (which is subject to displacement and shaping), invisible
elements are not affected.

<a id="x-28MGL-MAT-3A-40MAT-INSTALLATION-20MGL-PAX-3ASECTION-29"></a>
### 3.3 Where to Get it?

All dependencies are in quicklisp except for
[CL-CUDA](https://github.com/takagi/cl-cuda) that needs to be
fetched from github. Just clone CL-CUDA and MGL-MAT into
`quicklisp/local-projects/` and you are set. MGL-MAT itself lives
[at github](https://github.com/melisgl/mgl-mat), too.

Prettier-than-markdown HTML documentation cross-linked with other
libraries is
[available](http://melisgl.github.io/mgl-pax-world/mat-manual.html)
as part of [PAX World](http://melisgl.github.io/mgl-pax-world/).

<a id="x-28MGL-MAT-3A-40MAT-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
## 4 Tutorial

We are going to see how to create matrices, access their contents.

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

The most prominent difference from lisp arrays is that [`MAT`][6d14]s are
always numeric and their element type (called [`CTYPE`][1354] here) defaults
to `:DOUBLE`.

Individual elements can be accessed or set with [`MREF`][eb95]:

```commonlisp
(let ((m (make-mat '(2 3))))
  (setf (mref m 0 0) 1)
  (setf (mref m 0 1) (* 2 (mref m 0 0)))
  (incf (mref m 0 2) 4)
  m)
==> #<MAT 2x3 AB #2A((1.0d0 2.0d0 4.0d0) (0.0d0 0.0d0 0.0d0))>
```

Compared to [`AREF`][75d3] `MREF` is a very expensive operation and it's best
used sparingly. Instead, typical code relies much more on matrix
level operations:

```commonlisp
(princ (scal! 2 (fill! 3 (make-mat 4))))
.. #<MAT 4 BF #(6.0d0 6.0d0 6.0d0 6.0d0)>
==> #<MAT 4 ABF #(6.0d0 6.0d0 6.0d0 6.0d0)>
```

The content of a matrix can be accessed in various representations
called *facets*. MGL-MAT takes care of synchronizing these facets
by copying data around lazily, but doing its best to share storage
for facets that allow it.

Notice the `ABF` in the printed results. It illustrates that behind
the scenes [`FILL!`][510c] worked on the [`BACKING-ARRAY`][1fda]
facet (hence the `B`) that's basically a 1d lisp array. [`SCAL!`][3f21] on the
other hand made a foreign call to the BLAS `dscal` function for
which it needed the [`FOREIGN-ARRAY`][4f59] facet (`F`).
Finally, the `A` stands for the [`ARRAY`][cb94] facet that was
created when the array was printed. All facets are up-to-date (else
some of the characters would be lowercase). This is possible because
these three facets actually share storage which is never the case
for the [`CUDA-ARRAY`][7a7a] facet. Now if we have a
CUDA-capable GPU, CUDA can be enabled with [`WITH-CUDA*`][3db3]:

```commonlisp
(with-cuda* ()
  (princ (scal! 2 (fill! 3 (make-mat 4)))))
.. #<MAT 4 C #(6.0d0 6.0d0 6.0d0 6.0d0)>
==> #<MAT 4 A #(6.0d0 6.0d0 6.0d0 6.0d0)>
```

Note the lonely `C` showing that only the [`CUDA-ARRAY`][7a7a]
facet was used for both `FILL!` and `SCAL!`. When `WITH-CUDA*` exits and
destroys the CUDA context, it destroys all CUDA facets, moving their
data to the [`ARRAY`][cb94] facet, so the returned `MAT` only has
that facet.

When there is no high-level operation that does what we want, we may
need to add new operations. This is usually best accomplished by
accessing one of the facets directly, as in the following example:

<a id="x-28MGL-MAT-3ALOG-DET-EXAMPLE-20-28MGL-PAX-3AINCLUDE-20-28-3ASTART-20-28MGL-MAT-3ALOGDET-20FUNCTION-29-20-3AEND-20-28MGL-MAT-3A-3AEND-OF-LOGDET-EXAMPLE-20VARIABLE-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29"></a>
```commonlisp
(defun logdet (mat)
  "Logarithm of the determinant of MAT. Return -1, 1 or 0 (or
  equivalent) to correct for the sign, as the second value."
  (with-facets ((array (mat 'array :direction :input)))
    (lla:logdet array)))

```

Notice that [`LOGDET`][e1aa] doesn't know about CUDA at all. `WITH-FACETS`([`0`][ddd4] [`1`][0706])
gives it the content of the matrix as a normal multidimensional lisp
array, copying the data from the GPU or elsewhere if necessary. This
allows new representations ([`FACET`][6406]s) to be added easily and it also
avoids copying if the facet is already up-to-date. Of course, adding
CUDA support to `LOGDET` could make it more efficient.

Adding support for matrices that, for instance, live on a remote
machine is thus possible with a new facet type and existing code
would continue to work (albeit possibly slowly). Then one could
optimize the bottleneck operations by sending commands over the
network instead of copying data.

It is a bad idea to conflate resource management policy and
algorithms. MGL-MAT does its best to keep them separate.

<a id="x-28MGL-MAT-3A-40MAT-BASICS-20MGL-PAX-3ASECTION-29"></a>
## 5 Basics

<a id="x-28MGL-MAT-3AMAT-20CLASS-29"></a>
- [class] **MAT** *[CUBE][1659]*

    A `MAT` is a data [`CUBE`][1659] that is much like a lisp
    array, it supports `DISPLACEMENT`, arbitrary `DIMENSIONS` and
    `INITIAL-ELEMENT` with the usual semantics. However, a `MAT` supports
    different representations of the same data. See [Tutorial][1b99] for
    an introduction.

<a id="x-28MGL-MAT-3AMAT-CTYPE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-CTYPE** *MAT (:CTYPE = \*DEFAULT-MAT-CTYPE\*)*

    One of [`*SUPPORTED-CTYPES*`][fc7d]. The matrix can hold
    only values of this type.

<a id="x-28MGL-MAT-3AMAT-DISPLACEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-DISPLACEMENT** *MAT (:DISPLACEMENT = 0)*

    A value in the `[0,MAX-SIZE]` interval. This is
    like the DISPLACED-INDEX-OFFSET of a lisp array, but displacement
    is relative to the start of the underlying storage vector.

<a id="x-28MGL-MAT-3AMAT-DIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-DIMENSIONS** *MAT (:DIMENSIONS)*

    Like `ARRAY-DIMENSIONS`([`0`][b956] [`1`][046b]). It holds a list of
    dimensions, but it is allowed to pass in scalars too.

<a id="x-28MGL-MAT-3AMAT-DIMENSION-20FUNCTION-29"></a>
- [function] **MAT-DIMENSION** *MAT AXIS-NUMBER*

    Return the dimension along `AXIS-NUMBER`. Similar to
    [`ARRAY-DIMENSION`][046b].

<a id="x-28MGL-MAT-3AMAT-INITIAL-ELEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-INITIAL-ELEMENT** *MAT (:INITIAL-ELEMENT = 0)*

    If non-nil, then when a facet is created, it is
    filled with `INITIAL-ELEMENT` coerced to the appropriate numeric
    type. If `NIL`, then no initialization is performed.

<a id="x-28MGL-MAT-3AMAT-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-SIZE** *MAT*

    The number of elements in the visible portion of
    the array. This is always the product of the elements
    `MAT-DIMENSIONS`([`0`][73f6] [`1`][4815]) and is similar to [`ARRAY-TOTAL-SIZE`][e69b].

<a id="x-28MGL-MAT-3AMAT-MAX-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29"></a>
- [reader] **MAT-MAX-SIZE** *MAT (:MAX-SIZE)*

    The number of elements for which storage may be
    allocated. This is `DISPLACEMENT` + [`MAT-SIZE`][3573] + `SLACK` where `SLACK`
    is the number of trailing invisible elements.

<a id="x-28MGL-MAT-3AMAKE-MAT-20FUNCTION-29"></a>
- [function] **MAKE-MAT** *DIMENSIONS &REST ARGS &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*) (DISPLACEMENT 0) MAX-SIZE INITIAL-ELEMENT INITIAL-CONTENTS (SYNCHRONIZATION \*DEFAULT-SYNCHRONIZATION\*) DISPLACED-TO (CUDA-ENABLED \*DEFAULT-MAT-CUDA-ENABLED\*)*

    Return a new [`MAT`][6d14] object. If `INITIAL-CONTENTS` is given then the
    matrix contents are initialized with [`REPLACE!`][0ecb]. See class [`MAT`][6d14] for the
    description of the rest of the parameters. This is exactly
    what ([`MAKE-INSTANCE`][9743] '`MAT` ...) does except `DIMENSIONS` is not a
    keyword argument so that `MAKE-MAT` looks more like [`MAKE-ARRAY`][0b2b]. The
    semantics of `SYNCHRONIZATION` are desribed in the
    [Synchronization][a5a6] section.
    
    If specified, `DISPLACED-TO` must be a `MAT` object large enough (in the
    sense of its [`MAT-SIZE`][3573]), to hold `DISPLACEMENT` plus `(REDUCE #'*
    DIMENSIONS)` elements. Just like with `MAKE-ARRAY`, `INITIAL-ELEMENT`
    and `INITIAL-CONTENTS` must not be supplied together with
    `DISPLACED-TO`. See [Shaping][7bf8] for more.

<a id="x-28MGL-MAT-3AARRAY-TO-MAT-20FUNCTION-29"></a>
- [function] **ARRAY-TO-MAT** *ARRAY &KEY CTYPE (SYNCHRONIZATION \*DEFAULT-SYNCHRONIZATION\*)*

    Create a [`MAT`][6d14] that's equivalent to `ARRAY`. Displacement of the
    created array will be 0 and the size will be equal to
    [`ARRAY-TOTAL-SIZE`][e69b]. If `CTYPE` is non-nil, then it will be the ctype of
    the new matrix. Else `ARRAY`'s type is converted to a ctype. If there
    is no corresponding ctype, then [`*DEFAULT-MAT-CTYPE*`][3046] is used.
    Elements of `ARRAY` are coerced to `CTYPE`.
    
    Also see [Synchronization][a5a6].

<a id="x-28MGL-MAT-3AMAT-TO-ARRAY-20FUNCTION-29"></a>
- [function] **MAT-TO-ARRAY** *MAT*

<a id="x-28MGL-MAT-3AREPLACE-21-20FUNCTION-29"></a>
- [function] **REPLACE!** *MAT SEQ-OF-SEQS*

    Replace the contents of `MAT` with the elements of `SEQ-OF-SEQS`.
    `SEQ-OF-SEQS` is a nested sequence of sequences similar to the
    `INITIAL-CONTENTS` argument of [`MAKE-ARRAY`][0b2b]. The total number of
    elements must match the size of `MAT`. Returns `MAT`.
    
    `SEQ-OF-SEQS` may contain multi-dimensional arrays as *leafs*, so the
    following is legal:
    
    ```common-lisp
    (replace! (make-mat '(1 2 3)) '(#2A((1 2 3) (4 5 6))))
    ==> #<MAT 1x2x3 AB #3A(((1.0d0 2.0d0 3.0d0) (4.0d0 5.0d0 6.0d0)))>
    ```


<a id="x-28MGL-MAT-3AMREF-20FUNCTION-29"></a>
- [function] **MREF** *MAT &REST INDICES*

    Like [`AREF`][75d3] for arrays. Don't use this if you care about performance
    at all. SETFable. When set, the value is coerced to the ctype of `MAT`
    with [`COERCE-TO-CTYPE`][6ead]. Note that currently `MREF` always operates on
    the [`BACKING-ARRAY`][1fda] facet so it can trigger copying of facets. When
    it's [`SETF`][17b7]'ed, however, it will update the [`CUDA-ARRAY`][7a7a] if cuda is
    enabled and it is up-to-date or there are no facets at all.

<a id="x-28MGL-MAT-3AROW-MAJOR-MREF-20FUNCTION-29"></a>
- [function] **ROW-MAJOR-MREF** *MAT INDEX*

    Like [`ROW-MAJOR-AREF`][0a31] for arrays. Don't use this if you care about
    performance at all. SETFable. When set, the value is coerced to the
    ctype of `MAT` with [`COERCE-TO-CTYPE`][6ead]. Note that currently
    `ROW-MAJOR-MREF` always operates on the [`BACKING-ARRAY`][1fda] facet so it can
    trigger copying of facets. When it's [`SETF`][17b7]'ed, however, it will
    update the [`CUDA-ARRAY`][7a7a] if cuda is enabled and it is up-to-date or
    there are no facets at all.

<a id="x-28MGL-MAT-3AMAT-ROW-MAJOR-INDEX-20FUNCTION-29"></a>
- [function] **MAT-ROW-MAJOR-INDEX** *MAT &REST SUBSCRIPTS*

    Like [`ARRAY-ROW-MAJOR-INDEX`][ba46] for arrays.

<a id="x-28MGL-MAT-3A-40MAT-CTYPES-20MGL-PAX-3ASECTION-29"></a>
## 6 Element types

<a id="x-28MGL-MAT-3A-2ASUPPORTED-CTYPES-2A-20VARIABLE-29"></a>
- [variable] **\*SUPPORTED-CTYPES\*** *(:FLOAT :DOUBLE)*

<a id="x-28MGL-MAT-3ACTYPE-20TYPE-29"></a>
- [type] **CTYPE**

    This is basically `(MEMBER :FLOAT :DOUBLE)`.

<a id="x-28MGL-MAT-3A-2ADEFAULT-MAT-CTYPE-2A-20VARIABLE-29"></a>
- [variable] **\*DEFAULT-MAT-CTYPE\*** *:DOUBLE*

    By default [`MAT`][6d14]s are created with this ctype. One of `:FLOAT`
    or `:DOUBLE`.

<a id="x-28MGL-MAT-3ACOERCE-TO-CTYPE-20FUNCTION-29"></a>
- [function] **COERCE-TO-CTYPE** *X &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*)*

    Coerce the scalar `X` to the lisp type corresponding to `CTYPE`.

<a id="x-28MGL-MAT-3A-40MAT-PRINTING-20MGL-PAX-3ASECTION-29"></a>
## 7 Printing

<a id="x-28MGL-MAT-3A-2APRINT-MAT-2A-20VARIABLE-29"></a>
- [variable] **\*PRINT-MAT\*** *T*

    Controls whether the contents of a [`MAT`][6d14] object are printed as an
    array (subject to the standard printer control variables).

<a id="x-28MGL-MAT-3A-2APRINT-MAT-FACETS-2A-20VARIABLE-29"></a>
- [variable] **\*PRINT-MAT-FACETS\*** *T*

    Controls whether a summary of existing and up-to-date facets is
    printed when a [`MAT`][6d14] object is printed. The summary that looks like
    `ABcfh` indicates that all five facets ([`ARRAY`][cb94],
    [`BACKING-ARRAY`][1fda], [`CUDA-ARRAY`][7a7a],
    [`FOREIGN-ARRAY`][4f59], [`CUDA-HOST-ARRAY`][0789]) are
    present and the first two are up-to-date. A summary of a single #-
    indicates that there are no facets.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-20MGL-PAX-3ASECTION-29"></a>
## 8 Shaping

We are going to discuss various ways to change the visible portion
and dimensions of matrices. Conceptually a matrix has an *underlying
non-displaced storage vector*. For `(MAKE-MAT 10 :DISPLACEMENT
7 :MAX-SIZE 21)` this underlying vector looks like this:

    displacement | visible elements  | slack
    . . . . . . . 0 0 0 0 0 0 0 0 0 0 . . . .

Whenever a matrix is reshaped (or *displaced to* in lisp
terminology), its displacement and dimensions change but the
underlying vector does not.

The rules for accessing displaced matrices is the same as always:
multiple readers can run in parallel, but attempts to write will
result in an error if there are either readers or writers on any of
the matrices that share the same underlying vector.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-COMPARISON-TO-LISP-20MGL-PAX-3ASECTION-29"></a>
### 8.1 Comparison to Lisp Arrays

One way to reshape and displace [`MAT`][6d14] objects is with [`MAKE-MAT`][e3c3] and
its `DISPLACED-TO` argument whose semantics are similar to that of
[`MAKE-ARRAY`][0b2b] in that the displacement is *relative* to the
displacement of `DISPLACED-TO`.

```commonlisp
(let* ((base (make-mat 10 :initial-element 5 :displacement 1))
       (mat (make-mat 6 :displaced-to base :displacement 2)))
  (fill! 1 mat)
  (values base mat))
==> #<MAT 1+10+0 A #(5.0d0 5.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 5.0d0
-->                  5.0d0)>
==> #<MAT 3+6+2 AB #(1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0)>
```

There are important semantic differences compared to lisp arrays all
which follow from the fact that displacement operates on the
underlying conceptual non-displaced vector.

- Matrices can be displaced and have slack even without `DISPLACED-TO`
  just like `BASE` in the above example.

- It's legal to alias invisible elements of `DISPLACED-TO` as long as
  the new matrix fits into the underlying storage.

- Negative displacements are allowed with `DISPLACED-TO` as long as
  the adjusted displacement is non-negative.

- Further shaping operations can make invisible portions of the
  `DISPLACED-TO` matrix visible by changing the displacement.

- In contrast to [`ARRAY-DISPLACEMENT`][7669], [`MAT-DISPLACEMENT`][f5be] only returns
  an offset into the underlying storage vector.


<a id="x-28MGL-MAT-3A-40MAT-SHAPING-FUNCTIONAL-20MGL-PAX-3ASECTION-29"></a>
### 8.2 Functional Shaping

The following functions are collectively called the functional
shaping operations, since they don't alter their arguments in any
way. Still, since storage is aliased modification to the returned
matrix will affect the original.

<a id="x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-20FUNCTION-29"></a>
- [function] **RESHAPE-AND-DISPLACE** *MAT DIMENSIONS DISPLACEMENT*

    Return a new matrix of `DIMENSIONS` that aliases `MAT`'s storage at
    offset `DISPLACEMENT`. `DISPLACEMENT` 0 is equivalent to the start of
    the storage of `MAT` regardless of `MAT`'s displacement.

<a id="x-28MGL-MAT-3ARESHAPE-20FUNCTION-29"></a>
- [function] **RESHAPE** *MAT DIMENSIONS*

    Return a new matrix of `DIMENSIONS` whose displacement is the same as
    the displacement of `MAT`.

<a id="x-28MGL-MAT-3ADISPLACE-20FUNCTION-29"></a>
- [function] **DISPLACE** *MAT DISPLACEMENT*

    Return a new matrix that aliases `MAT`'s storage at offset
    `DISPLACEMENT`. `DISPLACEMENT` 0 is equivalent to the start of the
    storage of `MAT` regardless of `MAT`'s displacement. The returned matrix
    has the same dimensions as `MAT`.

<a id="x-28MGL-MAT-3A-40MAT-SHAPING-DESTRUCTIVE-20MGL-PAX-3ASECTION-29"></a>
### 8.3 Destructive Shaping

The following destructive operations don't alter the contents of
the matrix, but change what is visible. [`ADJUST!`][8100] is the odd one out,
it may create a new [`MAT`][6d14].

<a id="x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-21-20FUNCTION-29"></a>
- [function] **RESHAPE-AND-DISPLACE!** *MAT DIMENSIONS DISPLACEMENT*

    Change the visible (or active) portion of `MAT` by altering its
    displacement offset and dimensions. Future operations will only
    affect this visible portion as if the rest of the elements were not
    there. Return `MAT`.
    
    `DISPLACEMENT` + the new size must not exceed [`MAT-MAX-SIZE`][c4f8].
    Furthermore, there must be no facets being viewed (with `WITH-FACETS`([`0`][ddd4] [`1`][0706]))
    when calling this function as the identity of the facets is not
    stable.

<a id="x-28MGL-MAT-3ARESHAPE-21-20FUNCTION-29"></a>
- [function] **RESHAPE!** *MAT DIMENSIONS*

    Like [`RESHAPE-AND-DISPLACE!`][896c] but only alters the dimensions.

<a id="x-28MGL-MAT-3ADISPLACE-21-20FUNCTION-29"></a>
- [function] **DISPLACE!** *MAT DISPLACEMENT*

    Like [`RESHAPE-AND-DISPLACE!`][896c] but only alters the displacement.

<a id="x-28MGL-MAT-3ARESHAPE-TO-ROW-MATRIX-21-20FUNCTION-29"></a>
- [function] **RESHAPE-TO-ROW-MATRIX!** *MAT ROW*

    Reshape the 2d `MAT` to make only a single `ROW` visible. This is made
    possible by the row-major layout, hence no column counterpart.
    Return `MAT`.

<a id="x-28MGL-MAT-3AWITH-SHAPE-AND-DISPLACEMENT-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-SHAPE-AND-DISPLACEMENT** *(MAT &OPTIONAL (DIMENSIONS NIL) (DISPLACEMENT NIL)) &BODY BODY*

    Reshape and displace `MAT` if `DIMENSIONS` and/or `DISPLACEMENT` is given
    and restore the original shape and displacement after `BODY` is
    executed. If neither is specificed, then nothing will be changed,
    but `BODY` is still allowed to alter the shape and displacement.

<a id="x-28MGL-MAT-3AADJUST-21-20FUNCTION-29"></a>
- [function] **ADJUST!** *MAT DIMENSIONS DISPLACEMENT &KEY (DESTROY-OLD-P T)*

    Like [`RESHAPE-AND-DISPLACE!`][896c] but creates a new matrix if `MAT` isn't
    large enough. If a new matrix is created, the contents are not
    copied over and the old matrix is destroyed with [`DESTROY-CUBE`][dbac] if
    `DESTROY-OLD-P`.

<a id="x-28MGL-MAT-3A-40MAT-ASSEMBLING-20MGL-PAX-3ASECTION-29"></a>
## 9 Assembling

The functions here assemble a single [`MAT`][6d14] from a number of
[`MAT`][6d14]s.

<a id="x-28MGL-MAT-3ASTACK-21-20FUNCTION-29"></a>
- [function] **STACK!** *AXIS MATS MAT*

    Stack `MATS` along `AXIS` into `MAT` and return `MAT`. If `AXIS` is 0, place
    `MATS` into `MAT` below each other starting from the top. If `AXIS` is 1,
    place `MATS` side by side starting from the left. Higher `AXIS` are also
    supported. All dimensions except for `AXIS` must be the same for all
    `MATS`.

<a id="x-28MGL-MAT-3ASTACK-20FUNCTION-29"></a>
- [function] **STACK** *AXIS MATS &KEY (CTYPE \*DEFAULT-MAT-CTYPE\*)*

    Like [`STACK!`][cd9e] but return a new [`MAT`][6d14] of `CTYPE`.
    
    ```commonlisp
    (stack 1 (list (make-mat '(3 2) :initial-element 0)
                   (make-mat '(3 1) :initial-element 1)))
    ==> #<MAT 3x3 B #2A((0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0)
    -->                 (0.0d0 0.0d0 1.0d0))>
    ```


<a id="x-28MGL-MAT-3A-40MAT-CACHING-20MGL-PAX-3ASECTION-29"></a>
## 10 Caching

Allocating and initializing a [`MAT`][6d14] object and its necessary facets
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

<a id="x-28MGL-MAT-3AWITH-THREAD-CACHED-MAT-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-THREAD-CACHED-MAT** *(VAR DIMENSIONS &REST ARGS &KEY (PLACE :SCRATCH) (CTYPE '\*DEFAULT-MAT-CTYPE\*) (DISPLACEMENT 0) MAX-SIZE (INITIAL-ELEMENT 0) INITIAL-CONTENTS) &BODY BODY*

    Bind `VAR` to a matrix of `DIMENSIONS`, `CTYPE`, etc. Cache this matrix,
    and possibly reuse it later by reshaping it. When `BODY` exits the
    cached object is updated with the binding of `VAR` which `BODY` may
    change.
    
    There is a separate cache for each thread and each `PLACE` (under
    [`EQ`][a1d4]). Since every cache holds exactly one [`MAT`][6d14] per `CTYPE`, nested
    `WITH-THREAD-CACHED-MAT` often want to use different `PLACE`s. By
    convention, these places are called `:SCRATCH-1`, `:SCRATCH-2`,
    etc.

<a id="x-28MGL-MAT-3AWITH-THREAD-CACHED-MATS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-THREAD-CACHED-MATS** *SPECS &BODY BODY*

    A shorthand for writing nested [`WITH-THREAD-CACHED-MAT`][aa72] calls.
    
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
    ```


<a id="x-28MGL-MAT-3AWITH-ONES-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-ONES** *(VAR DIMENSIONS &KEY (CTYPE '\*DEFAULT-MAT-CTYPE\*) (PLACE :ONES)) &BODY BODY*

    Bind `VAR` to a matrix of `DIMENSIONS` whose every element is 1. The
    matrix is cached for efficiency.

<a id="x-28MGL-MAT-3A-40MAT-BLAS-20MGL-PAX-3ASECTION-29"></a>
## 11 BLAS Operations

Only some BLAS functions are implemented, but it should be easy to
add more as needed. All of them default to using CUDA, if it is
initialized and enabled (see [`USE-CUDA-P`][c442]).

Level 1 BLAS operations

<a id="x-28MGL-MAT-3AASUM-20FUNCTION-29"></a>
- [function] **ASUM** *X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Return the l1 norm of `X`, that is, sum of the absolute values of its
    elements.

<a id="x-28MGL-MAT-3AAXPY-21-20FUNCTION-29"></a>
- [function] **AXPY!** *ALPHA X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Set `Y` to `ALPHA` \* `X` + `Y`. Return `Y`.

<a id="x-28MGL-MAT-3ACOPY-21-20FUNCTION-29"></a>
- [function] **COPY!** *X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Copy `X` into `Y`. Return `Y`.

<a id="x-28CL-CUDA-2ELANG-2EBUILT-IN-3ADOT-20FUNCTION-29"></a>
- [function] **DOT** *X Y &KEY (N (MAT-SIZE X)) (INCX 1) (INCY 1)*

    Return the dot product of `X` and `Y`.

<a id="x-28MGL-MAT-3ANRM2-20FUNCTION-29"></a>
- [function] **NRM2** *X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Return the l2 norm of `X`, which is the square root of the sum of the
    squares of its elements.

<a id="x-28MGL-MAT-3ASCAL-21-20FUNCTION-29"></a>
- [function] **SCAL!** *ALPHA X &KEY (N (MAT-SIZE X)) (INCX 1)*

    Set `X` to `ALPHA` \* `X`. Return `X`.

Level 3 BLAS operations

<a id="x-28MGL-MAT-3AGEMM-21-20FUNCTION-29"></a>
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


<a id="x-28MGL-MAT-3A-40MAT-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29"></a>
## 12 Destructive API

<a id="x-28MGL-MAT-3A-2ESQUARE-21-20FUNCTION-29"></a>
- [function] **.SQUARE!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise square. Return `X`.

<a id="x-28MGL-MAT-3A-2ESQRT-21-20FUNCTION-29"></a>
- [function] **.SQRT!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise square root. Return `X`.

<a id="x-28MGL-MAT-3A-2ELOG-21-20FUNCTION-29"></a>
- [function] **.LOG!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise natural logarithm. Return `X`.

<a id="x-28MGL-MAT-3A-2EEXP-21-20FUNCTION-29"></a>
- [function] **.EXP!** *X &KEY (N (MAT-SIZE X))*

    Apply [`EXP`][45be] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2EEXPT-21-20FUNCTION-29"></a>
- [function] **.EXPT!** *X POWER*

    Raise matrix `X` to `POWER` in an elementwise manner. Return `X`. Note
    that CUDA and non-CUDA implementations may disagree on the treatment
    of NaNs, infinities and complex results. In particular, the lisp
    implementation always computes the [`REALPART`][89bf] of the results while
    CUDA's pow() returns NaNs instead of complex numbers.

<a id="x-28MGL-MAT-3A-2EINV-21-20FUNCTION-29"></a>
- [function] **.INV!** *X &KEY (N (MAT-SIZE X))*

    Set `X` to its elementwise inverse `(/ 1 X)`. Return `X`.

<a id="x-28MGL-MAT-3A-2ELOGISTIC-21-20FUNCTION-29"></a>
- [function] **.LOGISTIC!** *X &KEY (N (MAT-SIZE X))*

    Destructively apply the logistic function to `X` in an elementwise
    manner. Return `X`.

<a id="x-28MGL-MAT-3A-2E-2B-21-20FUNCTION-29"></a>
- [function] **.+!** *ALPHA X*

    Add the scalar `ALPHA` to each element of `X` destructively modifying
    `X`. Return `X`.

<a id="x-28MGL-MAT-3A-2E-2A-21-20FUNCTION-29"></a>
- [function] **.\*!** *X Y*

<a id="x-28MGL-MAT-3AGEEM-21-20FUNCTION-29"></a>
- [function] **GEEM!** *ALPHA A B BETA C*

    Like [`GEMM!`][e95b], but multiplication is elementwise. This is not a
    standard BLAS routine.

<a id="x-28MGL-MAT-3AGEERV-21-20FUNCTION-29"></a>
- [function] **GEERV!** *ALPHA A X BETA B*

    GEneric Elementwise Row - Vector multiplication. `B = beta * B +
    alpha a .* X*` where `X*` is a matrix of the same shape as `A` whose
    every row is `X`. Perform elementwise multiplication on each row of `A`
    with the vector `X` and add the scaled result to the corresponding row
    of `B`. Return `B`. This is not a standard BLAS routine.

<a id="x-28MGL-MAT-3A-2E-3C-21-20FUNCTION-29"></a>
- [function] **.\<!** *X Y*

    For each element of `X` and `Y` set `Y` to 1 if the element in `Y` is
    greater than the element in `X`, and to 0 otherwise. Return `Y`.

<a id="x-28MGL-MAT-3A-2EMIN-21-20FUNCTION-29"></a>
- [function] **.MIN!** *ALPHA X*

    Set each element of `X` to `ALPHA` if it's greater than `ALPHA`. Return
    `X`.

<a id="x-28MGL-MAT-3A-2EMAX-21-20FUNCTION-29"></a>
- [function] **.MAX!** *ALPHA X*

    Set each element of `X` to `ALPHA` if it's less than `ALPHA`. Return `X`.

<a id="x-28MGL-MAT-3AADD-SIGN-21-20FUNCTION-29"></a>
- [function] **ADD-SIGN!** *ALPHA A BETA B*

    Add the elementwise sign (-1, 0 or 1 for negative, zero and
    positive numbers respectively) of `A` times `ALPHA` to `BETA` \* `B`. Return
    `B`.

<a id="x-28MGL-MAT-3AFILL-21-20FUNCTION-29"></a>
- [function] **FILL!** *ALPHA X &KEY (N (MAT-SIZE X))*

    Fill matrix `X` with `ALPHA`. Return `X`.

<a id="x-28MGL-MAT-3ASUM-21-20FUNCTION-29"></a>
- [function] **SUM!** *X Y &KEY AXIS (ALPHA 1) (BETA 0)*

    Sum matrix `X` along `AXIS` and add `ALPHA` \* `SUM`s to `BETA` \* `Y`
    destructively modifying `Y`. Return `Y`. On a 2d matrix (nothing else is
    supported currently), if `AXIS` is 0, then columns are summed, if `AXIS`
    is 1 then rows are summed.

<a id="x-28MGL-MAT-3ASCALE-ROWS-21-20FUNCTION-29"></a>
- [function] **SCALE-ROWS!** *SCALES A &KEY (RESULT A)*

    Set `RESULT` to `DIAG(SCALES)*A` and return it. `A` is an `MxN`
    matrix, `SCALES` is treated as a length `M` vector.

<a id="x-28MGL-MAT-3ASCALE-COLUMNS-21-20FUNCTION-29"></a>
- [function] **SCALE-COLUMNS!** *SCALES A &KEY (RESULT A)*

    Set `RESULT` to `A*DIAG(SCALES)` and return it. `A` is an `MxN`
    matrix, `SCALES` is treated as a length `N` vector.

<a id="x-28MGL-MAT-3A-2ESIN-21-20FUNCTION-29"></a>
- [function] **.SIN!** *X &KEY (N (MAT-SIZE X))*

    Apply [`SIN`][5ce7] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2ECOS-21-20FUNCTION-29"></a>
- [function] **.COS!** *X &KEY (N (MAT-SIZE X))*

    Apply [`COS`][3164] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2ETAN-21-20FUNCTION-29"></a>
- [function] **.TAN!** *X &KEY (N (MAT-SIZE X))*

    Apply [`TAN`][657e] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2ESINH-21-20FUNCTION-29"></a>
- [function] **.SINH!** *X &KEY (N (MAT-SIZE X))*

    Apply [`SINH`][2098] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2ECOSH-21-20FUNCTION-29"></a>
- [function] **.COSH!** *X &KEY (N (MAT-SIZE X))*

    Apply [`COSH`][f43d] elementwise to `X` in a destructive manner. Return `X`.

<a id="x-28MGL-MAT-3A-2ETANH-21-20FUNCTION-29"></a>
- [function] **.TANH!** *X &KEY (N (MAT-SIZE X))*

    Apply [`TANH`][7582] elementwise to `X` in a destructive manner. Return `X`.

Finally, some neural network operations.

<a id="x-28MGL-MAT-3ACONVOLVE-21-20FUNCTION-29"></a>
- [function] **CONVOLVE!** *X W Y &KEY START STRIDE ANCHOR BATCHED*

    `Y` = `Y` + conv(`X`, `W`) and return `Y`. If `BATCHED`, then the first
    dimension of `X` and `Y` is the number of elements in the batch (B),
    else B is assumed to be 1. The rest of the dimensions encode the
    input (`X`) and output (Y\} N dimensional feature maps. `START`, `STRIDE`
    and `ANCHOR` are lists of length N. `START` is the multi-dimensional
    index of the first element of the input feature map (for each
    element in the batch) for which the convolution must be computed.
    Then ([`ELT`][87ca] `STRIDE` (- N 1)) is added to the last element of `START` and
    so on until ([`ARRAY-DIMENSION`][046b] `X` 1) is reached. Then the last element
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

<a id="x-28MGL-MAT-3ADERIVE-CONVOLVE-21-20FUNCTION-29"></a>
- [function] **DERIVE-CONVOLVE!** *X XD W WD YD &KEY START STRIDE ANCHOR BATCHED*

    Add the dF/dX to `XD` and and dF/dW to `WD` where `YD` is dF/dY for some
    function F where Y is the result of convolution with the same
    arguments. 

<a id="x-28MGL-MAT-3AMAX-POOL-21-20FUNCTION-29"></a>
- [function] **MAX-POOL!** *X Y &KEY START STRIDE ANCHOR BATCHED POOL-DIMENSIONS*

<a id="x-28MGL-MAT-3ADERIVE-MAX-POOL-21-20FUNCTION-29"></a>
- [function] **DERIVE-MAX-POOL!** *X XD Y YD &KEY START STRIDE ANCHOR BATCHED POOL-DIMENSIONS*

    Add the dF/dX to `XD` and and dF/dW to WD where `YD` is dF/dY for some
    function F where `Y` is the result of [`MAX-POOL!`][e17e] with the same
    arguments. 

<a id="x-28MGL-MAT-3A-40MAT-NON-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29"></a>
## 13 Non-destructive API

<a id="x-28MGL-MAT-3ACOPY-MAT-20FUNCTION-29"></a>
- [function] **COPY-MAT** *A*

    Return a copy of the active portion with regards to displacement
    and shape of `A`. 

<a id="x-28MGL-MAT-3ACOPY-ROW-20FUNCTION-29"></a>
- [function] **COPY-ROW** *A ROW*

    Return `ROW` of `A` as a new 1d matrix.

<a id="x-28MGL-MAT-3ACOPY-COLUMN-20FUNCTION-29"></a>
- [function] **COPY-COLUMN** *A COLUMN*

    Return `COLUMN` of `A` as a new 1d matrix.

<a id="x-28MGL-MAT-3AMAT-AS-SCALAR-20FUNCTION-29"></a>
- [function] **MAT-AS-SCALAR** *A*

    Return the first element of `A`. `A` must be of size 1.

<a id="x-28MGL-MAT-3ASCALAR-AS-MAT-20FUNCTION-29"></a>
- [function] **SCALAR-AS-MAT** *X &KEY (CTYPE (LISP-\>CTYPE (TYPE-OF X)))*

    Return a matrix of one dimension and one element: `X`. `CTYPE`, the
    type of the matrix, defaults to the ctype corresponding to the type
    of `X`.

<a id="x-28MGL-MAT-3AM-3D-20FUNCTION-29"></a>
- [function] **M=** *A B*

    Check whether `A` and `B`, which must be matrices of the same size, are
    elementwise equal.

<a id="x-28MGL-MAT-3ATRANSPOSE-20FUNCTION-29"></a>
- [function] **TRANSPOSE** *A*

    Return the transpose of `A`.

<a id="x-28MGL-MAT-3AM-2A-20FUNCTION-29"></a>
- [function] **M\*** *A B &KEY TRANSPOSE-A? TRANSPOSE-B?*

    Compute op(`A`) \* op(`B`). Where op is either the identity or the
    transpose operation depending on `TRANSPOSE-A?` and `TRANSPOSE-B?`.

<a id="x-28MGL-MAT-3AMM-2A-20FUNCTION-29"></a>
- [function] **MM\*** *M &REST ARGS*

    Convenience function to multiply several matrices. 
    
    (mm\* a b c) => a \* b \* c

<a id="x-28MGL-MAT-3AM--20FUNCTION-29"></a>
- [function] **M-** *A B*

    Return `A` - `B`.

<a id="x-28MGL-MAT-3AM-2B-20FUNCTION-29"></a>
- [function] **M+** *A B*

    Return `A` + `B`.

<a id="x-28MGL-MAT-3AINVERT-20FUNCTION-29"></a>
- [function] **INVERT** *A*

    Return the inverse of `A`.

<a id="x-28MGL-MAT-3ALOGDET-20FUNCTION-29"></a>
- [function] **LOGDET** *MAT*

    Logarithm of the determinant of `MAT`. Return -1, 1 or 0 (or
    equivalent) to correct for the sign, as the second value.

<a id="x-28MGL-MAT-3A-40MAT-MAPPINGS-20MGL-PAX-3ASECTION-29"></a>
## 14 Mappings

<a id="x-28MGL-MAT-3AMAP-CONCAT-20FUNCTION-29"></a>
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


<a id="x-28MGL-MAT-3AMAP-DISPLACEMENTS-20FUNCTION-29"></a>
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


<a id="x-28MGL-MAT-3AMAP-MATS-INTO-20FUNCTION-29"></a>
- [function] **MAP-MATS-INTO** *RESULT-MAT FN &REST MATS*

    Like [`CL:MAP-INTO`][ce52] but for [`MAT`][6d14] objects. Destructively modifies
    `RESULT-MAT` to contain the results of applying `FN` to each element in
    the argument `MATS` in turn.

<a id="x-28MGL-MAT-3A-40MAT-RANDOM-20MGL-PAX-3ASECTION-29"></a>
## 15 Random numbers

Unless noted these work efficiently with CUDA.

<a id="x-28MGL-MAT-3ACOPY-RANDOM-STATE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **COPY-RANDOM-STATE** *STATE*

    Return a copy of `STATE` be it a lisp or cuda random
    state.

<a id="x-28MGL-MAT-3AUNIFORM-RANDOM-21-20FUNCTION-29"></a>
- [function] **UNIFORM-RANDOM!** *MAT &KEY (LIMIT 1)*

    Fill `MAT` with random numbers sampled uniformly from the \[0,LIMIT)
    interval of `MAT`'s type.

<a id="x-28MGL-MAT-3AGAUSSIAN-RANDOM-21-20FUNCTION-29"></a>
- [function] **GAUSSIAN-RANDOM!** *MAT &KEY (MEAN 0) (STDDEV 1)*

    Fill `MAT` with independent normally distributed random numbers with
    `MEAN` and `STDDEV`.

<a id="x-28MGL-MAT-3AMV-GAUSSIAN-RANDOM-20FUNCTION-29"></a>
- [function] **MV-GAUSSIAN-RANDOM** *&KEY MEANS COVARIANCES*

    Return a column vector of samples from the multivariate normal
    distribution defined by `MEANS` (Nx1) and `COVARIANCES` (NxN). No CUDA
    implementation.

<a id="x-28MGL-MAT-3AORTHOGONAL-RANDOM-21-20FUNCTION-29"></a>
- [function] **ORTHOGONAL-RANDOM!** *M &KEY (SCALE 1)*

    Fill the matrix `M` with random values in such a way that `M^T * M`
    is the identity matrix (or something close if `M` is wide). Return `M`.

<a id="x-28MGL-MAT-3A-40MAT-IO-20MGL-PAX-3ASECTION-29"></a>
## 16 I/O

<a id="x-28MGL-MAT-3A-2AMAT-HEADERS-2A-20VARIABLE-29"></a>
- [variable] **\*MAT-HEADERS\*** *T*

    If true, a header with [`MAT-CTYPE`][62f5] and [`MAT-SIZE`][3573] is written by
    [`WRITE-MAT`][f0db] before the contents and [`READ-MAT`][420d] checks that these match
    the matrix into which it is reading.

<a id="x-28MGL-MAT-3AWRITE-MAT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **WRITE-MAT** *MAT STREAM*

    Write `MAT` to binary `STREAM` in portable binary
    format. Return `MAT`. Displacement and size are taken into account,
    only visible elements are written. Also see [`*MAT-HEADERS*`][a9da].

<a id="x-28MGL-MAT-3AREAD-MAT-20GENERIC-FUNCTION-29"></a>
- [generic-function] **READ-MAT** *MAT STREAM*

    Destructively modify the visible portion (with
    regards to displacement and shape) of `MAT` by reading [`MAT-SIZE`][3573] number
    of elements from binary `STREAM`. Return `MAT`. Also see
    [`*MAT-HEADERS*`][a9da].

<a id="x-28MGL-MAT-3A-40MAT-DEBUGGING-20MGL-PAX-3ASECTION-29"></a>
## 17 Debugging

The largest class of bugs has to do with synchronization of facets
being broken. This is almost always caused by an operation that
mispecifies the `DIRECTION` argument of [`WITH-FACET`][0706]. For example, the
matrix argument of [`SCAL!`][3f21] should be accessed with direciton `:IO`. But
if it's `:INPUT` instead, then subsequent access to the `ARRAY`([`0`][cb94] [`1`][e121]) facet
will not see the changes made by [`AXPY!`][7408], and if it's `:OUTPUT`, then
any changes made to the `ARRAY` facet since the last update of the
[`CUDA-ARRAY`][7a7a] facet will not be copied and from the wrong input `SCAL!`
will compute the wrong result.

Using the SLIME inspector or trying to access the
[`CUDA-ARRAY`][7a7a] facet from threads other than the one in
which the corresponding CUDA context was initialized will fail. For
now, the easy way out is to debug the code with CUDA disabled (see
[`*CUDA-ENABLED*`][aba0]).

Another thing that tends to come up is figuring out where memory is
used.

<a id="x-28MGL-MAT-3AMAT-ROOM-20FUNCTION-29"></a>
- [function] **MAT-ROOM** *&KEY (STREAM \*STANDARD-OUTPUT\*) (VERBOSE T)*

    Calls [`FOREIGN-ROOM`][d5f8] and [`CUDA-ROOM`][010e].

<a id="x-28MGL-MAT-3AWITH-MAT-COUNTERS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-MAT-COUNTERS** *(&KEY COUNT N-BYTES) &BODY BODY*

    Count all [`MAT`][6d14] allocations and also the number of bytes they may
    require. *May require* here really means an upper bound,
    because `(MAKE-MAT (EXPT 2 60))` doesn't actually uses memory until
    one of its facets is accessed (don't simply evaluate it though,
    printing the result will access the [`ARRAY`][cb94] facet if
    [`*PRINT-MAT*`][1f37]). Also, while facets today all require the same number
    of bytes, this may change in the future. This is a debugging tool,
    don't use it in production.
    
    ```common-lisp
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


<a id="x-28MGL-MAT-3A-40MAT-FACET-API-20MGL-PAX-3ASECTION-29"></a>
## 18 Facet API



<a id="x-28MGL-MAT-3A-40MAT-FACETS-20MGL-PAX-3ASECTION-29"></a>
### 18.1 Facets

A [`MAT`][6d14] is a [`CUBE`][1659] (see [Cube Manual][713f]) whose facets are
different representations of numeric arrays. These facets can be
accessed with `WITH-FACETS`([`0`][ddd4] [`1`][0706]) with one of the following
[`FACET-NAME`][799d] locatives:

<a id="x-28MGL-MAT-3ABACKING-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
- [facet-name] **BACKING-ARRAY**

    The corresponding facet's value is a one dimensional lisp array or
    a static vector that also looks exactly like a lisp array but is
    allocated in foreign memory. See [`*FOREIGN-ARRAY-STRATEGY*`][193b].

<a id="x-28ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
- [facet-name] **ARRAY**

    Same as [`BACKING-ARRAY`][1fda] if the matrix is one-dimensional, all
    elements are visible (see [Shaping][7bf8]), else it's a lisp array
    displaced to the backing array.

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
- [facet-name] **FOREIGN-ARRAY**

    The facet's value is a [`FOREIGN-ARRAY`][cdc3] which is an
    `OFFSET-POINTER` wrapping a CFFI pointer. See
    [`*FOREIGN-ARRAY-STRATEGY*`][193b].

<a id="x-28MGL-MAT-3ACUDA-HOST-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
- [facet-name] **CUDA-HOST-ARRAY**

    This facet's value is a basically the same as that of
    [`FOREIGN-ARRAY`][4f59]. In fact, they share storage. The
    difference is that accessing [`CUDA-HOST-ARRAY`][0789] ensures
    that the foreign memory region is page-locked and registered with
    the CUDA Driver API function cuMemHostRegister(). Copying between
    GPU memory ([`CUDA-ARRAY`][7a7a]) and registered memory is
    significantly faster than with non-registered memory and also allows
    overlapping copying with computation. See
    [`WITH-SYNCING-CUDA-FACETS`][9fff].

<a id="x-28MGL-MAT-3ACUDA-ARRAY-20MGL-CUBE-3AFACET-NAME-29"></a>
- [facet-name] **CUDA-ARRAY**

    The facet's value is a `CUDA-ARRAY`, which is an `OFFSET-POINTER`
    wrapping a `CL-CUDA.DRIVER-API:CU-DEVICE-PTR`, allocated with
    `CL-CUDA.DRIVER-API:CU-MEM-ALLOC` and freed automatically.

Facets bound by with `WITH-FACETS`([`0`][ddd4] [`1`][0706]) are to be treated as dynamic
extent: it is not allowed to keep a reference to them beyond the
dynamic scope of `WITH-FACETS`.

For example, to implement the [`FILL!`][510c] operation using only the
[`BACKING-ARRAY`][1fda], one could do this:

```commonlisp
(let ((displacement (mat-displacement x))
      (size (mat-size x)))
 (with-facets ((x* (x 'backing-array :direction :output)))
   (fill x* 1 :start displacement :end (+ displacement size))))
```

[`DIRECTION`][69de] is `:OUTPUT` because we clobber all values in `X`. Armed
with this knowledge about the direction, `WITH-FACETS` will not copy
data from another facet if the backing array is not up-to-date.

To transpose a 2d matrix with the [`ARRAY`][cb94] facet:

```commonlisp
(destructuring-bind (n-rows n-columns) (mat-dimensions x)
  (with-facets ((x* (x 'array :direction :io)))
    (dotimes (row n-rows)
      (dotimes (column n-columns)
        (setf (aref x* row column) (aref x* column row))))))
```

Note that `DIRECTION` is `:IO`, because we need the data in this facet
to be up-to-date (that's the input part) and we are invalidating all
other facets by changing values (that's the output part).

To sum the values of a matrix using the [`FOREIGN-ARRAY`][4f59]
facet:

```commonlisp
(let ((sum 0))
  (with-facets ((x* (x 'foreign-array :direction :input)))
    (let ((pointer (offset-pointer x*)))
      (loop for index below (mat-size x)
            do (incf sum (cffi:mem-aref pointer (mat-ctype x) index)))))
  sum)
```

See `DIRECTION` for a complete description of `:INPUT`, `:OUTPUT` and `:IO`.
For [`MAT`][6d14] objects, that needs to be refined. If a `MAT` is reshaped
and/or displaced in a way that not all elements are visible then
those elements are always kept intact and copied around. This is
accomplished by turning `:OUTPUT` into `:IO` automatically on such `MAT`s.

We have finished our introduction to the various facets. It must be
said though that one can do anything without ever accessing a facet
directly or even being aware of them as most operations on `MAT`s
take care of choosing the most appropriate facet behind the scenes.
In particular, most operations automatically use CUDA, if available
and initialized. See [`WITH-CUDA*`][3db3] for detail.

<a id="x-28MGL-MAT-3A-40MAT-FOREIGN-20MGL-PAX-3ASECTION-29"></a>
### 18.2 Foreign arrays

One facet of [`MAT`][6d14] objects is [`FOREIGN-ARRAY`][4f59] which is
backed by a memory area that can be a pinned lisp array or is
allocated in foreign memory depending on [`*FOREIGN-ARRAY-STRATEGY*`][193b].

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-20CLASS-29"></a>
- [class] **FOREIGN-ARRAY**

    [`FOREIGN-ARRAY`][cdc3] wraps a foreign pointer (in
    the sense of `CFFI:POINTERP`). That is, both `OFFSET-POINTER` and
    `BASE-POINTER` return a foreign pointer. There are no other public
    operations that work with [`FOREIGN-ARRAY`][cdc3] objects, their sole
    purpose is represent facets of [`MAT`][6d14] objects.

<a id="x-28MGL-MAT-3A-2AFOREIGN-ARRAY-STRATEGY-2A-20-28VARIABLE-20-22-see-20below--22-29-29"></a>
- [variable] **\*FOREIGN-ARRAY-STRATEGY\*** *"-see below-"*

    One of `:PINNED`, `:STATIC` and `:CUDA-HOST` (see type
    [`FOREIGN-ARRAY-STRATEGY`][4495]). This variable controls how foreign arrays
    are handled and it can be changed at any time.
    
    If it's `:PINNED` (only supported if ([`PINNING-SUPPORTED-P`][fae1]), then no
    separate storage is allocated for the foreign array. Instead, it
    aliases the lisp array (via the [`BACKING-ARRAY`][1fda] facet).
    
    If it's `:STATIC`, then the lisp backing arrays are allocated
    statically via the static-vectors library. On some implementations,
    explicit freeing of static vectors is necessary, this is taken care
    of by finalizers or can be controlled with [`WITH-FACET-BARRIER`][c44d].
    [`DESTROY-CUBE`][dbac] and [`DESTROY-FACET`][39a0] may also be of help.
    
    `:CUDA-HOST` is the same as `:STATIC`, but any copies to/from the
    GPU (i.e. the [`CUDA-ARRAY`][7a7a] facet) will be done via the
    [`CUDA-HOST-ARRAY`][0789] facet whose memory pages will also be
    locked and registered with `cuMemHostRegister` which allows quicker
    and asynchronous copying to and from CUDA land.
    
    The default is `:PINNED` if available, because it's the most
    efficient. If pinning is not available, then it's `:STATIC`.

<a id="x-28MGL-MAT-3AFOREIGN-ARRAY-STRATEGY-20TYPE-29"></a>
- [type] **FOREIGN-ARRAY-STRATEGY**

    One of `:PINNED`, `:STATIC` and `:CUDA-HOST`. See
    [`*FOREIGN-ARRAY-STRATEGY*`][193b] for their semantics.

<a id="x-28MGL-MAT-3APINNING-SUPPORTED-P-20FUNCTION-29"></a>
- [function] **PINNING-SUPPORTED-P**

    Return true iff the lisp implementation efficiently supports
    pinning lisp arrays. Pinning ensures that the garbage collector
    doesn't move the array in memory. Currently this is only supported on
    SBCL gencgc platforms.

<a id="x-28MGL-MAT-3AFOREIGN-ROOM-20FUNCTION-29"></a>
- [function] **FOREIGN-ROOM** *&KEY (STREAM \*STANDARD-OUTPUT\*) (VERBOSE T)*

    Print a summary of foreign memory usage to `STREAM`. If `VERBOSE`, make
    the output human easily readable, else try to present it in a very
    concise way. Sample output with `VERBOSE`:
    
    ```
    Foreign memory usage:
    foreign arrays: 450 (used bytes: 3,386,295,808)
    ```
    
    The same data presented with `VERBOSE` false:
    
    ```
    f: 450 (3,386,295,808)
    ```


<a id="x-28MGL-MAT-3A-40MAT-CUDA-20MGL-PAX-3ASECTION-29"></a>
### 18.3 CUDA

<a id="x-28MGL-MAT-3ACUDA-AVAILABLE-P-20FUNCTION-29"></a>
- [function] **CUDA-AVAILABLE-P** *&KEY (DEVICE-ID 0)*

    Check that a cuda context is already in initialized in the current
    thread or a device with `DEVICE-ID` is available.

<a id="x-28MGL-MAT-3AWITH-CUDA-2A-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-CUDA\*** *(&KEY (ENABLED '\*CUDA-ENABLED\*) (DEVICE-ID '\*CUDA-DEFAULT-DEVICE-ID\*) (RANDOM-SEED '\*CUDA-DEFAULT-RANDOM-SEED\*) (N-RANDOM-STATES '\*CUDA-DEFAULT-N-RANDOM-STATES\*) N-POOL-BYTES) &BODY BODY*

    Initializes CUDA with with all bells and whistles before `BODY` and
    deinitializes it after. Simply wrapping `WITH-CUDA*` around a piece
    code is enough to make use of the first available CUDA device or
    fall back on blas and lisp kernels if there is none.
    
    If CUDA is already initialized, then it sets up a facet barrier
    which destroys [`CUDA-ARRAY`][7a7a] and [`CUDA-HOST-ARRAY`][0789] facets after ensuring
    that the [`ARRAY`][cb94] facet is up-to-date.
    
    Else, if CUDA is available and `ENABLED`, then in addition to the
    facet barrier, a CUDA context is set up, [`*N-MEMCPY-HOST-TO-DEVICE*`][9d0b],
    [`*N-MEMCPY-DEVICE-TO-HOST*`][7332] are bound to zero, a cublas handle
    created, and [`*CURAND-STATE*`][8673] is bound to a [`CURAND-XORWOW-STATE`][ffae] with
    `N-RANDOM-STATES`, seeded with `RANDOM-SEED`, and allocation of device
    memory is limited to `N-POOL-BYTES` (`NIL` means no limit, see
    [CUDA Memory Management][4e65]).
    
    Else - that is, if CUDA is not available, `BODY` is simply executed.

<a id="x-28MGL-MAT-3ACALL-WITH-CUDA-20FUNCTION-29"></a>
- [function] **CALL-WITH-CUDA** *FN &KEY ((:ENABLED \*CUDA-ENABLED\*) \*CUDA-ENABLED\*) (DEVICE-ID \*CUDA-DEFAULT-DEVICE-ID\*) (RANDOM-SEED \*CUDA-DEFAULT-RANDOM-SEED\*) (N-RANDOM-STATES \*CUDA-DEFAULT-N-RANDOM-STATES\*) N-POOL-BYTES*

    Like [`WITH-CUDA*`][3db3], but takes a no argument function instead of the
    macro's `BODY`.

<a id="x-28MGL-MAT-3A-2ACUDA-ENABLED-2A-20VARIABLE-29"></a>
- [variable] **\*CUDA-ENABLED\*** *T*

    Set or bind this to false to disable all use of cuda. If this is
    done from within `WITH-CUDA`*, then cuda becomes temporarily disabled.
    If this is done from outside `WITH-CUDA`*, then it changes the default
    values of the `ENABLED` argument of any future [`WITH-CUDA*`][3db3]s which
    turns off cuda initialization entirely.

<a id="x-28MGL-MAT-3ACUDA-ENABLED-20-28MGL-PAX-3AACCESSOR-20MGL-MAT-3AMAT-29-29"></a>
- [accessor] **CUDA-ENABLED** *MAT (:CUDA-ENABLED = \*DEFAULT-MAT-CUDA-ENABLED\*)*

    The control provided by [`*CUDA-ENABLED*`][aba0] can be too
    coarse. This flag provides a per-object mechanism to turn cuda
    off. If it is set to `NIL`, then any operation that pays attention
    to this flag will not create or access the [`CUDA-ARRAY`][7a7a] facet.
    Implementationally speaking, this is easily accomplished by using
    [`USE-CUDA-P`][c442].

<a id="x-28MGL-MAT-3A-2ADEFAULT-MAT-CUDA-ENABLED-2A-20VARIABLE-29"></a>
- [variable] **\*DEFAULT-MAT-CUDA-ENABLED\*** *T*

    The default for [`CUDA-ENABLED`][47eb].

<a id="x-28MGL-MAT-3A-2AN-MEMCPY-HOST-TO-DEVICE-2A-20VARIABLE-29"></a>
- [variable] **\*N-MEMCPY-HOST-TO-DEVICE\*** *0*

    Incremented each time a host to device copy is performed. Bound to
    0 by [`WITH-CUDA*`][3db3]. Useful for tracking down performance problems.

<a id="x-28MGL-MAT-3A-2AN-MEMCPY-DEVICE-TO-HOST-2A-20VARIABLE-29"></a>
- [variable] **\*N-MEMCPY-DEVICE-TO-HOST\*** *0*

    Incremented each time a device to host copy is performed. Bound to
    0 by [`WITH-CUDA*`][3db3]. Useful for tracking down performance problems.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-DEVICE-ID-2A-20VARIABLE-29"></a>
- [variable] **\*CUDA-DEFAULT-DEVICE-ID\*** *0*

    The default value of [`WITH-CUDA*`][3db3]'s `:DEVICE-ID` argument.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-RANDOM-SEED-2A-20VARIABLE-29"></a>
- [variable] **\*CUDA-DEFAULT-RANDOM-SEED\*** *1234*

    The default value of [`WITH-CUDA*`][3db3]'s `:RANDOM-SEED` argument.

<a id="x-28MGL-MAT-3A-2ACUDA-DEFAULT-N-RANDOM-STATES-2A-20VARIABLE-29"></a>
- [variable] **\*CUDA-DEFAULT-N-RANDOM-STATES\*** *4096*

    The default value of [`WITH-CUDA*`][3db3]'s `:N-RANDOM-STATES` argument.

<a id="x-28MGL-MAT-3A-40MAT-CUDA-MEMORY-MANAGEMENT-20MGL-PAX-3ASECTION-29"></a>
#### 18.3.1 CUDA Memory Management

The GPU (called *device* in CUDA terminology) has its own memory
and it can only perform computation on data in this *device memory*
so there is some copying involved to and from main memory. Efficient
algorithms often allocate device memory up front and minimize the
amount of copying that has to be done by computing as much as
possible on the GPU.

MGL-MAT reduces the cost of device of memory allocations by
maintaining a cache of currently unused allocations from which it
first tries to satisfy allocation requests. The total size of all
the allocated device memory regions (be they in use or currently
unused but cached) is never more than `N-POOL-BYTES` as specified in
[`WITH-CUDA*`][3db3]. `N-POOL-BYTES` being `NIL` means no limit.

<a id="x-28MGL-MAT-3ACUDA-OUT-OF-MEMORY-20CONDITION-29"></a>
- [condition] **CUDA-OUT-OF-MEMORY** *STORAGE-CONDITION*

    If an allocation request cannot be
    satisfied (either because of `N-POOL-BYTES` or physical device memory
    limits being reached), then `CUDA-OUT-OF-MEMORY` is signalled.

<a id="x-28MGL-MAT-3ACUDA-ROOM-20FUNCTION-29"></a>
- [function] **CUDA-ROOM** *&KEY (STREAM \*STANDARD-OUTPUT\*) (VERBOSE T)*

    When CUDA is in use (see [`USE-CUDA-P`][c442]), print a summary of memory
    usage in the current CUDA context to `STREAM`. If `VERBOSE`, make the
    output human easily readable, else try to present it in a very
    concise way. Sample output with `VERBOSE`:
    
    ```
    CUDA memory usage:
    device arrays: 450 (used bytes: 3,386,295,808, pooled bytes: 1,816,657,920)
    host arrays: 14640 (used bytes: 17,380,147,200)
    host->device copies: 154,102,488, device->host copies: 117,136,434
    ```
    
    The same data presented with `VERBOSE` false:
    
    ```
    d: 450 (3,386,295,808 + 1,816,657,920), h: 14640 (17,380,147,200)
    h->d: 154,102,488, d->h: 117,136,434
    ```


That's it about reducing the cost allocations. The other important
performance consideration, minimizing the amount copying done, is
very hard to do if the data doesn't fit in device memory which is
often a very limited resource. In this case the next best thing is
to do the copying concurrently with computation.

<a id="x-28MGL-MAT-3AWITH-SYNCING-CUDA-FACETS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-SYNCING-CUDA-FACETS** *(MATS-TO-CUDA MATS-TO-CUDA-HOST &KEY (SAFEP '\*SYNCING-CUDA-FACETS-SAFE-P\*)) &BODY BODY*

    Update CUDA facets in a possibly asynchronous way while `BODY`
    executes. Behind the scenes, a separate CUDA stream is used to copy
    between registered host memory and device memory. When
    `WITH-SYNCING-CUDA-FACETS` finishes either by returning normally or by
    a performing a non-local-exit the following are true:
    
    - All [`MAT`][6d14]s in `MATS-TO-CUDA` have an up-to-date
      [`CUDA-ARRAY`][7a7a] facet.
    
    - All `MAT`s in `MATS-TO-CUDA-HOST` have an up-to-date
      [`CUDA-HOST-ARRAY`][0789] facet and no
      [`CUDA-ARRAY`][7a7a].
    
    It is an error if the same matrix appears in both `MATS-TO-CUDA` and
    `MATS-TO-CUDA-HOST`, but the same matrix may appear any number of
    times in one of them.
    
    If `SAFEP` is true, then the all matrices in either of the two lists
    are effectively locked for output until `WITH-SYNCING-CUDA-FACETS`
    finishes. With SAFE `NIL`, unsafe accesses to facets of these matrices
    are not detected, but the whole operation has a bit less overhead.

<a id="x-28MGL-MAT-3A-2ASYNCING-CUDA-FACETS-SAFE-P-2A-20VARIABLE-29"></a>
- [variable] **\*SYNCING-CUDA-FACETS-SAFE-P\*** *T*

    The default value of the `SAFEP` argument of
    [`WITH-SYNCING-CUDA-FACETS`][9fff].

Also note that often the easiest thing to do is to prevent the use
of CUDA (and consequently the creation of [`CUDA-ARRAY`][7a7a]
facets, and allocations). This can be done either by binding
[`*CUDA-ENABLED*`][aba0] to `NIL` or by setting [`CUDA-ENABLED`][47eb] to `NIL` on specific
matrices.

<a id="x-28MGL-MAT-3A-40MAT-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
## 19 Writing Extensions

New operations are usually implemented in lisp, CUDA, or by calling
a foreign function in, for instance, BLAS, CUBLAS, CURAND.

<a id="x-28MGL-MAT-3A-40MAT-LISP-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
### 19.1 Lisp Extensions

<a id="x-28MGL-MAT-3ADEFINE-LISP-KERNEL-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-LISP-KERNEL** *(NAME &KEY (CTYPES '(:FLOAT :DOUBLE))) (&REST PARAMS) &BODY BODY*

    This is very much like [`DEFINE-CUDA-KERNEL`][4074] but for normal lisp code.
    It knows how to deal with [`MAT`][6d14] objects and can define the same
    function for multiple `CTYPES`. Example:
    
    ```commonlisp
    (define-lisp-kernel (lisp-.+!)
        ((alpha single-float) (x :mat :input) (start-x index) (n index))
      (loop for xi of-type index upfrom start-x
              below (the! index (+ start-x n))
            do (incf (aref x xi) alpha)))
    ```
    
    Parameters are either of the form `(<NAME> <LISP-TYPE)`
    or `(<NAME> :MAT <DIRECTION>)`. In the latter case, the appropriate
    CFFI pointer is passed to the kernel. `<DIRECTION>` is passed on to
    the [`WITH-FACET`][0706] that's used to acquire the foreign array. Note that
    the return type is not declared.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `CTYPES` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this means that one needs to write only one
    kernel for [`SINGLE-FLOAT`][ade2] and [`DOUBLE-FLOAT`][a5f2]. All such functions get the
    declaration from [`*DEFAULT-LISP-KERNEL-DECLARATIONS*`][6a5e].
    
    Finally, a dispatcher function with `NAME` is defined which determines
    the ctype of the `MAT` objects passed for `:MAT` typed parameters. It's
    an error if they are not of the same type. Scalars declared
    `SINGLE-FLOAT` are coerced to that type and the appropriate kernel is
    called.

<a id="x-28MGL-MAT-3A-2ADEFAULT-LISP-KERNEL-DECLARATIONS-2A-20VARIABLE-29"></a>
- [variable] **\*DEFAULT-LISP-KERNEL-DECLARATIONS\*** *((OPTIMIZE SPEED (SB-C:INSERT-ARRAY-BOUNDS-CHECKS 0)))*

    These declarations are added automatically to kernel functions.

<a id="x-28MGL-MAT-3A-40MAT-CUDA-EXTENSIONS-20MGL-PAX-3ASECTION-29"></a>
### 19.2 CUDA Extensions

<a id="x-28MGL-MAT-3AUSE-CUDA-P-20FUNCTION-29"></a>
- [function] **USE-CUDA-P** *&REST MATS*

    Return true if cuda is enabled ([`*CUDA-ENABLED*`][aba0]), it's initialized
    and all `MATS` have [`CUDA-ENABLED`][47eb]. Operations of
    matrices use this to decide whether to go for the CUDA
    implementation or BLAS/Lisp. It's provided for implementing new
    operations.

<a id="x-28MGL-MAT-3ACHOOSE-1D-BLOCK-AND-GRID-20FUNCTION-29"></a>
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

<a id="x-28MGL-MAT-3ACHOOSE-2D-BLOCK-AND-GRID-20FUNCTION-29"></a>
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


<a id="x-28MGL-MAT-3ACHOOSE-3D-BLOCK-AND-GRID-20FUNCTION-29"></a>
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


<a id="x-28MGL-MAT-3ADEFINE-CUDA-KERNEL-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-CUDA-KERNEL** *(NAME &KEY (CTYPES '(:FLOAT :DOUBLE))) (RETURN-TYPE PARAMS) &BODY BODY*

    This is an extended `CL-CUDA:DEFKERNEL` macro. It knows how to deal
    with [`MAT`][6d14] objects and can define the same function for multiple
    `CTYPES`. Example:
    
    ```commonlisp
    (define-cuda-kernel (cuda-.+!)
        (void ((alpha float) (x :mat :input) (n int)))
      (let ((stride (* block-dim-x grid-dim-x)))
        (do ((i (+ (* block-dim-x block-idx-x) thread-idx-x)
                (+ i stride)))
            ((>= i n))
          (set (aref x i) (+ (aref x i) alpha)))))
    ```
    
    The signature looks pretty much like in `CL-CUDA:DEFKERNEL`, but
    parameters can take the form of `(<NAME> :MAT <DIRECTION>)` too, in
    which case the appropriate `CL-CUDA.DRIVER-API:CU-DEVICE-PTR` is
    passed to the kernel. `<DIRECTION>` is passed on to the [`WITH-FACET`][0706]
    that's used to acquire the cuda array.
    
    Both the signature and the body are written as if for single floats,
    but one function is defined for each ctype in `CTYPES` by transforming
    types, constants and code by substituting them with their ctype
    equivalents. Currently this means that one needs to write only one
    kernel for `FLOAT` and `DOUBLE`.
    
    Finally, a dispatcher function with `NAME` is defined which determines
    the ctype of the `MAT` objects passed for `:MAT` typed parameters. It's
    an error if they are not of the same type. Scalars declared
    `FLOAT` are coerced to that type and the appropriate
    kernel is called.

<a id="x-28MGL-MAT-3A-40MAT-CUBLAS-20MGL-PAX-3ASECTION-29"></a>
#### 19.2.1 CUBLAS

In a [`WITH-CUDA*`][3db3] [BLAS Operations][4958] will automatically use CUBLAS. No need to
use these at all.

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-20CONDITION-29"></a>
- [condition] **CUBLAS-ERROR** *ERROR*

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-FUNCTION-NAME-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29"></a>
- [reader] **CUBLAS-ERROR-FUNCTION-NAME** *CUBLAS-ERROR (:FUNCTION-NAME)*

<a id="x-28MGL-MAT-3ACUBLAS-ERROR-STATUS-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACUBLAS-ERROR-29-29"></a>
- [reader] **CUBLAS-ERROR-STATUS** *CUBLAS-ERROR (:STATUS)*

<a id="x-28MGL-MAT-3A-2ACUBLAS-HANDLE-2A-20VARIABLE-29"></a>
- [variable] **\*CUBLAS-HANDLE\***

<a id="x-28MGL-MAT-3ACUBLAS-CREATE-20FUNCTION-29"></a>
- [function] **CUBLAS-CREATE** *HANDLE*

<a id="x-28MGL-MAT-3ACUBLAS-DESTROY-20FUNCTION-29"></a>
- [function] **CUBLAS-DESTROY** *&KEY (HANDLE \*CUBLAS-HANDLE\*)*

<a id="x-28MGL-MAT-3AWITH-CUBLAS-HANDLE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-CUBLAS-HANDLE** *NIL &BODY BODY*

<a id="x-28MGL-MAT-3ACUBLAS-GET-VERSION-20FUNCTION-29"></a>
- [function] **CUBLAS-GET-VERSION** *VERSION &KEY (HANDLE \*CUBLAS-HANDLE\*)*

<a id="x-28MGL-MAT-3A-40MAT-CURAND-20MGL-PAX-3ASECTION-29"></a>
#### 19.2.2 CURAND

This the low level CURAND API. You probably want [Random numbers][c669]
instead.

<a id="x-28MGL-MAT-3AWITH-CURAND-STATE-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-CURAND-STATE** *(STATE) &BODY BODY*

<a id="x-28MGL-MAT-3A-2ACURAND-STATE-2A-20VARIABLE-29"></a>
- [variable] **\*CURAND-STATE\***

<a id="x-28MGL-MAT-3ACURAND-XORWOW-STATE-20CLASS-29"></a>
- [class] **CURAND-XORWOW-STATE**

<a id="x-28MGL-MAT-3AN-STATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29"></a>
- [reader] **N-STATES** *CURAND-XORWOW-STATE (:N-STATES)*

<a id="x-28MGL-MAT-3ASTATES-20-28MGL-PAX-3AREADER-20MGL-MAT-3ACURAND-XORWOW-STATE-29-29"></a>
- [reader] **STATES** *CURAND-XORWOW-STATE (:STATES)*
<a id="x-28MGL-CUBE-3A-40CUBE-MANUAL-20MGL-PAX-3ASECTION-29"></a>
# Cube Manual

## Table of Contents

- [1 Links][3912]
- [2 Introduction][43a2]
- [3 Basics][f36f]
- [4 Synchronization][a5a6]
- [5 Facets][0ef1]
- [6 Facet Extension API][1eda]
- [7 The Default Implementation of `CALL-WITH-FACET*`][d720]
- [8 Lifetime][a810]
    - [8.1 Facet Barriers][0fc8]

###### \[in package MGL-CUBE\]
<a id="x-28MGL-CUBE-3A-40CUBE-LINKS-20MGL-PAX-3ASECTION-29"></a>
## 1 Links

Here is the [official
repository](https://github.com/melisgl/mgl-mat) and the [HTML
documentation](http://melisgl.github.io/mgl-mat/cube-manual.html)
for the latest version.

<a id="x-28MGL-CUBE-3A-40CUBE-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>
## 2 Introduction

This is the library on which MGL-MAT (see [MAT Manual][f470])
is built. The idea of automatically translating between various
representations may be useful for other applications, so this got
its own package and all ties to MGL-MAT has been severed.

This package defines [`CUBE`][1659], an abstract base class that provides a
framework for automatic conversion between various representations
of the same data. To define a cube, `CUBE` needs to be subclassed and
the [Facet Extension API][1eda] be implemented.

If you are only interested in how to use cubes in general, read
[Basics][f36f], [Lifetime][a810] and [Facet Barriers][0fc8].

If you want to implement a new cube datatype, then see [Facets][0ef1],
[Facet Extension API][1eda], and [The Default Implementation of `CALL-WITH-FACET*`][d720].

<a id="x-28MGL-CUBE-3A-40CUBE-BASICS-20MGL-PAX-3ASECTION-29"></a>
## 3 Basics

Here we learn what a [`CUBE`][1659] is and how to access the data in it with
[`WITH-FACET`][0706].

<a id="x-28MGL-CUBE-3ACUBE-20CLASS-29"></a>
- [class] **CUBE**

    A datacube that has various representations of the
    same stuff. These representations go by the name \`facet'. Clients
    must use [`WITH-FACET`][0706] to acquire a dynamic extent reference to a
    facet. With the information provided in the `DIRECTION` argument of
    `WITH-FACET`, the cube keeps track of which facets are up-to-date and
    copies data between them as necessary.
    
    The cube is an abstract class, it does not provide useful behavior
    in itself. One must subclass it and implement the
    [Facet Extension API][1eda].
    
    Also see [Lifetime][a810] and [Facet Barriers][0fc8].

<a id="x-28MGL-CUBE-3AWITH-FACET-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-FACET** *(VAR (CUBE FACET-NAME &KEY (DIRECTION :IO) TYPE)) &BODY BODY*

    Find or create the facet with `FACET-NAME` in `CUBE` and bind `VAR` to
    the representation of `CUBE`'s data provided by that facet. This
    representation is called the facet's *value*. The value is to be
    treated as dynamic extent: it is not allowed to keep a reference to
    it. For the description of the `DIRECTION` parameter, see the type
    [`DIRECTION`][69de].
    
    If `TYPE` is specified, then `VAR` is declared to be of that type.

<a id="x-28MGL-CUBE-3ADIRECTION-20TYPE-29"></a>
- [type] **DIRECTION**

    Used by [`WITH-FACET`][0706], `DIRECTION` can be `:INPUT`, `:OUTPUT` or `:IO`.
    
    - `:INPUT` promises that the facet will only be read and never
      written. Other up-to-date facets of the same cube remain
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      [`SELECT-COPY-SOURCE-FOR-FACET*`][7a49]).
    
    - `:OUTPUT` promises that *all* data will be overwritten without
      reading any data. All up-to-date facets become non-up-to-date,
      while this facet is marked as up-to-date. No copying of data takes
      place.
    
    - `:IO` promises nothing about the type of access. All up-to-date
      facets become non-up-to-date, while this facet is marked as
      up-to-date. If the facet in question is not up-to-date then data
      is copied to it from one of the up-to-date facets (see
      `SELECT-COPY-SOURCE-FOR-FACET*`).
    
    Any number of `WITH-FACET`s with direction `:INPUT` may be active at
    the same time, but `:IO` and `:OUTPUT` cannot coexists with another
    `WITH-FACET` regardless of the direction. The exception for this rule
    is that an inner `WITH-FACET` does not conflict with an enclosing
    `WITH-FACET` if they are for the same facet (but inner `WITH-FACET`s
    for another facet or for the same facet from another thread do).
    
    See [`CHECK-NO-WRITERS`][ea8f] and [`CHECK-NO-WATCHERS`][face] called by
    [The Default Implementation of `CALL-WITH-FACET*`][d720].

<a id="x-28MGL-CUBE-3AWITH-FACETS-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-FACETS** *(&REST FACET-BINDING-SPECS) &BODY BODY*

    A shorthand for writing nested [`WITH-FACET`][0706] calls.
    
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
    ```


<a id="x-28MGL-CUBE-3A-40CUBE-SYNCHRONIZATION-20MGL-PAX-3ASECTION-29"></a>
## 4 Synchronization

Cubes keep track of which facets are used, which are up-to-date to
be able to perform automatic translation between facets. [`WITH-FACET`][0706]
and other operations access and make changes to this metadata so
thread safety is a concern. In this section, we detail how to relax
the default thread safety guarantees.

A related concern is async signal safety which arises most often
when C-c'ing or killing a thread or when the extremely nasty
WITH-TIMEOUT macro is used. In a nutshell, changes to cube metadata
are always made with interrupts disabled so things should be async
signal safe.

<a id="x-28MGL-CUBE-3ASYNCHRONIZATION-20-28MGL-PAX-3AACCESSOR-20MGL-CUBE-3ACUBE-29-29"></a>
- [accessor] **SYNCHRONIZATION** *CUBE (:SYNCHRONIZATION = \*DEFAULT-SYNCHRONIZATION\*)*

    By default, setup and teardown of facets by
    [`WITH-FACET`][0706] is performed in a thread safe way. Corrupting internal
    data structures of cubes is not fun, but in the name of
    performance, synchronization can be turned off either dynamically
    or on a per instance basis.
    
    If `T`, then access to cube metadata is always synchronized. If `NIL`,
    then never. If `:MAYBE`, then whether access is synchronized is
    determined by [`*MAYBE-SYNCHRONIZE-CUBE*`][8324] that's true by default.
    
    The default is the value of [`*DEFAULT-SYNCHRONIZATION*`][e2c4]
    that's `:MAYBE` by default.
    
    Note that the body of a `WITH-FACET` is never synchronized with
    anyone, apart from the implicit reader/writer conflict (see
    [`DIRECTION`][69de]).

<a id="x-28MGL-CUBE-3A-2ADEFAULT-SYNCHRONIZATION-2A-20VARIABLE-29"></a>
- [variable] **\*DEFAULT-SYNCHRONIZATION\*** *:MAYBE*

    The default value for [`SYNCHRONIZATION`][7bcf] of new cubes.

<a id="x-28MGL-CUBE-3A-2AMAYBE-SYNCHRONIZE-CUBE-2A-20VARIABLE-29"></a>
- [variable] **\*MAYBE-SYNCHRONIZE-CUBE\*** *T*

    Determines whether access the cube metadata is synchronized for
    cubes with [`SYNCHRONIZATION`][7bcf] `:MAYBE`.

<a id="x-28MGL-CUBE-3A-40CUBE-FACETS-20MGL-PAX-3ASECTION-29"></a>
## 5 Facets

The basic currency for implementing new cube types is the [`FACET`][6406].
Simply using a cube only involves facet names and values, never
facets themselves.

<a id="x-28MGL-CUBE-3AFACETS-20FUNCTION-29"></a>
- [function] **FACETS** *CUBE*

    Return the facets of `CUBE`.

<a id="x-28MGL-CUBE-3AFIND-FACET-20FUNCTION-29"></a>
- [function] **FIND-FACET** *CUBE FACET-NAME*

    Return the facet of `CUBE` for the facet with `FACET-NAME` or `NIL` if no
    such facet exists.

<a id="x-28MGL-CUBE-3AFACET-20CLASS-29"></a>
- [class] **FACET** *STRUCTURE-OBJECT*

    A cube has facets, as we discussed in [Basics][f36f]. Facets holds
    the data in a particular representation, this is called the *value*
    of the facet. A facet holds one such value and some metadata
    pertaining to it: its `FACET-NAME`([`0`][799d] [`1`][e4bc]), whether it's
    up-to-date ([`FACET-UP-TO-DATE-P`][18b2]), etc. `FACET` objects are never seen
    when simply using a cube, they are for implementing the
    [Facet Extension API][1eda].

<a id="x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-NAME**

    A symbol that uniquely identifies the facet within a cube.

<a id="x-28MGL-CUBE-3AFACET-VALUE-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-VALUE**

    This is what's normally exposed by [`WITH-FACET`][0706].

<a id="x-28MGL-CUBE-3AFACET-DESCRIPTION-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-DESCRIPTION**

    Returned by [`MAKE-FACET*`][6afc] as its second value, this is an
    arbitrary object in which additional information can be
    stored.

<a id="x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-UP-TO-DATE-P**

    Whether the cube has changed since this facet has been last
    updated. See [`FACET-UP-TO-DATE-P*`][696b].

<a id="x-28MGL-CUBE-3AFACET-N-WATCHERS-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-N-WATCHERS**

    The number of active [`WITH-FACET`][0706]s. Updated by [`WATCH-FACET`][f791] and
    [`UNWATCH-FACET`][2ce8].

<a id="x-28MGL-CUBE-3AFACET-WATCHER-THREADS-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-WATCHER-THREADS**

    The threads (one for each watcher) that have active
    [`WITH-FACET`][0706]s.

<a id="x-28MGL-CUBE-3AFACET-DIRECTION-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29"></a>
- [structure-accessor] **FACET-DIRECTION**

    The direction of the last [`WITH-FACET`][0706] on this facet.

<a id="x-28MGL-CUBE-3A-40CUBE-FACET-EXTENSION-API-20MGL-PAX-3ASECTION-29"></a>
## 6 Facet Extension API

Many of the generic functions in this section take [`FACET`][6406] arguments.
`FACET` is a structure and is not intended to be subclassed. To be
able to add specialized methods, the name of the
facet ([`FACET-NAME`][e4bc]) is also passed as the
argument right in front of the corresponding facet argument.

In summary, define `EQL`([`0`][8517] [`1`][01e5]) specializers on facet name arguments, and use
[`FACET-DESCRIPTION`][3c7b] to associate arbitrary information with facets.

<a id="x-28MGL-CUBE-3AMAKE-FACET-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **MAKE-FACET\*** *CUBE FACET-NAME*

    Called by [`WITH-FACET`][0706] (or more directly [`WATCH-FACET`][f791])
    when there is no facet with `FACET-NAME`. As the first value, return a
    new object capable of storing `CUBE`'s data in the facet with
    `FACET-NAME`. As the second value, return a facet description which
    will be available as [`FACET-DESCRIPTION`][3c7b]. As the third value, return a
    generalized boolean indicating whether this facet must be explicitly
    destroyed (in which case a finalizer will be added to `CUBE`).

<a id="x-28MGL-CUBE-3ADESTROY-FACET-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **DESTROY-FACET\*** *FACET-NAME FACET*

    Free the resources associated with `FACET` with
    `FACET-NAME`. The cube this facet belongs to is not among the
    parameters because this method can be called from a finalizer on the
    cube (so we can't have a reference to the cube portably) which also
    means that it may run in an unpredictable thread.

<a id="x-28MGL-CUBE-3ACOPY-FACET-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **COPY-FACET\*** *CUBE FROM-FACET-NAME FROM-FACET TO-FACET-NAME TO-FACET*

    Copy the `CUBE`'s data from `FROM-FACET` with
    `FROM-FACET-NAME` to `TO-FACET` with `TO-FACET-NAME`. Called by
    [`WITH-FACET`][0706] (or more directly [`WATCH-FACET`][f791]) when necessary. `FROM-FACET`
    is what [`SELECT-COPY-SOURCE-FOR-FACET*`][7a49] returned.

<a id="x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **CALL-WITH-FACET\*** *CUBE FACET-NAME DIRECTION FN*

    Call `FN` with an up-to-date [`FACET-VALUE`][e2ff] that belongs
    to `FACET-NAME` of `CUBE`. [`WITH-FACET`][0706] is directly implemented in terms
    of this function. See [The Default Implementation of `CALL-WITH-FACET*`][d720] for the gory
    details.
    
    Specializations will most likely want to call the default
    implementation (with [`CALL-NEXT-METHOD`][1097]) but with a lambda that
    transforms `FACET-VALUE` before passing it on to `FN`.

<a id="x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **FACET-UP-TO-DATE-P\*** *CUBE FACET-NAME FACET*

    Check if `FACET` with `FACET-NAME` has been updated
    since the latest change to `CUBE` (that is, since the access to other
    facets with [`DIRECTION`][69de] of `:IO` or `:OUTPUT`). The default method simply
    calls [`FACET-UP-TO-DATE-P`][18b2] on `FACET`.
    
    One reason to specialize this is when some facets actually share
    common storage, so updating one make the other up-to-date as well.

<a id="x-28MGL-CUBE-3ASELECT-COPY-SOURCE-FOR-FACET-2A-20GENERIC-FUNCTION-29"></a>
- [generic-function] **SELECT-COPY-SOURCE-FOR-FACET\*** *CUBE TO-NAME TO-FACET*

    Called when `TO-FACET` with `TO-NAME` is about to be
    updated by copying data from an up-to-date facet. Return the
    facet (or its name) from which data shall be copied. Note that if
    the returned facet is not [`FACET-UP-TO-DATE-P`][18b2]*, then it will be
    updated first and another SELECT-COPY-SOURCE-FOR-FACET* will take
    place, so be careful not to get into endless recursion. The default
    method simply returns the first up-to-date facet.

PAX integration follows, don't worry about it if you don't use PAX,
but you really should (see `MGL-PAX::@PAX-MANUAL`).

<a id="x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ALOCATIVE-29"></a>
- [locative] **FACET-NAME**

    The [`FACET-NAME`][799d] locative is to refer to stuff defined with
    [`DEFINE-FACET-NAME`][0e24].

<a id="x-28MGL-CUBE-3ADEFINE-FACET-NAME-20MGL-PAX-3AMACRO-29"></a>
- [macro] **DEFINE-FACET-NAME** *SYMBOL LAMBDA-LIST &BODY DOCSTRING*

    Just a macro to document that `SYMBOL` refers to a facet name (as in
    the [`FACET-NAME`][799d]). This is totally confusing, so here is
    an example of how MGL-MAT (see [MAT Manual][f470]) documents the
    [`MGL-MAT:BACKING-ARRAY`][1fda] facet:
    
    ```commonlisp
    (define-facet-name backing-array ()
      "The corresponding facet is a one dimensional lisp array.")
    ```
    
    Which makes it possible to refer to this definition (refer as in
    link and `M-.` to) [`MGL-MAT:BACKING-ARRAY`][1fda] facet-name. See
    `MGL-PAX::@PAX-MANUAL` for more.

Also see [The Default Implementation of `CALL-WITH-FACET*`][d720].

<a id="x-28MGL-CUBE-3A-40CUBE-DEFAULT-CALL-WITH-FACET-2A-20MGL-PAX-3ASECTION-29"></a>
## 7 The Default Implementation of `CALL-WITH-FACET*`

<a id="x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20-28METHOD-20NIL-20-28MGL-CUBE-3ACUBE-20T-20T-20T-29-29-29"></a>
- [method] **CALL-WITH-FACET\*** *(CUBE CUBE) FACET-NAME DIRECTION FN*

    The default implementation of `CALL-WITH-FACET*` is defined in terms
    of the [`WATCH-FACET`][f791] and the [`UNWATCH-FACET`][2ce8] generic functions. These
    can be considered part of the [Facet Extension API][1eda].

<a id="x-28MGL-CUBE-3AWATCH-FACET-20GENERIC-FUNCTION-29"></a>
- [generic-function] **WATCH-FACET** *CUBE FACET-NAME DIRECTION*

    This is what the default [`CALL-WITH-FACET*`][f1d8] method,
    in terms of which [`WITH-FACET`][0706] is implemented, calls first. The
    default method takes care of creating facets, copying and tracking
    up-to-dateness.
    
    Calls [`CHECK-NO-WRITERS`][ea8f] (unless [`*LET-INPUT-THROUGH-P*`][6fe2]) and
    [`CHECK-NO-WATCHERS`][face] (unless [`*LET-OUTPUT-THROUGH-P*`][bf71]) depending on
    `DIRECTION` to detect situations with a writer being concurrent to
    readers/writers because that would screw up the tracking of
    up-to-dateness.
    
    The default implementation should suffice most of the time.
    MGL-MAT specializes it to override the `DIRECTION` arg, if
    it's `:OUTPUT` but not all elements are visible due to reshaping, so
    that invisible elements are still copied over.

<a id="x-28MGL-CUBE-3AUNWATCH-FACET-20GENERIC-FUNCTION-29"></a>
- [generic-function] **UNWATCH-FACET** *CUBE FACET-NAME*

    This is what the default [`CALL-WITH-FACET*`][f1d8] method,
    in terms of which [`WITH-FACET`][0706] is implemented, calls last. The default
    method takes care of taking down facets. External resource managers
    may want to hook into this to handle unused facets.

<a id="x-28MGL-CUBE-3A-2ALET-INPUT-THROUGH-P-2A-20VARIABLE-29"></a>
- [variable] **\*LET-INPUT-THROUGH-P\*** *NIL*

    If true, `WITH-FACETS`([`0`][ddd4] [`1`][0706]) (more precisely, the default implementation of
    [`CALL-WITH-FACET*`][f1d8]) with `:DIRECTION` `:INPUT` does not call
    [`CHECK-NO-WRITERS`][ea8f]. This knob is intended to be bound locally for
    debugging purposes.

<a id="x-28MGL-CUBE-3A-2ALET-OUTPUT-THROUGH-P-2A-20VARIABLE-29"></a>
- [variable] **\*LET-OUTPUT-THROUGH-P\*** *NIL*

    If true, `WITH-FACETS`([`0`][ddd4] [`1`][0706]) (more precisely, the default implementation of
    [`CALL-WITH-FACET*`][f1d8]) with `:DIRECTION` `:IO` or `:OUTPUT` does not call
    [`CHECK-NO-WATCHERS`][face]. This knob is intended to be bound locally for
    debugging purposes.

<a id="x-28MGL-CUBE-3ACHECK-NO-WRITERS-20FUNCTION-29"></a>
- [function] **CHECK-NO-WRITERS** *CUBE FACET-NAME MESSAGE-FORMAT &REST MESSAGE-ARGS*

    Signal an error if `CUBE` has facets (with names other than
    `FACET-NAME`) being written (i.e. direction is `:IO` or `:OUTPUT`).

<a id="x-28MGL-CUBE-3ACHECK-NO-WATCHERS-20FUNCTION-29"></a>
- [function] **CHECK-NO-WATCHERS** *CUBE FACET-NAME MESSAGE-FORMAT &REST MESSAGE-ARGS*

    Signal an error if `CUBE` has facets (with names other than
    `FACET-NAME`) being regardless of the direction.

<a id="x-28MGL-CUBE-3A-40CUBE-LIFETIME-20MGL-PAX-3ASECTION-29"></a>
## 8 Lifetime

Lifetime management of facets is manual (but facets of garbage
cubes are freed automatically by a finalizer, see [`MAKE-FACET*`][6afc]). One
may destroy a single facet or all facets of a cube with
[`DESTROY-FACET`][39a0] and [`DESTROY-CUBE`][dbac], respectively. Also see
[Facet Barriers][0fc8].

<a id="x-28MGL-CUBE-3ADESTROY-FACET-20FUNCTION-29"></a>
- [function] **DESTROY-FACET** *CUBE FACET-NAME*

    Free resources associated with the facet with `FACET-NAME` and remove
    it from `FACETS`([`0`][6406] [`1`][b1f9]) of `CUBE`.

<a id="x-28MGL-CUBE-3ADESTROY-CUBE-20FUNCTION-29"></a>
- [function] **DESTROY-CUBE** *CUBE*

    Destroy all facets of `CUBE` with [`DESTROY-FACET`][39a0].

In some cases it is useful to declare the intent to use a facet in
the future to prevent its destruction. Hence, every facet has
reference count which starts from 0. The reference count is
incremented and decremented by [`ADD-FACET-REFERENCE-BY-NAME`][db31] and
[`REMOVE-FACET-REFERENCE-BY-NAME`][b78f], respectively. If it is positive,
then the facet will not be destroyed by explicit [`DESTROY-FACET`][39a0] and
[`DESTROY-CUBE`][dbac] calls, but it will still be destroyed by the finalizer
to prevent resource leaks caused by stray references.

<a id="x-28MGL-CUBE-3AADD-FACET-REFERENCE-BY-NAME-20FUNCTION-29"></a>
- [function] **ADD-FACET-REFERENCE-BY-NAME** *CUBE FACET-NAME*

    Make sure `FACET-NAME` exists on `CUBE` and increment its reference
    count. Return the [`FACET`][6406] behind `FACET-NAME`.

<a id="x-28MGL-CUBE-3AREMOVE-FACET-REFERENCE-BY-NAME-20FUNCTION-29"></a>
- [function] **REMOVE-FACET-REFERENCE-BY-NAME** *CUBE FACET-NAME*

    Decrement the reference count of the facet with `FACET-NAME` of `CUBE`.
    It is an error if the facet does not exists or if the reference
    count becomes negative.

<a id="x-28MGL-CUBE-3AREMOVE-FACET-REFERENCE-20FUNCTION-29"></a>
- [function] **REMOVE-FACET-REFERENCE** *FACET*

    Decrement the reference count of `FACET`. It is an error if the facet
    is already destroyed or if the reference count becomes negative.
    This function has the same purpose as
    [`REMOVE-FACET-REFERENCE-BY-NAME`][b78f], but by having a single `FACET`
    argument, it's more suited for use in finalizers because it does not
    keep the whole [`CUBE`][1659] alive.

<a id="x-28MGL-CUBE-3A-40CUBE-FACET-BARRIER-20MGL-PAX-3ASECTION-29"></a>
### 8.1 Facet Barriers

A facility to control lifetime of facets tied to a dynamic extent.
Also see [Lifetime][a810].

<a id="x-28MGL-CUBE-3AWITH-FACET-BARRIER-20MGL-PAX-3AMACRO-29"></a>
- [macro] **WITH-FACET-BARRIER** *(CUBE-TYPE ENSURES DESTROYS) &BODY BODY*

    When `BODY` exits, destroy facets which:
    
    - are of cubes with `CUBE-TYPE`
    
    - have a facet name among `DESTROYS`
    
    - were created in the dynamic extent of `BODY`
    
    Before destroying the facets, it is ensured that facets with names
    among `ENSURES` are up-to-date. `WITH-FACET-BARRIER`s can be nested, in
    case of multiple barriers matching the cube's type and the created
    facet's name, the innermost one takes precedence.
    
    The purpose of this macro is twofold. First, it makes it easy to
    temporarily work with a certain facet of many cubes without leaving
    newly created facets around. Second, it can be used to make sure
    that facets whose extent is tied to some dynamic boundary (such as
    the thread in which they were created) are destroyed.

<a id="x-28MGL-CUBE-3ACOUNT-BARRED-FACETS-20FUNCTION-29"></a>
- [function] **COUNT-BARRED-FACETS** *FACET-NAME &KEY (TYPE 'CUBE)*

    Count facets with `FACET-NAME` of cubes of `TYPE` which will be
    destroyed by a facet barrier.

  [010e]: #x-28MGL-MAT-3ACUDA-ROOM-20FUNCTION-29 "MGL-MAT:CUDA-ROOM FUNCTION"
  [01e5]: http://www.lispworks.com/documentation/HyperSpec/Body/t_eql.htm "EQL TYPE"
  [046b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dim.htm "ARRAY-DIMENSION FUNCTION"
  [0706]: #x-28MGL-CUBE-3AWITH-FACET-20MGL-PAX-3AMACRO-29 "MGL-CUBE:WITH-FACET MGL-PAX:MACRO"
  [0789]: #x-28MGL-MAT-3ACUDA-HOST-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "MGL-MAT:CUDA-HOST-ARRAY MGL-CUBE:FACET-NAME"
  [086b]: #x-28MGL-MAT-3A-40MAT-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [0a31]: http://www.lispworks.com/documentation/HyperSpec/Body/f_row_ma.htm "ROW-MAJOR-AREF FUNCTION"
  [0b2b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm "MAKE-ARRAY FUNCTION"
  [0e24]: #x-28MGL-CUBE-3ADEFINE-FACET-NAME-20MGL-PAX-3AMACRO-29 "MGL-CUBE:DEFINE-FACET-NAME MGL-PAX:MACRO"
  [0ecb]: #x-28MGL-MAT-3AREPLACE-21-20FUNCTION-29 "MGL-MAT:REPLACE! FUNCTION"
  [0ef1]: #x-28MGL-CUBE-3A-40CUBE-FACETS-20MGL-PAX-3ASECTION-29 "Facets"
  [0fc8]: #x-28MGL-CUBE-3A-40CUBE-FACET-BARRIER-20MGL-PAX-3ASECTION-29 "Facet Barriers"
  [1097]: http://www.lispworks.com/documentation/HyperSpec/Body/f_call_n.htm "CALL-NEXT-METHOD FUNCTION"
  [1354]: #x-28MGL-MAT-3ACTYPE-20TYPE-29 "MGL-MAT:CTYPE TYPE"
  [1600]: #x-28MGL-MAT-3A-40MAT-MAPPINGS-20MGL-PAX-3ASECTION-29 "Mappings"
  [1659]: #x-28MGL-CUBE-3ACUBE-20CLASS-29 "MGL-CUBE:CUBE CLASS"
  [17b7]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf.htm "SETF MGL-PAX:MACRO"
  [18b2]: #x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "MGL-CUBE:FACET-UP-TO-DATE-P MGL-PAX:STRUCTURE-ACCESSOR"
  [193b]: #x-28MGL-MAT-3A-2AFOREIGN-ARRAY-STRATEGY-2A-20-28VARIABLE-20-22-see-20below--22-29-29 'MGL-MAT:*FOREIGN-ARRAY-STRATEGY* (VARIABLE "-see below-")'
  [1b99]: #x-28MGL-MAT-3A-40MAT-TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [1eda]: #x-28MGL-CUBE-3A-40CUBE-FACET-EXTENSION-API-20MGL-PAX-3ASECTION-29 "Facet Extension API"
  [1f37]: #x-28MGL-MAT-3A-2APRINT-MAT-2A-20VARIABLE-29 "MGL-MAT:*PRINT-MAT* VARIABLE"
  [1fda]: #x-28MGL-MAT-3ABACKING-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "MGL-MAT:BACKING-ARRAY MGL-CUBE:FACET-NAME"
  [2098]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "SINH FUNCTION"
  [2cbb]: #x-28MGL-MAT-3A-40MAT-SHAPING-COMPARISON-TO-LISP-20MGL-PAX-3ASECTION-29 "Comparison to Lisp Arrays"
  [2ce8]: #x-28MGL-CUBE-3AUNWATCH-FACET-20GENERIC-FUNCTION-29 "MGL-CUBE:UNWATCH-FACET GENERIC-FUNCTION"
  [2ea4]: #x-28MGL-MAT-3A-40MAT-WHAT-KIND-OF-MATRICES-20MGL-PAX-3ASECTION-29 "What kind of matrices are supported?"
  [3046]: #x-28MGL-MAT-3A-2ADEFAULT-MAT-CTYPE-2A-20VARIABLE-29 "MGL-MAT:*DEFAULT-MAT-CTYPE* VARIABLE"
  [3164]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "COS FUNCTION"
  [3573]: #x-28MGL-MAT-3AMAT-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "MGL-MAT:MAT-SIZE (MGL-PAX:READER MGL-MAT:MAT)"
  [36b5]: #x-28MGL-MAT-3A-40MAT-FOREIGN-20MGL-PAX-3ASECTION-29 "Foreign arrays"
  [3912]: #x-28MGL-CUBE-3A-40CUBE-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [3993]: #x-28MGL-MAT-3A-40MAT-CURAND-20MGL-PAX-3ASECTION-29 "CURAND"
  [39a0]: #x-28MGL-CUBE-3ADESTROY-FACET-20FUNCTION-29 "MGL-CUBE:DESTROY-FACET FUNCTION"
  [3c7b]: #x-28MGL-CUBE-3AFACET-DESCRIPTION-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "MGL-CUBE:FACET-DESCRIPTION MGL-PAX:STRUCTURE-ACCESSOR"
  [3db3]: #x-28MGL-MAT-3AWITH-CUDA-2A-20MGL-PAX-3AMACRO-29 "MGL-MAT:WITH-CUDA* MGL-PAX:MACRO"
  [3e36]: #x-28MGL-MAT-3A-40MAT-NON-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29 "Non-destructive API"
  [3f21]: #x-28MGL-MAT-3ASCAL-21-20FUNCTION-29 "MGL-MAT:SCAL! FUNCTION"
  [4074]: #x-28MGL-MAT-3ADEFINE-CUDA-KERNEL-20MGL-PAX-3AMACRO-29 "MGL-MAT:DEFINE-CUDA-KERNEL MGL-PAX:MACRO"
  [420d]: #x-28MGL-MAT-3AREAD-MAT-20GENERIC-FUNCTION-29 "MGL-MAT:READ-MAT GENERIC-FUNCTION"
  [43a2]: #x-28MGL-CUBE-3A-40CUBE-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [43e9]: #x-28MGL-MAT-3A-40MAT-PRINTING-20MGL-PAX-3ASECTION-29 "Printing"
  [4495]: #x-28MGL-MAT-3AFOREIGN-ARRAY-STRATEGY-20TYPE-29 "MGL-MAT:FOREIGN-ARRAY-STRATEGY TYPE"
  [45be]: http://www.lispworks.com/documentation/HyperSpec/Body/f_exp_e.htm "EXP FUNCTION"
  [46e6]: #x-28MGL-MAT-3A-40MAT-ASSEMBLING-20MGL-PAX-3ASECTION-29 "Assembling"
  [47eb]: #x-28MGL-MAT-3ACUDA-ENABLED-20-28MGL-PAX-3AACCESSOR-20MGL-MAT-3AMAT-29-29 "MGL-MAT:CUDA-ENABLED (MGL-PAX:ACCESSOR MGL-MAT:MAT)"
  [4815]: #x-28MGL-MAT-3AMAT-DIMENSION-20FUNCTION-29 "MGL-MAT:MAT-DIMENSION FUNCTION"
  [4958]: #x-28MGL-MAT-3A-40MAT-BLAS-20MGL-PAX-3ASECTION-29 "BLAS Operations"
  [4e65]: #x-28MGL-MAT-3A-40MAT-CUDA-MEMORY-MANAGEMENT-20MGL-PAX-3ASECTION-29 "CUDA Memory Management"
  [4f59]: #x-28MGL-MAT-3AFOREIGN-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "MGL-MAT:FOREIGN-ARRAY MGL-CUBE:FACET-NAME"
  [510c]: #x-28MGL-MAT-3AFILL-21-20FUNCTION-29 "MGL-MAT:FILL! FUNCTION"
  [5212]: #x-28MGL-MAT-3A-40MAT-CUDA-20MGL-PAX-3ASECTION-29 "CUDA"
  [545d]: #x-28MGL-MAT-3A-40MAT-DESTRUCTIVE-API-20MGL-PAX-3ASECTION-29 "Destructive API"
  [54ea]: #x-28MGL-MAT-3A-40MAT-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [5ce7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "SIN FUNCTION"
  [62f5]: #x-28MGL-MAT-3AMAT-CTYPE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "MGL-MAT:MAT-CTYPE (MGL-PAX:READER MGL-MAT:MAT)"
  [6406]: #x-28MGL-CUBE-3AFACET-20CLASS-29 "MGL-CUBE:FACET CLASS"
  [657e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sin_c.htm "TAN FUNCTION"
  [696b]: #x-28MGL-CUBE-3AFACET-UP-TO-DATE-P-2A-20GENERIC-FUNCTION-29 "MGL-CUBE:FACET-UP-TO-DATE-P* GENERIC-FUNCTION"
  [69de]: #x-28MGL-CUBE-3ADIRECTION-20TYPE-29 "MGL-CUBE:DIRECTION TYPE"
  [6a5e]: #x-28MGL-MAT-3A-2ADEFAULT-LISP-KERNEL-DECLARATIONS-2A-20VARIABLE-29 "MGL-MAT:*DEFAULT-LISP-KERNEL-DECLARATIONS* VARIABLE"
  [6afc]: #x-28MGL-CUBE-3AMAKE-FACET-2A-20GENERIC-FUNCTION-29 "MGL-CUBE:MAKE-FACET* GENERIC-FUNCTION"
  [6d14]: #x-28MGL-MAT-3AMAT-20CLASS-29 "MGL-MAT:MAT CLASS"
  [6ead]: #x-28MGL-MAT-3ACOERCE-TO-CTYPE-20FUNCTION-29 "MGL-MAT:COERCE-TO-CTYPE FUNCTION"
  [6fe2]: #x-28MGL-CUBE-3A-2ALET-INPUT-THROUGH-P-2A-20VARIABLE-29 "MGL-CUBE:*LET-INPUT-THROUGH-P* VARIABLE"
  [713f]: #x-28MGL-CUBE-3A-40CUBE-MANUAL-20MGL-PAX-3ASECTION-29 "Cube Manual"
  [7332]: #x-28MGL-MAT-3A-2AN-MEMCPY-DEVICE-TO-HOST-2A-20VARIABLE-29 "MGL-MAT:*N-MEMCPY-DEVICE-TO-HOST* VARIABLE"
  [73f6]: #x-28MGL-MAT-3AMAT-DIMENSIONS-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "MGL-MAT:MAT-DIMENSIONS (MGL-PAX:READER MGL-MAT:MAT)"
  [7408]: #x-28MGL-MAT-3AAXPY-21-20FUNCTION-29 "MGL-MAT:AXPY! FUNCTION"
  [7582]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "TANH FUNCTION"
  [75d3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_aref.htm "AREF FUNCTION"
  [7669]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_dis.htm "ARRAY-DISPLACEMENT FUNCTION"
  [799d]: #x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ALOCATIVE-29 "MGL-CUBE:FACET-NAME MGL-PAX:LOCATIVE"
  [7a49]: #x-28MGL-CUBE-3ASELECT-COPY-SOURCE-FOR-FACET-2A-20GENERIC-FUNCTION-29 "MGL-CUBE:SELECT-COPY-SOURCE-FOR-FACET* GENERIC-FUNCTION"
  [7a7a]: #x-28MGL-MAT-3ACUDA-ARRAY-20MGL-CUBE-3AFACET-NAME-29 "MGL-MAT:CUDA-ARRAY MGL-CUBE:FACET-NAME"
  [7bcf]: #x-28MGL-CUBE-3ASYNCHRONIZATION-20-28MGL-PAX-3AACCESSOR-20MGL-CUBE-3ACUBE-29-29 "MGL-CUBE:SYNCHRONIZATION (MGL-PAX:ACCESSOR MGL-CUBE:CUBE)"
  [7bf8]: #x-28MGL-MAT-3A-40MAT-SHAPING-20MGL-PAX-3ASECTION-29 "Shaping"
  [7e60]: #x-28MGL-MAT-3A-40MAT-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [7ecb]: #x-28MGL-MAT-3A-40MAT-FACETS-20MGL-PAX-3ASECTION-29 "Facets"
  [7feb]: #x-28MGL-MAT-3A-40MAT-IO-20MGL-PAX-3ASECTION-29 "I/O"
  [8100]: #x-28MGL-MAT-3AADJUST-21-20FUNCTION-29 "MGL-MAT:ADJUST! FUNCTION"
  [8324]: #x-28MGL-CUBE-3A-2AMAYBE-SYNCHRONIZE-CUBE-2A-20VARIABLE-29 "MGL-CUBE:*MAYBE-SYNCHRONIZE-CUBE* VARIABLE"
  [8517]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL FUNCTION"
  [8673]: #x-28MGL-MAT-3A-2ACURAND-STATE-2A-20VARIABLE-29 "MGL-MAT:*CURAND-STATE* VARIABLE"
  [87ca]: http://www.lispworks.com/documentation/HyperSpec/Body/f_elt.htm "ELT FUNCTION"
  [896c]: #x-28MGL-MAT-3ARESHAPE-AND-DISPLACE-21-20FUNCTION-29 "MGL-MAT:RESHAPE-AND-DISPLACE! FUNCTION"
  [89bf]: http://www.lispworks.com/documentation/HyperSpec/Body/f_realpa.htm "REALPART FUNCTION"
  [8eda]: #x-28MGL-MAT-3A-40MAT-FACET-API-20MGL-PAX-3ASECTION-29 "Facet API"
  [8f0e]: #x-28MGL-MAT-3A-40MAT-EXTENSIONS-20MGL-PAX-3ASECTION-29 "Writing Extensions"
  [93e1]: #x-28MGL-MAT-3A-40MAT-CUDA-EXTENSIONS-20MGL-PAX-3ASECTION-29 "CUDA Extensions"
  [9743]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm "MAKE-INSTANCE FUNCTION"
  [9d0b]: #x-28MGL-MAT-3A-2AN-MEMCPY-HOST-TO-DEVICE-2A-20VARIABLE-29 "MGL-MAT:*N-MEMCPY-HOST-TO-DEVICE* VARIABLE"
  [9fff]: #x-28MGL-MAT-3AWITH-SYNCING-CUDA-FACETS-20MGL-PAX-3AMACRO-29 "MGL-MAT:WITH-SYNCING-CUDA-FACETS MGL-PAX:MACRO"
  [a1d4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ FUNCTION"
  [a5a6]: #x-28MGL-CUBE-3A-40CUBE-SYNCHRONIZATION-20MGL-PAX-3ASECTION-29 "Synchronization"
  [a5f2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "DOUBLE-FLOAT TYPE"
  [a810]: #x-28MGL-CUBE-3A-40CUBE-LIFETIME-20MGL-PAX-3ASECTION-29 "Lifetime"
  [a9da]: #x-28MGL-MAT-3A-2AMAT-HEADERS-2A-20VARIABLE-29 "MGL-MAT:*MAT-HEADERS* VARIABLE"
  [aa5b]: #x-28MGL-MAT-3A-40MAT-SHAPING-FUNCTIONAL-20MGL-PAX-3ASECTION-29 "Functional Shaping"
  [aa72]: #x-28MGL-MAT-3AWITH-THREAD-CACHED-MAT-20MGL-PAX-3AMACRO-29 "MGL-MAT:WITH-THREAD-CACHED-MAT MGL-PAX:MACRO"
  [aba0]: #x-28MGL-MAT-3A-2ACUDA-ENABLED-2A-20VARIABLE-29 "MGL-MAT:*CUDA-ENABLED* VARIABLE"
  [ac8d]: #x-28MGL-MAT-3A-40MAT-DEBUGGING-20MGL-PAX-3ASECTION-29 "Debugging"
  [ade2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "SINGLE-FLOAT TYPE"
  [b116]: #x-28MGL-MAT-3A-40MAT-CUBLAS-20MGL-PAX-3ASECTION-29 "CUBLAS"
  [b1f9]: #x-28MGL-CUBE-3AFACETS-20FUNCTION-29 "MGL-CUBE:FACETS FUNCTION"
  [b6a7]: #x-28MGL-MAT-3A-40MAT-SHAPING-DESTRUCTIVE-20MGL-PAX-3ASECTION-29 "Destructive Shaping"
  [b78f]: #x-28MGL-CUBE-3AREMOVE-FACET-REFERENCE-BY-NAME-20FUNCTION-29 "MGL-CUBE:REMOVE-FACET-REFERENCE-BY-NAME FUNCTION"
  [b956]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_d_1.htm "ARRAY-DIMENSIONS FUNCTION"
  [ba46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_row.htm "ARRAY-ROW-MAJOR-INDEX FUNCTION"
  [bf71]: #x-28MGL-CUBE-3A-2ALET-OUTPUT-THROUGH-P-2A-20VARIABLE-29 "MGL-CUBE:*LET-OUTPUT-THROUGH-P* VARIABLE"
  [c442]: #x-28MGL-MAT-3AUSE-CUDA-P-20FUNCTION-29 "MGL-MAT:USE-CUDA-P FUNCTION"
  [c44d]: #x-28MGL-CUBE-3AWITH-FACET-BARRIER-20MGL-PAX-3AMACRO-29 "MGL-CUBE:WITH-FACET-BARRIER MGL-PAX:MACRO"
  [c4f8]: #x-28MGL-MAT-3AMAT-MAX-SIZE-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "MGL-MAT:MAT-MAX-SIZE (MGL-PAX:READER MGL-MAT:MAT)"
  [c669]: #x-28MGL-MAT-3A-40MAT-RANDOM-20MGL-PAX-3ASECTION-29 "Random numbers"
  [c96c]: #x-28MGL-MAT-3A-40MAT-INSTALLATION-20MGL-PAX-3ASECTION-29 "Where to Get it?"
  [caca]: #x-28-22mgl-mat-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-mat" ASDF/SYSTEM:SYSTEM'
  [cb94]: #x-28ARRAY-20MGL-CUBE-3AFACET-NAME-29 "ARRAY MGL-CUBE:FACET-NAME"
  [cd9e]: #x-28MGL-MAT-3ASTACK-21-20FUNCTION-29 "MGL-MAT:STACK! FUNCTION"
  [cdc3]: #x-28MGL-MAT-3AFOREIGN-ARRAY-20CLASS-29 "MGL-MAT:FOREIGN-ARRAY CLASS"
  [ce52]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm "MAP-INTO FUNCTION"
  [d3b8]: #x-28MGL-MAT-3A-40MAT-LISP-EXTENSIONS-20MGL-PAX-3ASECTION-29 "Lisp Extensions"
  [d5f8]: #x-28MGL-MAT-3AFOREIGN-ROOM-20FUNCTION-29 "MGL-MAT:FOREIGN-ROOM FUNCTION"
  [d720]: #x-28MGL-CUBE-3A-40CUBE-DEFAULT-CALL-WITH-FACET-2A-20MGL-PAX-3ASECTION-29 "The Default Implementation of `CALL-WITH-FACET*`"
  [db31]: #x-28MGL-CUBE-3AADD-FACET-REFERENCE-BY-NAME-20FUNCTION-29 "MGL-CUBE:ADD-FACET-REFERENCE-BY-NAME FUNCTION"
  [dbac]: #x-28MGL-CUBE-3ADESTROY-CUBE-20FUNCTION-29 "MGL-CUBE:DESTROY-CUBE FUNCTION"
  [ddd4]: #x-28MGL-CUBE-3AWITH-FACETS-20MGL-PAX-3AMACRO-29 "MGL-CUBE:WITH-FACETS MGL-PAX:MACRO"
  [e121]: http://www.lispworks.com/documentation/HyperSpec/Body/t_array.htm "ARRAY TYPE"
  [e17e]: #x-28MGL-MAT-3AMAX-POOL-21-20FUNCTION-29 "MGL-MAT:MAX-POOL! FUNCTION"
  [e1aa]: #x-28MGL-MAT-3ALOGDET-20FUNCTION-29 "MGL-MAT:LOGDET FUNCTION"
  [e2c4]: #x-28MGL-CUBE-3A-2ADEFAULT-SYNCHRONIZATION-2A-20VARIABLE-29 "MGL-CUBE:*DEFAULT-SYNCHRONIZATION* VARIABLE"
  [e2ff]: #x-28MGL-CUBE-3AFACET-VALUE-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "MGL-CUBE:FACET-VALUE MGL-PAX:STRUCTURE-ACCESSOR"
  [e3c3]: #x-28MGL-MAT-3AMAKE-MAT-20FUNCTION-29 "MGL-MAT:MAKE-MAT FUNCTION"
  [e4bc]: #x-28MGL-CUBE-3AFACET-NAME-20MGL-PAX-3ASTRUCTURE-ACCESSOR-29 "MGL-CUBE:FACET-NAME MGL-PAX:STRUCTURE-ACCESSOR"
  [e678]: #x-28MGL-MAT-3A-40MAT-CACHING-20MGL-PAX-3ASECTION-29 "Caching"
  [e69b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_ar_tot.htm "ARRAY-TOTAL-SIZE FUNCTION"
  [e798]: #x-28MGL-MAT-3A-40MAT-CTYPES-20MGL-PAX-3ASECTION-29 "Element types"
  [e95b]: #x-28MGL-MAT-3AGEMM-21-20FUNCTION-29 "MGL-MAT:GEMM! FUNCTION"
  [ea8f]: #x-28MGL-CUBE-3ACHECK-NO-WRITERS-20FUNCTION-29 "MGL-CUBE:CHECK-NO-WRITERS FUNCTION"
  [eb95]: #x-28MGL-MAT-3AMREF-20FUNCTION-29 "MGL-MAT:MREF FUNCTION"
  [f0db]: #x-28MGL-MAT-3AWRITE-MAT-20GENERIC-FUNCTION-29 "MGL-MAT:WRITE-MAT GENERIC-FUNCTION"
  [f1d8]: #x-28MGL-CUBE-3ACALL-WITH-FACET-2A-20GENERIC-FUNCTION-29 "MGL-CUBE:CALL-WITH-FACET* GENERIC-FUNCTION"
  [f36f]: #x-28MGL-CUBE-3A-40CUBE-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [f43d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_sinh_.htm "COSH FUNCTION"
  [f470]: #x-28MGL-MAT-3A-40MAT-MANUAL-20MGL-PAX-3ASECTION-29 "MAT Manual"
  [f5be]: #x-28MGL-MAT-3AMAT-DISPLACEMENT-20-28MGL-PAX-3AREADER-20MGL-MAT-3AMAT-29-29 "MGL-MAT:MAT-DISPLACEMENT (MGL-PAX:READER MGL-MAT:MAT)"
  [f791]: #x-28MGL-CUBE-3AWATCH-FACET-20GENERIC-FUNCTION-29 "MGL-CUBE:WATCH-FACET GENERIC-FUNCTION"
  [face]: #x-28MGL-CUBE-3ACHECK-NO-WATCHERS-20FUNCTION-29 "MGL-CUBE:CHECK-NO-WATCHERS FUNCTION"
  [fae1]: #x-28MGL-MAT-3APINNING-SUPPORTED-P-20FUNCTION-29 "MGL-MAT:PINNING-SUPPORTED-P FUNCTION"
  [fc7d]: #x-28MGL-MAT-3A-2ASUPPORTED-CTYPES-2A-20VARIABLE-29 "MGL-MAT:*SUPPORTED-CTYPES* VARIABLE"
  [ff55]: #x-28MGL-MAT-3A-40MAT-WHAT-IS-IT-20MGL-PAX-3ASECTION-29 "What's MGL-MAT?"
  [ffae]: #x-28MGL-MAT-3ACURAND-XORWOW-STATE-20CLASS-29 "MGL-MAT:CURAND-XORWOW-STATE CLASS"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
