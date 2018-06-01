![build status](https://travis-ci.com/akai7000/clmx.svg?branch=master)

# CLMX 0.0.32

*Matrix manipulation library for Common Lisp*

_Works on ABCL, Allegro CL, Clozure CL, SBCL_

### How to install
_(this is not working yet; I'm yet to submit it to quicklisp)_

```lisp
(ql:quickload :clmx)
```


### Quick Start

Switch to "clmx-matrix" package:
```lisp
CL-USER> (in-package :mx)
#<PACKAGE "CLMX-MATRIX">
MX>
```

To create a matrix you can pass a LIST as _contents_ argument to `create-matrix` function:
```lisp
MX> (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9)))
#<CLMX::MATRIX SIZE: (3 3)>
| 1  2  3 |
| 4  5  6 |
| 7  8  9 |
```

Let's create two square matrices and store them in variables:
```lisp
MX> (defparameter m1 (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4))))
M1

MX> (defparameter m2 (create-matrix :contents '((1 5 4) (-3 -4 2) (1 1 0))))
M2

MX> m1
#<CLMX::MATRIX SIZE: (3 3)>
| 3  -2  0 |
| 7   3  1 |
| 0   2  4 |
MX> m2

#<CLMX::MATRIX SIZE: (3 3)>
|  1   5  4 |
| -3  -4  2 |
|  1   1  0 |
```

To reference a specific element of the matrix use `ref` function. Note that the count starts at 1, not at 0.
```lisp
MX> (ref m1 1 2)
-2
```

Let's add the matrices m1 and m2:
```lisp
MX> (add-matrices m1 m2)
#<CLMX::MATRIX SIZE: (3 3)>
| 4   3  4 |
| 4  -1  3 |
| 1   3  4 |
```

Let's multiply the matrices:
```lisp
MX> (multiply-matrices m1 m2)
#<CLMX::MATRIX SIZE: (3 3)>
|  9  23   8 |
| -1  24  34 |
| -2  -4   4 |
```

To find the number of rows of a matrix:
```lisp
MX> (rows m1)
3
```

Similarly, the number of columns of a matrix:
```lisp
MX> (cols m1)
3
```

Now, let's add a scalar to a matrix:
```lisp
MX> (add-scalar m1 10)
#<MATRIX SIZE: (3 3)>
| 13   8  10 |
| 17  13  11 |
| 10  12  14 |
```

We can also multiply by a scalar:
```lisp
MX> (multiply-scalar m1 10)
#<MATRIX SIZE: (3 3)>
| 30  -20   0 |
| 70   30  10 |
|  0   20  40 |
```

To find a transpose of a matrix you can use `transpose` function:
```lisp
MX> (transpose m1)
#<MATRIX SIZE: (3 3)>
|  3  7  0 |
| -2  3  2 |
|  0  1  4 |
MX> 
```

Let's find the inverse of a matrix:
```lisp
MX> (inverse m1)
[Not implemented yet]
```

We can also find a determinant of a square matrix:
```lisp
MX> (det m1)
86
```

### More on create-matrix

Besides the LIST the `create-matrix` function also takes a SIMPLE-ARRAY in _contents_ argument:
```lisp
MX> (create-matrix :contents #2A ((1 2 3) (4 5 6)))
#<CLMX::MATRIX SIZE: (2 3)>
| 1  2  3 |
| 4  5  6 |
```

Another way to create matrices is by specifying a _dimensions_ and _initial-element_ arguments to `create-matrix` function:
```lisp
MX> (create-matrix :dimensions '(3 5) :initial-element 7)
#<CLMX::MATRIX SIZE: (3 5)>
| 7  7  7  7  7 |
| 7  7  7  7  7 |
| 7  7  7  7  7 |
```

### Special matrices
To create a matrix filled with zeros:
```lisp
MX> (zero-matrix 3 4)
#<CLMX::MATRIX SIZE: (3 4)>
| 0  0  0  0 |
| 0  0  0  0 |
| 0  0  0  0 |
```

Matrix filled with ones:
```lisp
MX> (unit-matrix 3 4)
#<CLMX::MATRIX SIZE: (3 4)>
| 1  1  1  1 |
| 1  1  1  1 |
| 1  1  1  1 |
```

Identity matrix can be created like this:
```lisp
MX> (identity-matrix 5)
#<CLMX::MATRIX SIZE: (5 5)>
| 1  0  0  0  0 |
| 0  1  0  0  0 |
| 0  0  1  0  0 |
| 0  0  0  1  0 |
| 0  0  0  0  1 |
```

Strictly speaking identity matrix has to be a square matrix, but if you need to create a rectangular matrix which has ones along the "main" diagonal and zeros everywhere else then an optional argument _width_ lets you do that:
```lisp
MX> (identity-matrix 4 6)
#<CLMX::MATRIX SIZE: (4 6)>
| 1  0  0  0  0  0 |
| 0  1  0  0  0  0 |
| 0  0  1  0  0  0 |
| 0  0  0  1  0  0 |
```


### More resources

   - Tutorial
   - [Function Reference](https://github.com/akai7000/clmx/blob/master/FUNCTIONS.md)
   - CLMX website
   - Solve problems with CLMX
   - [Test results](https://travis-ci.com/akai7000/clmx)

   
### Support or Contact

Do you have a suggestion on how to improve **CLMX**? Found a bug you want to report?
Please visit the [Issues](https://github.com/akai7000/clmx/issues) page.

