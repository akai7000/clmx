# ak-matrix 0.0.1

Matrix manipulation library for Common Lisp

### How to install
_(this is not working yet; I'm yet to submit it to quicklisp)_

```lisp
(ql:quickload :ak-matrix)
```


### Quick Start

Switch to "mx-matrix" package:
```lisp
CL-USER> (in-package :mx-matrix)
#<PACKAGE "MX-MATRIX">
MX>
```

To create a matrix you can pass a list as initial-contents argument to create-matrix function:
```lisp
MX> (create-matrix :initial-contents '((1 2 3) (4 5 6) (7 8 9)))
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 1  2  3 |
| 4  5  6 |
| 7  8  9 |
MX> 
```

Let's create two square matrices and store them in variables:
```lisp
MX> (defparameter m1 (create-matrix :initial-contents '((3 -2 0) (7 3 1) (0 2 4))))
M1
MX> 

MX> (defparameter m2 (create-matrix :initial-contents '((1 5 4) (-3 -4 2) (1 1 0))))
M2
MX> 

MX> m1
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 3  -2  0 |
| 7   3  1 |
| 0   2  4 |
MX> 

MX> m2
#<MX-MATRIX::MATRIX SIZE: (3 3)>
|  1   5  4 |
| -3  -4  2 |
|  1   1  0 |
MX> 
```

To reference a specific element of the matrix use _ref_ function. Note that the count starts at 1, not at 0.  Think like a mathematician, not a computer scientist.
```lisp
MX> (ref m1 1 2)
-2
MX> 
```

Let's add the matrices m1 and m2:
```lisp
MX> (add-matrices m1 m2)
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 4   3  4 |
| 4  -1  3 |
| 1   3  4 |
MX> 
```

Let's multiply the matrices:
```lisp
MX> (multiply-matrices m1 m2)
#<MX-MATRIX::MATRIX SIZE: (3 3)>
|  9  23   8 |
| -1  24  34 |
| -2  -4   4 |
MX> 
```

To find the number of rows (height of the matrix):
```lisp
MX> (mheight m1)
3
MX> 
```

Similarly, the number of columns (width of the matrix):
```lisp
MX> (mwidth m1)
3
MX> 
```


### More on create-matrix

Besides the _list_ the `create-matrix` function also takes an _array_ in `initial-contents` argument:
```lisp
MX> (create-matrix :initial-contents #2A ((1 2 3) (4 5 6)))
#<MX-MATRIX::MATRIX SIZE: (2 3)>
| 1  2  3 |
| 4  5  6 |
MX> 
```

Another way to create matrices is by specifying a dimensions and initial-element arguments to `create-matrix` function:
```lisp
MX> (create-matrix :dimensions '(3 5) :initial-element 7)
#<MX-MATRIX::MATRIX SIZE: (3 5)>
| 7  7  7  7  7 |
| 7  7  7  7  7 |
| 7  7  7  7  7 |
MX> 
```

### Special matrices
To create a matrix filled with zeros:
```lisp
MX> (zeros 3 4)
#<MX-MATRIX::MATRIX SIZE: (3 4)>
| 0  0  0  0 |
| 0  0  0  0 |
| 0  0  0  0 |
MX> 
```

Matrix filled with ones:
```lisp
MX> (ones 3 4)
#<MX-MATRIX::MATRIX SIZE: (3 4)>
| 1  1  1  1 |
| 1  1  1  1 |
| 1  1  1  1 |
MX> 
```

Identity matrix can be created like this:
```lisp
MX> (identity-matrix 5)
#<MX-MATRIX::MATRIX SIZE: (5 5)>
| 1  0  0  0  0 |
| 0  1  0  0  0 |
| 0  0  1  0  0 |
| 0  0  0  1  0 |
| 0  0  0  0  1 |
MX> 
```

Strictly speaking identity matrix has to be a square matrix, but if you need to create a rectangular matrix which has ones along the "main" diagonal and zeros everywhere else then an optional argument _width_ lets you do that:
```lisp
MX> (identity-matrix 4 6)
#<MX-MATRIX::MATRIX SIZE: (4 6)>
| 1  0  0  0  0  0 |
| 0  1  0  0  0  0 |
| 0  0  1  0  0  0 |
| 0  0  0  1  0  0 |
MX> 
```

Or like this:
```lisp
MX> (identity-matrix 6 4)
#<MX-MATRIX::MATRIX SIZE: (6 4)>
| 1  0  0  0 |
| 0  1  0  0 |
| 0  0  1  0 |
| 0  0  0  1 |
| 0  0  0  0 |
| 0  0  0  0 |
MX> 
```


### Support or Contact

Do you have a suggestion on how to improve **ak-matrix**? Found a bug you want to report?
Please visit the [Issues](https://github.com/andrei12/ak-matrix/issues) page.

