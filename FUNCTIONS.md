# Function Reference

[add-matrices](#add-matrices) | [add-scalar](#add-scalar) | [adjugate](#adjugate) | [apply-to-each-cell](#apply-to-each-cell) | [create-matrix](#create-matrix) | [cofactors](#cofactors) | [cols](#cols) | [defmx](#defmx) | [det](#det) | [eigenvalues](#eigenvalues) | [extract-column-as-list](#extract-column-as-list) | [extract-column-as-vector](#extract-column-as-vector) | [extract-row-as-list](#extract-row-as-list) | [extract-row-as-vector](#extract-row-as-vector) | [flip-horizontally](#flip-horizontally) | [flip-vertically](#flip-vertically) | [identity-matrix](#identity-matrix) | [identity-matrix-p](#identity-matrix-p)| [inverse](#inverse) | [multiply-matrices](#multiply-matrices) | [multiply-scalar](#multiply-scalar) | [random-matrix](#random-matrix) | [ref](#ref) | 
[remove-column](#remove-column) | [remove-row](#remove-row) | [rows](#rows) | set-value! | square-matrix-p | transpose | unit-matrix | zero-matrix


### `add-matrices`

#### Description
Adds two matrices matrix-1 and matrix-2 together.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix-1 | First matrix
MATRIX    | matrix-2 | Second matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (add-matrices
        (create-matrix :contents '((1 2) (3 4)))
        (create-matrix :contents '((5 -3) (0 2))))
#<MATRIX SIZE: (2 2)>
| 6  -1 |
| 3   6 |

MX> (add-matrices
     (create-matrix :contents '((3.43 1.09) (8.33 2.50)))
     (create-matrix :dimensions '(2 2) :initial-element 10))
#<MATRIX SIZE: (2 2)>
| 13.43  11.09 |
| 18.33   12.5 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `add-scalar`

#### Description
Add scalar to a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to add a scalar to
NUMBER    | scalar | Scalar to be added to matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (add-scalar
        (create-matrix :contents '((1 2 3) (4 5 6)))
        100)
#<MATRIX SIZE: (2 3)>
| 101  102  103 |
| 104  105  106 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `adjugate`

#### Description
Calculate adjugate matrix (transpose of cofactor matrix).

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (adjugate (create-matrix :contents '((1 2) (3 4))))
#<MATRIX SIZE: (2 2)>
|  4  -2 |
| -3   1 |

MX> (adjugate (create-matrix :contents '((3 2 0) (-1 0 5) (10 2 3))))
#<CLMX-MATRIX::MATRIX SIZE: (3 3)>
| -10  -6   10 |
|  53   9  -15 |
|  -2  14    2 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `apply-to-each-cell`

#### Description
Apply a function to each cell of a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to apply the function to
FUNCTION  | function | Function that is applied to each cell

#### Returns
MATRIX

#### Examples
```lisp
MX> (apply-to-each-cell (create-matrix :contents '((4 9) (16 25))) #'sqrt)
#<MATRIX SIZE: (2 2)>
| 2.0  3.0 |
| 4.0  5.0 |

MX> (apply-to-each-cell
        (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9)))
        (lambda (x) (expt x 2)))
#<MATRIX SIZE: (3 3)>
|  1   4   9 |
| 16  25  36 |
| 49  64  81 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `create-matrix`
     
#### Description
Create a matrix. This function creates an instance of MATRIX class.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
LIST or SIMPLE-ARRAY | contents | Provide either list or array to populate the matrix
LIST | dimensions | a list of 2 integers to specify dimensions of the matrix
NUMBER | initial-element | Any real or complex number to be written in every cell of the matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (create-matrix :contents '((2 -2 5) (-1 0 1)))
#<MATRIX SIZE: (3 3)>
|  2  -2  5 |
| -1   0  1 |

MX> (create-matrix :contents #2A ((1 2 3) (4 5 6) (7 8 9))
#<CLMX::MATRIX SIZE: (2 3)>
| 1  2  3 |
| 4  5  6 |
| 7  8  9 |

MX> (create-matrix :dimensions '(3 4) :initial-element 5)
#<MATRIX SIZE: (3 4)>
| 5  5  5  5 |
| 5  5  5  5 |
| 5  5  5  5 |

MX> (create-matrix :dimensions '(3 3))
#<MATRIX SIZE: (3 3)>
| 0  0  0 |
| 0  0  0 |
| 0  0  0 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `cofactors`

#### Description
Calculate matrix of cofactors.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (cofactors (create-matrix :contents '((1 2) (3 4))))
#<MATRIX SIZE: (2 2)>
|  4  -3 |
| -2   1 |

MX> (cofactors (create-matrix :contents '((3 2 0) (-1 0 5) (10 2 3))))
#<CLMX-MATRIX::MATRIX SIZE: (3 3)>
| -10   53  -2 |
|  -6    9  14 |
|  10  -15   2 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `cols`

#### Description
Get the number of columns of a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Examples
```lisp
MX> (cols 
        (create-matrix :dimensions '(100 50) :initial-element 3))
50

MX> (cols (zero-matrix 15 20))
20

MX> (cols 
        (create-matrix :contents '((1 2 3 4 5) (6 7 8 9 10))))
5
```

[Go to top](#start-of-content)

-------------------------------------------------
### `defmx`

#### Description
Shortcut - creates matrix based on contents and assigns to var.
For example, this: (defmx m '((1 2 3) (4 5 6)))
is the same as this: (defmx m '((1 2 3) (4 5 6)))

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
SYMBOL    | var | Name of the variable
LIST or SIMPLE-ARRAY | contents | Provide either list or array to populate the matrix

#### Returns
COMMON-LISP:SYMBOL

#### Examples
```lisp
MX> (defmx m '((1 2 3) (4 5 6)))
M

MX> (defmx a #2A ((1 2 3) (4 5 6) (7 8 9)))
A
```

[Go to top](#start-of-content)

-------------------------------------------------
### `det`

#### Description
Calculate the determinant of a square matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Examples
```lisp
MX> (det 
        (create-matrix :contents '((1 3) (-2 5))))
11

MX> (det (unit-matrix 3 3))
0

MX> (det 
        (create-matrix :contents '((7 -2 3) (1 6 0) (-4 9 2))))
187
```

[Go to top](#start-of-content)

-------------------------------------------------
### `eigenvalues`

#### Description
Calculate the eigenvalues of a square matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Examples
```lisp
MX> (eigenvalues
        (create-matrix :contents '((6 -1) (2 3))))
5.0
4.0

MX> (eigenvalues (unit-matrix 2 2))
2.0
0.0
```

[Go to top](#start-of-content)

-------------------------------------------------
### `extract-column-as-list`

#### Description
Returns the column of the matrix as a list.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | col | Column number

#### Returns
LIST

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (extract-column-as-list m 1)
(3 2 -1)

MX> (extract-column-as-list m 2)
(4 -2 0)

MX> (extract-column-as-list m 3)
(7 5 1)
```

[Go to top](#start-of-content)

-------------------------------------------------
### `extract-column-as-vector`

#### Description
Returns the column of the matrix as a vector.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | col | Column number

#### Returns
SIMPLE-VECTOR

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (extract-column-as-vector m 1)
#(3 2 -1)

MX> (extract-column-as-vector m 2)
#(4 -2 0)

MX> (extract-column-as-vector m 3)
#(7 5 1)
```

[Go to top](#start-of-content)

-------------------------------------------------
### `extract-row-as-list`

#### Description
Returns the row of the matrix as a list.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | row | Row number

#### Returns
LIST

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (extract-row-as-list m 1)
(3 4 7)

MX> (extract-row-as-list m 2)
(2 -2 5)

MX> (extract-row-as-list m 3)
(-1 0 1)
```

[Go to top](#start-of-content)

-------------------------------------------------
### `extract-row-as-vector`

#### Description
Returns the row of the matrix as a vector.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | row | Row number

#### Returns
SIMPLE-VECTOR

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (extract-row-as-vector m 1)
#(3 4 7)

MX> (extract-row-as-vector m 2)
#(2 -2 5)

MX> (extract-row-as-vector m 3)
#(-1 0 1)
```

[Go to top](#start-of-content)

-------------------------------------------------
### `flip-horizontally`

#### Description
Flip the matrix horizontally (reverse column order).

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to be flipped

#### Returns
MATRIX

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (flip-horizontally m)
#<MATRIX SIZE: (3 3)>
| 7   4   3 |
| 5  -2   2 |
| 1   0  -1 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `flip-vertically`

#### Description
Flip the matrix vertically (reverse row order).

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to be flipped

#### Returns
MATRIX

#### Examples
```lisp
MX> (defparameter m (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> m
#<MATRIX SIZE: (3 3)>
|  3   4  7 |
|  2  -2  5 |
| -1   0  1 |

MX> (flip-vertically m)
#<MATRIX SIZE: (3 3)>
| -1   0  1 |
|  2  -2  5 |
|  3   4  7 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `identity-matrix`

#### Description
Create an identity matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
INTEGER    | num-rows | Number of rows
INTEGER    | num-cols | Number of columns (optional)

#### Returns
MATRIX

#### Examples
```lisp
MX> (identity-matrix 5)
#<MATRIX SIZE: (5 5)>
| 1  0  0  0  0 |
| 0  1  0  0  0 |
| 0  0  1  0  0 |
| 0  0  0  1  0 |
| 0  0  0  0  1 |

MX> (identity-matrix 3 6)
#<MATRIX SIZE: (3 6)>
| 1  0  0  0  0  0 |
| 0  1  0  0  0  0 |
| 0  0  1  0  0  0 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `identity-matrix-p`

#### Description
Determine if the matrix is an identity matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to be checked

#### Returns
BOOLEAN

#### Examples
```lisp
MX> (defparameter m1 (create-matrix :contents '((3 4 7) (2 -2 5) (-1 0 1))))
M

MX> (identity-matrix-p m1)
NIL

MX> (defparameter m2 (create-matrix :contents '((1 0 0) (0 1 0) (0 0 1))))
M

MX> (identity-matrix-p m2)
T
```

[Go to top](#start-of-content)

-------------------------------------------------
### `inverse`

#### Description
Calculate the inverse of a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (inverse (create-matrix :contents '((1 2) (3 4))))
#<MATRIX SIZE: (2 2)>
| -2.0   1.0 |
|  1.5  -0.5 |

MX> (inverse (create-matrix :contents '((3 2 0) (-1 0 5) (10 2 3))))
#<CLMX-MATRIX::MATRIX SIZE: (3 3)>
| -0.131579  -0.078947   0.131579 |
|  0.697368   0.118421  -0.197368 |
| -0.026316   0.184211   0.026316 |

MX> (inverse (unit-matrix 5 5))
Inverse does not exist - determinant of matrix is 0.
   [Condition of type SIMPLE-ERROR]

```

[Go to top](#start-of-content)

-------------------------------------------------
### `multiply-matrices`

#### Description
Multiply two matrices.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix-1 | Matrix
MATRIX    | matrix-2 | Matrix

#### Returns
MATRIX

#### Examples
```lisp
MX> (multiply-matrices
		(create-matrix :contents '((1 2) (3 -4)))
		(create-matrix :contents '((7 0) (-2 5))))
#<CLMX-MATRIX::MATRIX SIZE: (2 2)>
|  3   10 |
| 29  -20 |

MX> (multiply-matrices
		(create-matrix :contents '((3 2 0) (-1 0 5)))
		(create-matrix :contents '((4 -1) (0 2) (8 3))))
#<CLMX-MATRIX::MATRIX SIZE: (2 2)>
| 12   1 |
| 36  16 |

MX> (multiply-matrices
		(unit-matrix 5 5)
		(zero-matrix 5 5))
#<CLMX-MATRIX::MATRIX SIZE: (5 5)>
| 0  0  0  0  0 |
| 0  0  0  0  0 |
| 0  0  0  0  0 |
| 0  0  0  0  0 |
| 0  0  0  0  0 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `multiply-scalar`

#### Description
Multiply matrix by a scalar.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to be multiplied
NUMBER    | scalar | Scalar to be multiplied

#### Returns
MATRIX

#### Examples
```lisp
MX> (multiply-scalar
		(create-matrix :contents '((1 2) (3 4)))
		5)
#<CLMX-MATRIX::MATRIX SIZE: (2 2)>
|  5.0  10.0 |
| 15.0  20.0 |

MX> (multiply-scalar
		(unit-matrix 4 4)
		7)
#<CLMX-MATRIX::MATRIX SIZE: (4 4)>
| 7.0  7.0  7.0  7.0 |
| 7.0  7.0  7.0  7.0 |
| 7.0  7.0  7.0  7.0 |
| 7.0  7.0  7.0  7.0 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `random-matrix`

#### Description
Create a matrix populated with random numbers.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
NUMBER    | rows | Number of rows
NUMBER    | cols | Number of columns
NUMBER    | min-num | The smallest random number
NUMBER    | max-num | The largest random number

#### Returns
MATRIX

#### Examples
```lisp
MX> (random-matrix 3 3 100 120)
#<CLMX-MATRIX::MATRIX SIZE: (3 3)>
| 119  110  118 |
| 113  117  114 |
| 107  109  113 |
MX> (random-matrix 10 1 1 100)
#<CLMX-MATRIX::MATRIX SIZE: (1 10)>
| 77  39  92  71  67  78  51  53  12  19 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `ref`

#### Description
Get a value from a matrix at row 'row' and column 'col'.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
NUMBER    | row | Row of the value
NUMBER    | col | Column of the value

#### Returns
NUMBER

#### Examples
```lisp
MX> (ref (create-matrix :contents '((5 -2) (2 0))) 1 1)
5

MX> (ref (unit-matrix 5 5) 2 5)
1
```

[Go to top](#start-of-content)

-------------------------------------------------
### `remove-column`

#### Description
Remove column 'col-num' from a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
NUMBER    | col-num | Column to be removed

#### Returns
MATRIX

#### Examples
```lisp
MX> (remove-column (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9))) 2)
#<CLMX-MATRIX::MATRIX SIZE: (3 2)>
| 1  3 |
| 4  6 |
| 7  9 |

MX> (remove-column (create-matrix :contents '((1 2))) 1)
#<CLMX-MATRIX::MATRIX SIZE: (1 1)>
| 2 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `remove-row`

#### Description
Remove row 'row-num' from a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
NUMBER    | row-num | Row to be removed

#### Returns
MATRIX

#### Examples
```lisp
MX> (remove-row (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9))) 2)
#<CLMX-MATRIX::MATRIX SIZE: (2 3)>
| 1  2  3 |
| 7  8  9 |

MX> (remove-row (create-matrix :contents '((1) (2))) 1)
#<CLMX-MATRIX::MATRIX SIZE: (1 1)>
| 2 |
```

[Go to top](#start-of-content)

-------------------------------------------------
### `rows`

#### Description
Get the number of rows of a matrix.

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Examples
```lisp
MX> (rows 
        (create-matrix :dimensions '(100 50) :initial-element 3))
100

MX> (rows (zero-matrix 15 20))
15

MX> (rows 
        (create-matrix :contents '((1 2 3 4 5) (6 7 8 9 10))))
2
```

[Go to top](#start-of-content)