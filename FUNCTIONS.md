# Function Reference

[add-matrices](#add-matrices) | [add-scalar](#add-scalar) | [apply-to-each-cell](#apply-to-each-cell) | [create-matrix](#create-matrix) | [cols](#cols) | [det](#det) | [extract-column-as-list](#extract-column-as-list) | [extract-column-as-vector](#extract-column-as-vector) | [extract-row-as-list](#extract-row-as-list) | [extract-row-as-vector](#extract-row-as-vector) | [flip-horizontally](#flip-horizontally) | [flip-vertically](#flip-vertically) | [identity-matrix](#identity-matrix) | [identity-matrix-p](#identity-matrix-p)

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
