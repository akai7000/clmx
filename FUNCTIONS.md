# Function Reference

### `add-matrices`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix-1 | First matrix
MATRIX    | matrix-2 | Second matrix

#### Returns
MATRIX

#### Description
Adds two matrices matrix-1 and matrix-2 together.

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


-------------------------------------------------
### `add-scalar`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to add a scalar to
NUMBER    | scalar | Scalar to be added to matrix

#### Returns
MATRIX

#### Description
Add scalar to a matrix.

#### Examples
```lisp
MX> (add-scalar
     (create-matrix :contents '((1 2 3) (4 5 6)))
     100)
#<MATRIX SIZE: (2 3)>
| 101  102  103 |
| 104  105  106 |
```


-------------------------------------------------
### `apply-to-each-cell`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix to apply the function to
FUNCTION  | function | Function that is applied to each cell

#### Returns
MATRIX

#### Description
Apply a function to each cell of a matrix.

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


-------------------------------------------------
### `create-matrix`
     
#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
LIST or SIMPLE-ARRAY | contents | Provide either list or array to populate the matrix
LIST | dimensions | a list of 2 integers to specify dimensions of the matrix
NUMBER | initial-element | Any real or complex number to be written in every cell of the matrix

#### Returns
MATRIX

#### Description
Create a matrix. This function creates an instance of MATRIX class.


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


-------------------------------------------------
### `cols`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Description
Get the number of columns of a matrix.

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


-------------------------------------------------
### `det`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix

#### Returns
INTEGER

#### Description
Calculate the determinant of a square matrix.

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


-------------------------------------------------
### `extract-column-as-list`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | col | Column number

#### Returns
LIST

#### Description
Returns the column of the matrix as a list.

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


-------------------------------------------------
### `extract-row-as-list`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
MATRIX    | matrix | Matrix
INTEGER   | row | Row number

#### Returns
LIST

#### Description
Returns the row of the matrix as a list.

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