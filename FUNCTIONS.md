# Function Reference

-------------------------------------------------
### `add-matrices`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
`MATRIX` | matrix-1 | First matrix
`MATRIX` | matrix-2 | Second matrix

#### Returns
`MATRIX`

#### Description
Adds two matrices matrix-1 and matrix-2 together.

#### Examples
```lisp
MX> (add-matrices
     (create-matrix :initial-contents '((1 2) (3 4)))
     (create-matrix :initial-contents '((5 -3) (0 2))))
#<MATRIX SIZE: (2 2)>
| 6  -1 |
| 3   6 |

MX> (add-matrices
     (create-matrix :initial-contents '((3.43 1.09) (8.33 2.50)))
     (create-matrix :dimensions '(2 2) :initial-element 10))
#<MATRIX SIZE: (2 2)>
| 13.43  11.09 |
| 18.33  12.5 |

MX> (add-matrices
     (create-matrix :initial-contents '((2/3 5/6) (1/3 1)))
     (create-matrix :initial-contents '((7 -1/2) (2/5 0))))
#<MATRIX SIZE: (2 2)>
| 23/3   1/3 |
| 11/15  1   |
```

================================================
### `add-scalar`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
`MATRIX` | matrix | Matrix to add a scalar to
`NUMBER` | scalar | Scalar to be added to matrix

#### Returns
`MATRIX`

#### Description
Add scalar to a matrix.

#### Examples
```lisp
MX> (add-scalar
     (create-matrix :initial-contents '((1 2 3) (4 5 6)))
     100)
#<MATRIX SIZE: (2 3)>
| 101  102  103 |
| 104  105  106 |
```
