# Function Reference

### `add-matrices`

#### Arguments
Data Type | Argument Name | Description
--------- | ------------- | -----------
`MATRIX` | matrix-1
`MATRIX` | matrix-2

#### Returns
`MATRIX`

#### Description
Add two matrices together.

#### Examples
```lisp
MX> (add-matrices
     (create-matrix :initial-contents '((1 2) (3 4)))
     (create-matrix :initial-contents '((5 -3) (0 2))))
#<MATRIX SIZE: (2 2)>
| 6  -1 |
| 3   6 |
```

	