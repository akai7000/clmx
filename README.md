# ak-matrix

Matrix (2-D tensor) manipulation library for Common Lisp

### How to install
_(this is not working yet; I'm yet to submit it to quicklisp)_

```
(ql:quickload :ak-matrix)
```


### Quick Start

To create a matrix you can pass a list as initial-contents argument to create-matrix function:
```
CL-USER> (mx:create-matrix :initial-contents '((1 2 3) (4 5 6) (7 8 9)))
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 1  2  3 |
| 4  5  6 |
| 7  8  9 |
CL-USER> 
```

Let's create two square matrices and store them in variables:
```
CL-USER> (defparameter m1 (mx:create-matrix :initial-contents '((3 -2 0) (7 3 1) (0 2 4))))
M1
CL-USER> 

CL-USER> (defparameter m2 (mx:create-matrix :initial-contents '((1 5 4) (-3 -4 2) (1 1 0))))
M2
CL-USER> 

CL-USER> m1
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 3  -2  0 |
| 7   3  1 |
| 0   2  4 |
CL-USER> 

CL-USER> m2
#<MX-MATRIX::MATRIX SIZE: (3 3)>
|  1   5  4 |
| -3  -4  2 |
|  1   1  0 |
CL-USER> 
```

Let's add the matrices m1 and m2:
```
CL-USER> (mx:add-matrices m1 m2)
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 4   3  4 |
| 4  -1  3 |
| 1   3  4 |
CL-USER> 
```

Let's multiply the matrices:
```
CL-USER> (mx:multiply-matrices m1 m2)
#<MX-MATRIX::MATRIX SIZE: (3 3)>
|  9  23   8 |
| -1  24  34 |
| -2  -4   4 |
CL-USER> 
```

To find the number of rows (height of the matrix):
```
CL-USER> (mx:mheight m1)
3
CL-USER> 
```

Similarly, the number of columns (width of the matrix):
```
CL-USER> (mx:mwidth m1)
3
CL-USER> 
```

### More on create-matrix

Besides the list the create-matrix function also takes an _array_ in initial-contents argument:
```
CL-USER> (mx:create-matrix :initial-contents #2A ((1 2 3) (4 5 6)))
#<MX-MATRIX::MATRIX SIZE: (2 3)>
| 1  2  3 |
| 4  5  6 |
CL-USER> 
```

Another way to create matrices is by specifying a dimensions and initial-element arguments to **create-matrix** function:
```
CL-USER> (mx:create-matrix :dimensions '(3 5) :initial-element 7)
#<MX-MATRIX::MATRIX SIZE: (3 5)>
| 7  7  7  7  7 |
| 7  7  7  7  7 |
| 7  7  7  7  7 |
CL-USER> 
```

### Special matrices
To create a matrix filled with zeros:
```
CL-USER> (mx:zeros 3 4)
#<MX-MATRIX::MATRIX SIZE: (3 4)>
| 0  0  0  0 |
| 0  0  0  0 |
| 0  0  0  0 |
CL-USER> 
```

Matrix filled with ones:
```
CL-USER> (mx:ones 3 4)
#<MX-MATRIX::MATRIX SIZE: (3 4)>
| 1  1  1  1 |
| 1  1  1  1 |
| 1  1  1  1 |
CL-USER> 
```

Identity matrix can be created like this:
```
CL-USER> (mx:identity-matrix 5)
#<MX-MATRIX::MATRIX SIZE: (5 5)>
| 1  0  0  0  0 |
| 0  1  0  0  0 |
| 0  0  1  0  0 |
| 0  0  0  1  0 |
| 0  0  0  0  1 |
CL-USER> 
```

Strictly speaking identity matrix has to be a square matrix, but if you need to create a rectangular matrix which has ones along the "main" diagonal and zeros everywhere else. An optional argument _width_ lets you do that:
```
CL-USER> (mx:identity-matrix 4 6)
#<MX-MATRIX::MATRIX SIZE: (4 6)>
| 1  0  0  0  0  0 |
| 0  1  0  0  0  0 |
| 0  0  1  0  0  0 |
| 0  0  0  1  0  0 |
CL-USER> 
```

Or like this:
```
CL-USER> (mx:identity-matrix 6 4)
#<MX-MATRIX::MATRIX SIZE: (6 4)>
| 1  0  0  0 |
| 0  1  0  0 |
| 0  0  1  0 |
| 0  0  0  1 |
| 0  0  0  0 |
| 0  0  0  0 |
CL-USER> 
```


### Support or Contact

Do you have a suggestion on how to improve **ak-matrix**? Found a bug you want to report?
Please visit the [Issues](https://github.com/andrei12/ak-matrix/issues) page.

