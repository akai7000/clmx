# ak-matrix

Matrix (2-D tensor) manipulation library for Common Lisp

### How to install
_(this is not working yet; I'm yet to submit it to quicklisp)_

```
(ql:quickload :ak-matrix)
```


### Quick Start

To create a matrix:
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

### Support or Contact

Do you have a suggestion on how to improve **ak-matrix**? Found a bug you want to report?
Please visit the [Issues](https://github.com/andrei12/ak-matrix/issues) page.

