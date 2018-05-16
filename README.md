# ak-matrix

Matrix manipulation library for Common Lisp

### How to install

```
(ql:quickload :ak-matrix)
```


### How to use

To create a matrix:
```
CL-USER> (mx:create-matrix :initial-contents '((1 2 3) (4 5 6) (7 8 9)))
#<MX-MATRIX::MATRIX SIZE: (3 3)>
| 1  2  3 |
| 4  5  6 |
| 7  8  9 |
CL-USER> 
```


### Support or Contact

Do you have a suggestion on how to improve **ak-matrix**? Found a bug you want to report?
Please visit the [Issues](https://github.com/andrei12/ak-matrix/issues) page.

