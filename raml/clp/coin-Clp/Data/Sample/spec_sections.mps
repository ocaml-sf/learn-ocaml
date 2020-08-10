*NAME         Test
*Author       H.I. Gassmann
*Date         08/Oct/2013
*Purpose      This file tests the processing of the
*             CoinMpsIO subsystem --- it does not make
*             much sense as an optimization problem
*             but is useful to illustrate a fully loaded MPS file
NAME
ROWS
 N  obj     
 L  c1      
COLUMNS
    x0        obj                  1   c1                   1
    INT       'MARKER'                 'INTORG'
    x1        obj                 -1   c1                  10
    INT       'MARKER'                 'INTEND'
* S1 NAME1     'MARKER'                 'SOSORG'
    x2        obj                 -9   c1                   5
    x3        obj                 -6   c1                   8
*    NAME1     'MARKER'                 'SOSEND'
    x4        obj                  1   c1                   1
    x5        obj                 -6   c1                   8
    x6        obj                  1   c1                   1
    x7        obj                  1   c1                   1
    x8        obj                 -6   c1                   8
    x9        obj                 -2   c1                   1
    x10       obj                 -3   c1                   1
    x11       obj                 -1   c1                  -1
    x12       obj                 -2   c1                   1
    x13       obj                 -3   c1                   1
    x14       obj                 -9   c1                   5
RHS
    rhs       c1               10000
RANGES
    range     c1                2000
BOUNDS
 LI BOUND     x1                   2
 UI BOUND     x1                   3
SOS
 S1 set1
    x2                         
    x3                         
 S2 set2
    x4                            20
    x5                            40
QUADOBJ
    x6        x6                   1
    x6        x7                   2
    x7        x7                   7
CSECTION      cone1       0.0          QUAD
    x8
    x9
    x10
CSECTION      cone2       0.0          RQUAD
    x11
    x12
    x13
    x14
*BASIS
* XU x6        c1
* BS x7
ENDATA

