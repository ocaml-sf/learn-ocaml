*
* small test problem - tp4.mps
*
* knapsack problem where lifted greedy cover cut
* moves lp objective function value
*
NAME          tp4
ROWS                                
 N  obj
 L  knap
 G  r1
 G  r2
 G  r3
COLUMNS                             
    SET00001  'MARKER'                 'INTORG'              
    x1        knap          8.000000
    x1        r1            1.000000
    x2        knap          7.000000
    x2        r2            1.000000
    x3        knap          6.000000
    x3        r3            1.000000
    x4        knap          4.000000
    x4        obj          -1.000000
    x5        knap          6.000000
    x5        obj        -100.000000
    x6        knap         13.500000
    x6        obj        -100.000000
RHS                                 
    RHS       knap         22.000000
    RHS       r1            0.001000
    RHS       r2            0.001000
    RHS       r3            0.001000
BOUNDS                              
 BV ONE       x1            1.000000
 BV ONE       x2            1.000000
 BV ONE       x3            1.000000
 BV ONE       x4            1.000000
 BV ONE       x5            1.000000
 BV ONE       x6            1.000000
ENDATA                              
