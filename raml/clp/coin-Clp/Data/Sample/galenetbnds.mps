* Modified galenet with equalities converted to a pair of inequalities and
* implicit bounds converted to explicit inequalities. All inequalities then
* converted to less than. In other words, the classic canonical form for
* conversion to dual, so that we can do an algebraic  test of getDualRays
* for solvers that don't return the column components.
NAME          galenetbnds
ROWS
 L  S1
 L  S2
 L  S3
 L  NODE4U
 L  NODE4L
 L  NODE5U
 L  NODE5L
 L  D6
 L  D7
 L  D8
 L  T14UB
 L  T14LB
 L  T24UB
 L  T24LB
 L  T25UB
 L  T25LB
 L  T35UB
 L  T35LB
 L  T46UB
 L  T46LB
 L  T47UB
 L  T47LB
 L  T57UB
 L  T57LB
 L  T58UB
 L  T58LB
 N  COST
COLUMNS
    T14       S1                  1.   NODE4U              1.
    T14       NODE4L             -1
    T14       T14UB               1.   T14LB              -1.
    T24       S2                  1.   NODE4U              1.
    T24       NODE4L             -1
    T24       T24UB               1.   T24LB              -1.
    T25       S2                  1.   NODE5U              1.
    T25       NODE5L             -1.
    T25       T25UB               1.   T25LB              -1.
    T35       S3                  1.   NODE5U              1.
    T35       NODE5L             -1.
    T35       T35UB               1.   T35LB              -1.
    T46       D6                 -1.   NODE4U             -1.
    T46       NODE4L              1
    T46       T46UB               1.   T46LB              -1.
    T47       D7                 -1.   NODE4U             -1.
    T47       NODE4L              1
    T47       T47UB               1.   T47LB              -1.
    T57       D7                 -1.   NODE5U             -1.
    T57       NODE5L              1.
    T57       T57UB               1.   T57LB              -1.
    T58       D8                 -1.   NODE5U             -1.
    T58       NODE5L              1.
    T58       T58UB               1.   T58LB              -1.
RHS
    RHS       S1                 20.   S2                 20.
    RHS       S3                 20.   D6                -10.
    RHS       D7                -20.   D8                -30.
    RHS       T14UB              30.
    RHS       T24UB              20.
    RHS       T25UB              10.
    RHS       T35UB              10.
    RHS       T46UB              10.
    RHS       T47UB               2.
    RHS       T57UB              20.
    RHS       T58UB              30.
BOUNDS
 FR BND       T14
 FR BND       T24
 FR BND       T25
 FR BND       T35
 FR BND       T46
 FR BND       T47
 FR BND       T57
 FR BND       T58
ENDATA
