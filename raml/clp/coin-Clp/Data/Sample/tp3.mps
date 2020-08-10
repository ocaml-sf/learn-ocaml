*
* small test problem derived from P0548
*
NAME          tp3                
ROWS                                
 N  R1001                           
 L  R1006                           
 G  ROWA                            
 G  ROWB                            
COLUMNS                             
    SET00001  'MARKER'                 'INTORG'              
    C1045     R1001       155.000000
    C1045     R1006       161.000000
    C1045     ROWA          1.000000
    C1047     R1006      -120.000000
    C1050     R1006       -68.000000
    C1050     ROWB          1.000000
RHS                                 
    RHS       R1006        -5.000000
    RHS       ROWA          0.627000
    RHS       ROWB          0.380000
BOUNDS                              
 BV ONE       C1045         1.000000
 BV ONE       C1047         1.000000
 BV ONE       C1050         1.000000
ENDATA                              
