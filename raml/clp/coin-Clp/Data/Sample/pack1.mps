************************************************************************
*
*  The data in this file represents a tiny covering problem:
*
*  Minimize or maximize Z = x1 + x2 + x3
*
*  Subject to:
*
*  1.0 <= x1 +  x2
*  1.0 <=       x2 + x3
*  1.0 <= x1         x3
*
*  where all variables are binary.
*
************************************************************************
NAME          EXAMPLE                                        
ROWS                                                         
 N  OBJ                                                      
 G  ROW01                                                    
 G  ROW02                                                    
 G  ROW03                                                    
COLUMNS                                                      
    INT1      'MARKER'                 'INTORG'              
    COL01     OBJ                1.0                         
    COL01     ROW01              1.0   ROW03              1.0
    COL02     OBJ                1.0                         
    COL02     ROW01              1.0   ROW02              1.0
    COL03     OBJ                1.0                         
    COL03     ROW02              1.0   ROW03              1.0
    INT1END   'MARKER'                 'INTEND'              
RHS                                                          
    RHS1      ROW01              1.0                         
    RHS1      ROW02              1.0                         
    RHS1      ROW03              1.0                         
ENDATA                                                       
