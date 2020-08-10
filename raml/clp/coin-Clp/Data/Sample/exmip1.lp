\ENCODING=ISO-8859-1
\Problem name: exmip1.mps

Minimize
 OBJ: COL01 + 2 COL05 - COL08
Subject To
 ROW01: 3 COL01 + COL02 - 2 COL04 - COL05 - COL08 >= 2.5
 ROW02: 2 COL02 + 1.1 COL03 <= 2.1
 ROW03: COL03 + COL06  = 4
 ROW04: 2.8 COL04 - 1.2 COL07 - RgROW04  = 1.8
 ROW05: 5.6 COL01 + COL05 + 1.9 COL08 - RgROW05  = 15
Bounds
      COL01 >= 2.5
 0 <= COL02 <= 4.1
 0 <= COL03 <= 1
 0 <= COL04 <= 1
 0.5 <= COL05 <= 4
 0 <= COL08 <= 4.3
 0 <= RgROW04 <= 3.2
-12 <= RgROW05 <= 0
Binaries
 COL03  COL04 
End
