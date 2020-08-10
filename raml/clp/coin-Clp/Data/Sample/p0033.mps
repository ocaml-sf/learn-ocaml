*NAME:         p0033
*ROWS:         16
*COLUMNS:      33
*INTEGER:      33
*NONZERO:      98
*BEST SOLN:    3089 (opt)
*LP SOLN:      2520.57
*SOURCE:       Crowder-Johnson-Padberg test set
*              
*              E. Andrew Boyd (Rice University)
*APPLICATION:  unknown
*COMMENTS:     pure 0/1 IP
*              5 SOS constraints 
*              
NAME          P0033  
ROWS
 N  R100    
 L  R114    
 L  R115    
 L  R116    
 L  R117    
 L  R118    
 L  R119    
 L  R120    
 L  R121    
 L  R122    
 L  R123    
 L  R124    
 L  R125    
 L  R126    
 L  R127    
 L  R128    
 L  ZBESTROW
COLUMNS
    MARK0000  'MARKER'                 'INTORG'
    C157      R100               171   R114                 1
    C157      R122              -300   R123              -300
    C158      R100               171   R114                 1
    C158      R126              -300   R127              -300
    C159      R100               171   R114                 1
    C159      R119               300   R120              -300
    C159      R123              -300
    C160      R100               171   R114                 1
    C160      R119               300   R120              -300
    C160      R121              -300
    C161      R100               163   R115                 1
    C161      R119               285   R120              -285
    C161      R124              -285   R125              -285
    C162      R100               162   R115                 1
    C162      R119               285   R120              -285
    C162      R122              -285   R123              -285
    C163      R100               163   R115                 1
    C163      R128              -285
    C164      R100                69   R116                 1
    C164      R119               265   R120              -265
    C164      R124              -265   R125              -265
    C165      R100                69   R116                 1
    C165      R119               265   R120              -265
    C165      R122              -265   R123              -265
    C166      R100               183   R117                 1
    C166      R118              -230
    C167      R100               183   R117                 1
    C167      R124              -230   R125              -230
    C168      R100               183   R117                 1
    C168      R119               230   R120              -230
    C168      R125              -230
    C169      R100               183   R117                 1
    C169      R119               230   R120              -230
    C169      R123              -230
    C170      R100                49   R119               190
    C170      R120              -190   R122              -190
    C170      R123              -190
    C171      R100               183   R117                 1
    C172      R100               258   R118              -200
    C173      R100               517   R118              -400
    C174      R100               250   R126              -200
    C174      R127              -200
    C175      R100               500   R126              -400
    C175      R127              -400
    C176      R100               250   R127              -200
    C177      R100               500   R127              -400
    C178      R100               159   R119               200
    C178      R120              -200   R124              -200
    C178      R125              -200
    C179      R100               318   R119               400
    C179      R120              -400   R124              -400
    C179      R125              -400
    C180      R100               159   R119               200
    C180      R120              -200   R125              -200
    C181      R100               318   R119               400
    C181      R120              -400   R125              -400
    C182      R100               159   R119               200
    C182      R120              -200   R122              -200
    C182      R123              -200
    C183      R100               318   R119               400
    C183      R120              -400   R122              -400
    C183      R123              -400
    C184      R100               159   R119               200
    C184      R120              -200   R123              -200
    C185      R100               318   R119               400
    C185      R120              -400   R123              -400
    C186      R100               114   R119               200
    C186      R120              -200   R121              -200
    C187      R100               228   R119               400
    C187      R120              -400   R121              -400
    C188      R100               159   R128              -200
    C189      R100               318   R128              -400
    MARK0001  'MARKER'                 'INTEND'
RHS
    RHS       R114                 1   R115                 1
    RHS       R116                 1   R117                 1
    RHS       R118                -5   R119              2700
    RHS       R120             -2600   R121              -100
    RHS       R122              -900   R123             -1656
    RHS       R124              -335   R125             -1026
    RHS       R126                -5   R127              -500
    RHS       R128              -270
BOUNDS
 UP ONE       C157                 1
 UP ONE       C158                 1
 UP ONE       C159                 1
 UP ONE       C160                 1
 UP ONE       C161                 1
 UP ONE       C162                 1
 UP ONE       C163                 1
 UP ONE       C164                 1
 UP ONE       C165                 1
 UP ONE       C166                 1
 UP ONE       C167                 1
 UP ONE       C168                 1
 UP ONE       C169                 1
 UP ONE       C170                 1
 UP ONE       C171                 1
 UP ONE       C172                 1
 UP ONE       C173                 1
 UP ONE       C174                 1
 UP ONE       C175                 1
 UP ONE       C176                 1
 UP ONE       C177                 1
 UP ONE       C178                 1
 UP ONE       C179                 1
 UP ONE       C180                 1
 UP ONE       C181                 1
 UP ONE       C182                 1
 UP ONE       C183                 1
 UP ONE       C184                 1
 UP ONE       C185                 1
 UP ONE       C186                 1
 UP ONE       C187                 1
 UP ONE       C188                 1
 UP ONE       C189                 1
ENDATA
