      SUBROUTINE WEIGHT( X, NX, XTAB, NXTAB, W, IX )                    
C                                                                       
C     Given                                                             
C        X(*)    abscissas                                              
C        XTAB(*) points for tabulation                                  
C        NX      number of x points                                     
C                                                                       
C     Compute                                                           
C        W(*)    weights for linear interpolation                       
C        IX(*)   indices for    "         "                             
C                                                                       
      INTEGER IX(*)                                                     
      REAL X(*), XTAB(*), W(*)                                          
C                                                                       
C     LOCATE INDICES                                                    
C                                                                       
      L = 1                                                             
      DO 2000 IXTAB = 1, NXTAB                                          
C        ------ DOES [X(L), X(L+1)] BRACKET RCVR DEPTH?                 
 1000    IF ( X(L+1) .GE. XTAB(IXTAB) .OR. L .GE. NX-1 ) THEN           
C           ------ YES: MAKE NOTE OF L AND WEIGHT FOR INTERPOLATION     
            IX(IXTAB) = L                                               
            W(IXTAB) = ( XTAB(IXTAB) - X(L) ) / ( X(L+1) - X(L) )       
            GOTO 2000                                                   
         ELSE                                                           
C           ------ NO: ADVANCE TO NEXT DEPTH                            
            IF ( L .LT. NX-1 ) L = L + 1                                
            GOTO 1000                                                   
         ENDIF                                                          
 2000 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
