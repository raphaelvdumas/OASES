C   ISITAN.FOR                                                          
                                                                        
      SUBROUTINE ISITAN(WORD, *)                                        
                                                                        
      CHARACTER*6 WORD                                                  
      DATA I / 1 /                                                      
                                                                        
                                                                        
      IF( (WORD(I:I) .EQ. ' ') .OR.                                     
     &    (WORD(I:I) .EQ. '-') .OR.                                     
     &    (WORD(I:I) .EQ. '+') .OR.                                     
     &    (WORD(I:I) .EQ. '.') )   THEN                                 
          RETURN                                                        
      ELSE                                                              
                                                                        
C        IF(WORD(I:I) .EQ. '
        IA= ICHAR(WORD(I:I))                                            
        IL= ICHAR('0')                                                  
        IH= ICHAR('9')                                                  
        IF( (IA .GE. IL) .AND. ( IA .LE. IH) )   RETURN                 
      END IF                                                            
      RETURN 1                                                          
                                                                        
      END                                                               
