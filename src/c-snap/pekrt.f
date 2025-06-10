      FUNCTION PEKRT( Z )                                               
                                                                        
C     PEKERIS BRANCH OF THE SQUARE ROOT                                 
C     THIS EXPOSES LEAKY MODES                                          
                                                                        
C      COMPLEX*16 PEKRT, Z                                              
      COMPLEX PEKRT, Z                                                  
                                                                        
      IF ( REAL(Z) .GT. 0.0 ) THEN                                      
         PEKRT = SQRT(Z)                                                
      ELSE                                                              
         PEKRT = (0.0, 1.0) * SQRT(-Z)                                  
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
