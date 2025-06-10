C   JOIN.FOR                                                         
                                                                        
      SUBROUTINE JOIN(AR, BR, CR, NA, NB, NC, TOL)                   
                                                                        
      REAL AR(*), BR(*), CR(*)                                          
                                                                        
C  INPUT  :    AR(*) and BR(*) arrays containing NA and NB data values. 
C         :    TOL is the threshold used to define whether two values ar
C              equivalent.                                              
C  OUTPUT :    CR(*) array containing the sorted data.                  
C              The sorting is done in ascending order. Values identical 
C              within the predefined tolerance TOL are not repeated.    
                                                                        
                                                                        
      IA= 1                                                             
      IB= 1                                                             
      IC= 0                                                             
      EPS= 1.0 + TOL                                                    
                                                                        
                                                                        
C      PRINT *,' INPUT to JOIN, na, nb ',na, nb                      
C      do 841   jk= 1,max(na,nb)                                        
C      PRINT *, jk, ar(jk), br(jk)                                      
C  841 continue                                                         
                                                                        
                                                                        
                                                                        
      IF( NA .EQ. 0 )   THEN                                            
        NC= NB                                                          
        DO 1000   I= 1, NC                                              
        CR(I)= BR(I)                                                    
 1000   CONTINUE                                                        
C      PRINT *,' OUTPUT from JOIN, na, nb, NC ',na, nb,NC            
                                                                        
        RETURN                                                          
      ELSE IF( NB .EQ. 0 )   THEN                                       
        NC= NA                                                          
        DO 1200   I= 1, NC                                              
        CR(I)= AR(I)                                                    
 1200   CONTINUE                                                        
C      PRINT *,' OUTPUT from JOIN, na, nb, NC ',na, nb,NC            
        RETURN                                                          
      END IF                                                            
                                                                        
                                                                        
 2000 CONTINUE                                                          
                                                                        
      IF( AR(IA) .LE. BR(IB) )   THEN                                   
        IC= IC+1                                                        
        CR(IC)= AR(IA)                                                  
C       Are the two quantities identical within a given tolerance ?     
        IF( EPS*AR(IA) .GE. BR(IB) )   IB= IB+1                         
        IA= IA+1                                                        
                                                                        
      ELSE                                                              
C       ( AR(IA) .GT. BR(IB) )                                          
        IC= IC+1                                                        
        CR(IC)= BR(IB)                                                  
C       Are the two quantities identical within a given tolerance ?     
        IF( AR(IA) .LE. EPS*BR(IB) )   IA= IA+1                         
        IB= IB+1                                                        
      END IF                                                            
                                                                        
      IF( IA .GT. NA)   THEN                                            
C     insert remaining elements from the BR array                       
        DO 2200   I= IB, NB                                             
        CR(IC+I-IB+1)= BR(I)                                            
 2200   CONTINUE                                                        
        NC= IC + (NB-IB) + 1                                            
C      PRINT *,' OUTPUT from JOIN, na, nb, NC ',na, nb,NC            
        RETURN                                                          
      END IF                                                            
                                                                        
      IF( IB .GT. NB)   THEN                                            
C     insert remaining elements from the AR array                       
        DO 2400   I= IA, NA                                             
        CR(IC+I-IA+1)= AR(I)                                            
 2400   CONTINUE                                                        
        NC= IC + (NA-IA) + 1                                            
C      PRINT *,' OUTPUT from JOIN, na, nb, NC ',na, nb,NC            
        RETURN                                                          
      END IF                                                            
                                                                        
      GO TO 2000                                                        
                                                                        
      END                                                               
