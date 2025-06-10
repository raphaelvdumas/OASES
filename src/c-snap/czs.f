      FUNCTION CZS( ZS, DEPTH, SPEED, NDP )                             

      DOUBLE PRECISION DEPTH(*), SPEED(*)                               

      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT

C                                                                       
C   DEPTH ARRAY IS NO LONGER NORMALIZED (AS OPPOSED TO OLDER VERSIONS)  
C                                                                       
      Z2=DEPTH(1)                                                       
      C2=SPEED(1)                                                       
      IF(Z2.NE.0.0)   THEN                                              
       WRITE(LUPRT,*) ' ERROR IN FIRST VALUE OF SVP '                       
       STOP                                                             
      END IF                                                            
      IF(NDP.EQ.1)  THEN                                                
       CZS=C2                                                           
       RETURN                                                           
      END IF                                                            
      DO 1000    KK= 2, NDP                                             
      Z1=Z2                                                             
      C1=C2                                                             
      Z2=DEPTH(KK)                                                      
      C2=SPEED(KK)                                                      
      IF(ZS.EQ.Z2)   THEN                                               
       CZS=C2                                                           
       RETURN                                                           
      END IF                                                            
      IF(ZS.LT.Z2)   THEN                                               
       CZS=C1 + ((C2-C1)*(ZS-Z1))/(Z2-Z1)                               
       RETURN                                                           
      END IF                                                            
 1000 CONTINUE                                                          
      CZS=C1 + ((C2-C1)*(ZS-Z1))/(Z2-Z1)                                
      WRITE(LUPRT,*)
     &  ' WARNING: SOURCE DEPTH NOT INCLUDED IN WATER SVP '    
      RETURN                                                            
      END                                                               
