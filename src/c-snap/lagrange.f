C   LAGRANGE.FOR
      SUBROUTINE LAGRAN(MODQTY,NMESH,MY,MAXMSH,MODEN,DH0SQ)                  
C                                                                       
      DOUBLE PRECISION MY(MAXMSH,MODEN)                                 
      DOUBLE PRECISION DH0SQ(8), ZL, X, Y

      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
C                                                                       
  100 FORMAT(1X,'*** EXTRAP NOT PERFORMED BECAUSE OF ARRAY SIZE',       
     & ' LIMITATION ***')                                               
                                                                        
      IF(NMESH+1 .GT. MAXMSH)   THEN                                    
       WRITE(LUPRT,100)                                                     
       RETURN                                                           
      END IF                                                            
      IF(NMESH .GE. 2)   THEN                                           
       X=DH0SQ(NMESH+1)                                                 
       DO 3000   M=1,MODQTY                                             
       Y=0.0                                                            
       DO 2000  J=1,NMESH                                               
       ZL=1.0                                                           
       DO 1000  K=1,NMESH                                               
       IF(K.NE.J)  ZL=ZL*(X-DH0SQ(K))/(DH0SQ(J)-DH0SQ(K))               
 1000  CONTINUE                                                         
       Y=Y+ZL*MY(J,M)                                                   
 2000  CONTINUE                                                         
       MY(NMESH+1,M)= Y                                                 
 3000  CONTINUE                                                         
      ELSE                                                              
       DO 4000   M=1,MODQTY                                             
       MY(2,M)=MY(1,M)                                                  
 4000  CONTINUE                                                         
      END IF                                                            
      RETURN                                                            
      END                                                               
