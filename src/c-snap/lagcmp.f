C   LAGCMP.FOR                                                        
                                                                        
      SUBROUTINE LAGCMP(NPOINT, XAR, YAR, X, Y)                       
C     LAGRANGE INTERPOLATION OF A COMPLEX FUNCTION                      
C     FERLA 07 JUL 1992                                                 
                                                                        
      COMPLEX YAR(*), Y                                                 
      DIMENSION XAR(*)                                                  
                                                                        
      Y= CMPLX( 0.0, 0.0 )                                              
      DO 2000  J= 1, NPOINT                                             
      ZL= 1.0                                                           
      DO 1000  K= 1, NPOINT                                             
      IF(K .NE. J)  ZL= ZL * (X-XAR(K)) / (XAR(J)-XAR(K))               
 1000 CONTINUE                                                          
      Y= Y + ZL * YAR(J)                                                
 2000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
