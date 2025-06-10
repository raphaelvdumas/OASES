C   ISOBR.FOR                                                           
      SUBROUTINE ISOBR( I, FRQ, MODQTY,                 
     & DH0, DSED, MH0, MSED,                                            
     & ADA, SPEED, ISO, MY, MAXMSH, MODEN,
     & C0, Z0, C1, Z1, * )
C__________________________________________________________             
C                                                          |            
C     The subject is to find an approximate solution for   |            
C     the eigenvalues of a continuous diff. equation by    |            
C     finite difference and extrapolation                  |            
C__________________________________________________________|            
C                                                                       
                                                                        
      INTEGER MH0(8), MSED(8)                                           
                                                                        
      DOUBLE PRECISION MY(MAXMSH,MODEN), MYBR, ISO(MODEN),              
     &                 ADA( * )
      DOUBLE PRECISION DH0(8), DSED(8)                                  
      DOUBLE PRECISION SPEED( * )
      DOUBLE PRECISION FRQ, A, B               
      DOUBLE PRECISION C0(*), Z0(*), C1(*), Z1(*)           
                                                                        
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
                                                                        
      CALL ISOINT( ISO, MODEN,
     & DH0(I), MH0(I), MSED(I), DSED(I),
     & FRQ, SPEED, ADA, C0, Z0, C1, Z1, *9999 )
      MODQTY=MAXMOD-MINMOD+1
      DO 1000 M=1,MODQTY                                                
      A=ISO(M+1)                                                        
      B=ISO(M)
      CALL BRENT(A,B,MYBR,MH0(I),MSED(I),DH0(I),DSED(I),ADA)
      MY(I,M)=MYBR                                                      
 1000 CONTINUE                                                          
                                                                        
      RETURN                                                            

 9999 RETURN 1
      END                                                               
