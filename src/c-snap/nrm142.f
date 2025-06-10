C    NRM142.FOR                                                       
      SUBROUTINE NRM142(MEIG,NTOT,MH0I,MSEDI,SPEED,DH0I,DSEDI,        
     & MINMOD,EIGF,MODAVR,EKM,EIGVAL,                               
     & ATTW, ATTS, ATTB, NORMT)                                         
C__________________________________________________________             
C                                                         |             
C     This routine calculates the normalization           |             
C     factor with the Simpson rule  1 4 2......4 2 4 1    |             
C_________________________________________________________|             
C                                                                       
      REAL MODAVR( * )                                                  
                                                                        
      DOUBLE PRECISION CREF, H0, H1                                     
      DOUBLE PRECISION EKM, EIGVAL                                      
      DOUBLE PRECISION NORMW, NORMS, NORMB, NORMT, SQNORM               
      DOUBLE PRECISION ATTW, ATTS, ATTB                                 
      DOUBLE PRECISION OMEGAC                                           
      DOUBLE PRECISION EIGF( * )                                        
      DOUBLE PRECISION SPEED( * )                                       
      DOUBLE PRECISION CC0, CC1, ROB, ROS                               
      DOUBLE PRECISION DH0I, DSEDI                                      
      DOUBLE PRECISION TWOPI, PI, OMEGA                            
      DOUBLE PRECISION EIGREF, EIGMIN, EIGMAX, STIFF                    
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /DENS8/ ROB, ROS                                           
      COMMON /G/ H0, H1                                                 
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF                        
      COMMON /REFSPD/ CREF                                            
      COMMON /TRIGON/ TWOPI, PI, OMEGA                                  
                                                                        
                                                                        
      ATTW=0.D0                                                         
      NORMW=0.D0                                                        
      DO 5000   N=1,MH0I-1,2                                            
      ATTW=ATTW + 4.0*EIGF(N)**2/SPEED(N+1) +                           
     &            2.0*EIGF(N+1)**2/SPEED(N+2)                           
      NORMW=NORMW + 4.0*EIGF(N)**2 + 2.0*EIGF(N+1)**2                   
 5000 CONTINUE                                                          
      ATTW=ATTW - EIGF(MH0I)**2/SPEED(MH0I+1)                           
      NORMW=NORMW - EIGF(MH0I)**2                                       
      ATTW=ATTW/3.0*DH0I                                                
      NORMW=NORMW/3.0*DH0I                                              
      IF(MSEDI.GT.0)   THEN                                             
       ATTS=EIGF(MH0I)**2/(ROS*ROS*SPEED(MH0I+2))                       
       NORMS=EIGF(MH0I)**2/(ROS*ROS)                                    
       DO 5200   N=MH0I+1,NTOT-1,2                                      
       ATTS=ATTS + 4.0*EIGF(N)**2/SPEED(N+2) +                          
     &             2.0*EIGF(N+1)**2/SPEED(N+3)                          
       NORMS=NORMS + 4.0*EIGF(N)**2 + 2.0*EIGF(N+1)**2                  
 5200  CONTINUE                                                         
       ATTS=ATTS - EIGF(NTOT)**2/SPEED(NTOT+2)                          
       NORMS=NORMS - EIGF(NTOT)**2                                      
       ATTS=ATTS/3.0*DSEDI*ROS                                          
       NORMS=NORMS/3.0*DSEDI*ROS                                        
      ELSE                                                              
       ATTS=0.0D0                                                       
       NORMS=0.0D0                                                      
      END IF                                                            
      NORMB=DH0I*ROB*((ROS/ROB)*EIGF(NTOT))**2*.5/SQRT(EIGVAL-EIGMIN)   
      ATTB=(NORMB*CREF)/DBLE(C2)                                        
      NORMT=NORMW+NORMS+NORMB                                           
      SQNORM=1.0/SQRT(H0*NORMT)                                         
                                                                        
      OMEGAC=OMEGA/CREF                                                 
      DO 5400   N=1,NTOT                                                
      IF(OMEGAC/SPEED(N).LE.EKM)   GO TO 5400                           
      IF(MOD(MEIG+MINMOD-1,2).EQ.0)   THEN                              
C ODD ORDER MODES                                                       
       SQNORM=-SIGN(SQNORM,EIGF(N))                                     
      ELSE                                                              
C EVEN ORDER MODES                                                      
       SQNORM=SIGN(SQNORM,EIGF(N))                                      
      END IF                                                            
      GO TO 5600                                                        
 5400 CONTINUE                                                          
 5600 CONTINUE                                                          
                                                                        
      DO 5800   N=1,NTOT                                                
      EIGF(N)=EIGF(N)*SQNORM                                            
 5800 CONTINUE                                                          
                                                                        
      MODAVR(MEIG)=SQRT(NORMW)*ABS(SQNORM)                              
                                                                        
      RETURN                                                            
      END                                                               
