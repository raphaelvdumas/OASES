C    NRM122.FOR                                                       
      SUBROUTINE NRM122(MEIG,NTOT,MH0I,MSEDI,SPEED,DH0I,DSEDI,        
     & MINMOD,EIGF,MODAVR,EKM,EIGVAL,                               
     & ATTW, ATTS, ATTB, NORMT)                                         
C__________________________________________________________             
C                                                         |             
C     This routine calculates the normalization           |             
C     factor                                              |             
C_________________________________________________________|             
C                                                                       
      REAL MODAVR( * )                                                  
                                                                        
      DOUBLE PRECISION TEMP                                             
      DOUBLE PRECISION CREF, H0, H1                                     
      DOUBLE PRECISION EKM, EIGVAL                                      
      DOUBLE PRECISION NORMW, NORMS, NORMB, NORMT, SQNORM               
      DOUBLE PRECISION ATTW, ATTS, ATTB                                 
      DOUBLE PRECISION OMEGAC                                           
      DOUBLE PRECISION EIGF( * )                                        
      DOUBLE PRECISION SPEED( * )                                       
      DOUBLE PRECISION CC0, CC1, ROB, ROS, ROS2                         
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
      DO 5000   N=1,MH0I-1                                              
      TEMP= EIGF(N)**2                                                  
      ATTW= ATTW + TEMP / SPEED(N+1)                                    
      NORMW= NORMW + TEMP                                               
 5000 CONTINUE                                                          
      ATTW= ATTW + 0.5 * EIGF(MH0I)**2/SPEED(MH0I+1)                    
      NORMW = NORMW + 0.5 * EIGF(MH0I)**2                               
      ATTW= ATTW * DH0I                                                 
      NORMW= NORMW * DH0I                                               
                                                                        
      IF(MSEDI.GT.0)   THEN                                             
       ROS2= ROS*ROS                                                    
       ATTS= 0.5 * EIGF(MH0I)**2 / (ROS2*SPEED(MH0I+2))                 
       NORMS= 0.5 * EIGF(MH0I)**2 / ROS2                                
       DO 5200   N=MH0I+1,NTOT-1                                        
       TEMP= EIGF(N)**2                                                 
       ATTS= ATTS + TEMP / SPEED(N+2)                                   
       NORMS= NORMS + TEMP                                              
 5200  CONTINUE                                                         
       ATTS= ATTS + 0.5 * EIGF(NTOT)**2/SPEED(NTOT+2)                   
       NORMS= NORMS + 0.5 * EIGF(NTOT)**2                               
       ATTS=ATTS * DSEDI*ROS                                            
       NORMS=NORMS * DSEDI*ROS                                          
      ELSE                                                              
       ATTS=0.0D0                                                       
       NORMS=0.0D0                                                      
      END IF                                                            
      NORMB= ((ROS/ROB)*EIGF(NTOT))**2 * 0.5/SQRT(EIGVAL-EIGMIN)        
      NORMB= NORMB * DH0I*ROB                                           
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
