C   MDIAG.FOR                                                           
      SUBROUTINE MDIAG(MH0I,MSEDI,DH0I,DSEDI,                           
     & SPEED,ADA,C0,Z0,C1,Z1)                                           
C                                                                       
C___________________________________________________________            
C                                                          |            
C     This routine defines the matrix diagonal             |            
C__________________________________________________________|            
C                                                                       
      DOUBLE PRECISION HRAT, HRATSQ, CC0, CC1, ROB, ROS                 
      DOUBLE PRECISION CMIN
      DOUBLE PRECISION ADA( * ), SPEED( * )
      DOUBLE PRECISION EIGREF, EIGMIN, EIGMAX, EIGDH0, EIGDH1
      DOUBLE PRECISION STIFF, H0, H1                                    
      DOUBLE PRECISION DH0I, DSEDI                                      
      DOUBLE PRECISION C0(*), Z0(*), C1(*), Z1(*)           
      DOUBLE PRECISION TWOPI, PI, OMEGA                                 
      DOUBLE PRECISION CON1, CON2, CON3, CON4, CON5, SEDK               
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /CONST/ CON1, CON2, CON3, CON4, CON5, SEDK                 
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /DENS8/ ROB, ROS                                           
      COMMON /G/ H0, H1                                                 
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF                        
      COMMON /NA/ ND0, ND1, CMIN                                        
      COMMON /TRIGON/ TWOPI, PI, OMEGA                                  
                                                                        
                                                                        
C     DEFINE MATRIX DIAGONAL                                            
                                                                        
      CALL VELCTY( MH0I, MSEDI, SPEED, DH0I, DSEDI,                        
     &             C0, Z0, C1, Z1)
                                                                        
      EIGDH0= EIGREF*DH0I                                               
      DO 1200   N= 1, MH0I                                              
      ADA(N)= (EIGDH0/SPEED(N+1))**2-2.                                 
 1200 CONTINUE                                                          
      EIGDH1= EIGREF*DSEDI                                             
      DO 1400  N= MH0I+1, MH0I+MSEDI                                    
      ADA(N)= (EIGDH1/SPEED(N+2))**2-2.                                
 1400 CONTINUE                                                          
                                                                        
      IF(H1/H0 .LT. 1.0D-6)   THEN                                      
       HRAT= 1.                                                         
       HRATSQ= 1                                                        
       CON3= 1.                                                         
      ELSE                                                              
       HRAT= (MSEDI*H0)/(MH0I*H1)                                       
       HRATSQ= HRAT**2                                                  
       CON3= (MH0I*H1)**2/(MSEDI*H0)**2                                 
      END IF                                                            
                                                                        
      IF(H1 .GT. 0.0)   THEN                                            
       SEDK= ((OMEGA*H1)/(C1(1)*MSEDI))**2                              
      ELSE                                                              
       SEDK= 0.0                                                        
      END IF                                                            
                                                                        
      CON1= 2.*(STIFF-HRATSQ/ROS)/(STIFF+HRAT)                          
      CON4= 2./ROS*HRATSQ/(STIFF+HRAT)                                  
      CON5= ROS/(ROB*HRAT)                                              
                                                                        
C  TO AVOID NUMERICAL PROBLEMS THE MINIMUM ACCEPTABLE VALUE FOR EIGMIN       
C  IS SET TO 1.0D-30
C     EIGMIN= ( (OMEGA*H0)/(DBLE(C2)*MH0I) )**2                         
      EIGMIN=   (OMEGA*H0) / DBLE(MH0I)
      IF( LOG10(EIGMIN) - LOG10(DBLE(C2)) .LE. -15.0 )   THEN
        EIGMIN= 1.0D-30
      ELSE
        EIGMIN= ( EIGMIN/DBLE(C2) )**2
      END IF

      EIGMAX= ( (OMEGA*H0)/(CMIN    *MH0I) )**2                         
                                                                        
      RETURN                                                            
      END                                                               
