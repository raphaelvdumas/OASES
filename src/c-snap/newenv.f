C   NEWENV.FOR                                                          
      SUBROUTINE NEWENV( Z0, C0, Z1, C1, Z0L, C0L, C1L, Z1L,
     &                                   Z0R, C0R, Z1R, C1R)


C   RKM= -1  :
C   This value signals the first call to sub NEWENV.
C   RKM is subsequently assigned the range coordinate where a new mode
C   set is required.

C   RKM= 0
C   This is the range coordinate of the FIRST environment.
C   If more environments exist, then a second one is read to define
C   the LEFT and RIGHT extremes of the first region.

C   RKM .GT. 0
C   In this case a new region is defined, with the current
C   "end of region properties" becoming the left extreme and with the
C   right extreme defined by the newly read properties.

      INTEGER FIXDZ                                                     
                                                                        
      DOUBLE PRECISION H0L, H1L, CC0L, CC1L
      DOUBLE PRECISION H0R, H1R, CC0R, CC1R
      DOUBLE PRECISION Z0(1), C0(1), Z1(1), C1(1)                       
      DOUBLE PRECISION Z0L(1), C0L(1), Z1L(1), C1L(1)                   
      DOUBLE PRECISION Z0R(1), C0R(1), Z1R(1), C1R(1)                   
      DOUBLE PRECISION H0, H1, CMIN, CC0, CC1, HTMP                     
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /G/ H0, H1                                                 
      COMMON /LEFT/ RKML, BETAL(3), SCATTL(2), R1L, R2L,                
     &               C2L, C2SL, H0L, H1L, ND0L, ND1L
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
      COMMON /NA/ ND0, ND1, CMIN                                        
      COMMON /RIGHT/ RKMR, BETAR(3), SCATTR(2), R1R, R2R,               
     &               C2R, C2SR, H0R, H1R, ND0R, ND1R
      COMMON /RNGPRF/ NSECT, RKM
C ****************************************************************      
                                                                        

      IF( RKM .EQ. -1)  THEN
        IF( ( NSECT .EQ. 1) )   THEN
          READ(10) RKM                                                      
          READ(10) R1, R2, H0, H1, ND0, ND1                                 
          READ(10) (BETA(J), J= 1, 3), SCATT, CC0, CC1                     
C         SVP IN WATER COLUMN                                                 
          READ(10) (Z0(J), C0(J), J= 1, ND0)                                
C         SVP IN SEDIMENT LAYER                                               
          IF(ND1.GT.0)   READ(10) (Z1(J), C1(J), J= 1, ND1)                 
C         BOTTOM                                                              
          READ(10) C2, C2S
          RETURN
        ELSE
          READ(10) RKMR                                                      
          READ(10) R1R, R2R, H0R, H1R, ND0R, ND1R
          READ(10) (BETAR(J), J= 1, 3), SCATTR, CC0R, CC1R
C         SVP IN WATER COLUMN                                                 
          READ(10) (Z0R(J), C0R(J), J= 1, ND0R)                                
C         SVP IN SEDIMENT LAYER                                               
          IF(ND1R.GT.0)   READ(10) (Z1R(J), C1R(J), J= 1, ND1R)                 
C         BOTTOM                                                              
          READ(10) C2R,C2SR

          H0=H0R                                                           
          H1=H1R                                                           
          R1=R1R                                                           
          C2=C2R                                                           
          R2=R2R                                                           
          C2S=C2SR                                                         
          ND0=ND0R                                                         
          ND1=ND1R                                                         
          DO 3000    I=1,ND0R                                               
          Z0(I)=Z0R(I)                                                     
          C0(I)=C0R(I)                                                     
 3000     CONTINUE                                                          
          DO 3200    I=1,ND1R                                               
          Z1(I)=Z1R(I)                                                     
          C1(I)=C1R(I)                                                     
 3200     CONTINUE                                                          
          DO 3400    I=1,3                                                  
          BETA(I)=BETAR(I)                                                 
 3400     CONTINUE                                                          
          SCATT(1)=SCATTR(1)
          SCATT(2)=SCATTR(2) 
          CC0= CC0R
          CC1= CC1R
          RKM= 0
        END IF
      END IF



C   SAVING PROPERTIES OF CURRENT ENVIRONMENT AS
C    "LEFT EXTREME OF REGION PROPERTIES"
      RKML=RKMR                                                         
      H0L=H0R                                                           
      H1L=H1R                                                           
      R1L=R1R                                                           
      C2L=C2R                                                           
      R2L=R2R                                                           
      C2SL=C2SR                                                         
      ND0L=ND0R                                                         
      ND1L=ND1R                                                         
                                                                        
      DO 2000    I=1,ND0R                                               
      Z0L(I)=Z0R(I)                                                     
      C0L(I)=C0R(I)                                                     
 2000 CONTINUE                                                          
                                                                        
      DO 2200    I=1,ND1R                                               
      Z1L(I)=Z1R(I)                                                     
      C1L(I)=C1R(I)                                                     
 2200 CONTINUE                                                          
                                                                        
      DO 2400    I=1,3                                                  
      BETAL(I)=BETAR(I)                                                 
 2400 CONTINUE                                                          
                                                                        
      SCATTL(1)=SCATTR(1)                                               
      SCATTL(2)=SCATTR(2)                                               
                                                                        
      CC0L= CC0R
      CC1L= CC1R


C ****************************************************************      
C   READING NEW PROPERTIES AS NEW "RIGHT EXTREME OF REGION PROPERTIES"
C      PRINT *,' ENTERING NEWENV WITH RKM, INPDZ: ', RKM, INPDZ         
                                                                        
      READ(10) RKMR                                                      
      READ(10) R1R, R2R, H0R, H1R, ND0R, ND1R                                 
C      PRINT 975, RKMR,SNGL(H0R),SNGL(H1R),ND0R,ND1R                         
C  975 FORMAT(1X,' NEWLY READ RKM,H0,H1,ND0,ND1 :',3F11.3,2I4)          
      READ(10) (BETAR(J), J= 1, 3), SCATTR, CC0R, CC1R                      
C   SVP IN WATER COLUMN                                                 
      READ(10) (Z0R(J), C0R(J), J= 1, ND0R)                                
C   SVP IN SEDIMENT LAYER                                               
      IF(ND1R.GT.0)   READ(10) (Z1R(J), C1R(J), J= 1, ND1R)                 
C   BOTTOM                                                              
      READ(10) C2R,C2SR                                                   
C ****************************************************************      

                                                                        
C      WRITE(LUPRT,*) ' NEWENV, INPDZ : ',INPDZ                         
C      PRINT *, ' INPDZ : ', INPDZ                                      
                                                                        
                                                                        
C   Adjust the water and the sediment depth                             
      IF( FIXDZ .EQ. 0 )   RETURN                                       
                                                                        
C                                                                       
      IF( (DZH0 + DZH1) .EQ. 0.0 )   RETURN                             
                                                                        
C     H0R and H1R are modified to be                                      
C     integer multiples of DZH0; ( H0R= Z0R(ND0R), H1R= Z1R(ND1R) )           
C     This guarantees that the mode amplitudes at the coupling interface
C     are known at the same depth (orthonormality problem)              
                                                                        
C      PRINT *,' initial h0r, h1r : ', h0r, h1r                             
C      PRINT *,' initial z0r(nd0r) + z1r(nd1r) ', z0r(nd0r) + z1(nd1r)          
      NH0= NINT(Z0R(ND0R)/DZH0)                                           
      HTMP= NH0 * DZH0                                                  
 4000 CONTINUE                                                          
C      WRITE(LUPRT,*) ' NEWMODSET, Z0R(ND0), HTMP ',Z0R(ND0), HTMP        
      IF(HTMP .LT. Z0R(ND0R-1))   THEN                                    
        ND0R= ND0R-1                                                      
        GO TO 4000                                                      
      ELSE IF(HTMP .NE. Z0R(ND0R))   THEN                                 
        C0R(ND0R)= C0R(ND0R-1) + (C0R(ND0R)-C0R(ND0R-1)) *                      
     &           ((HTMP-Z0R(ND0R-1))/(Z0R(ND0R)-Z0R(ND0R-1)))                 
        H0R= HTMP                                                        
        Z0R(ND0R)= HTMP                                                   
      END IF                                                            
                                                                        
      IF(H1R .GT. 0.0)   THEN                                            
C       PRINT *,' REDEF H1R, PREV H1R= ', SNGL(H1R)                        
        NH1= NINT(Z1R(ND1R)/DZH0)                                         
        HTMP= NH1 * DZH0                                                
 4200   CONTINUE                                                        
C        WRITE(LUPRT,*) ' NEWENV, Z1R(ND1R), HTMP ',Z1R(ND1R), HTMP         
        IF(HTMP .LT. Z1R(ND1R-1))   THEN                                  
          ND1R= ND1R-1                                                    
          GO TO 4200                                                    
        ELSE IF(HTMP .NE. Z1R(ND1R))   THEN                               
          C1R(ND1R)= C1R(ND1R-1) + (C1R(ND1R)-C1R(ND1R-1)) *                    
     &             ((HTMP-Z1R(ND1R-1))/(Z1R(ND1R)-Z1R(ND1R-1)))               
          H1R= HTMP                                                      
          Z1R(ND1R)= HTMP                                                 
        END IF                                                          
      END IF                                                            
                                                                        
C      PRINT *, ' FINAL H0R, H1R :',SNGL(H0R), SNGL(H1R)                    
      RETURN                                                            
      END                                                               
