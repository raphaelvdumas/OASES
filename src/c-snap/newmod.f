C   NEWMOD.FOR                                                        
      SUBROUTINE NEWMOD( *, RMARCH,                                   
     &  Z0L,C0L,Z1L,C1L,Z0R,C0R,Z1R,C1R,                                
     &  ND0LR, Z0LR, CE0L, CE0R,                                        
     &  ND1LR, Z1LR, CE1L, CE1R,                                        
     &  Z0,C0,Z1,C1,Z0NRM,Z1NRM,                                        
     &  EK, ISO, MY, MAXMSH, MODEN,
     &  ADA, SPEED, ZZ)
                                                                        
                                                                        
      INTEGER FIXDZ, EXTPOL                                 
                                                                        
                                                                        
                                                                        
      DOUBLE PRECISION H0, H1, H0T, H1T, H0TH1T                         
      DOUBLE PRECISION H0L, H0R, H1L, H1R                               
      DOUBLE PRECISION CC0, CC1, CMIN                                   
      DOUBLE PRECISION RATIOX                                           
      DOUBLE PRECISION Z0(*), C0(*), Z1(*), C1(*),                      
     &                 Z0NRM(*), Z1NRM(*)                               
      DOUBLE PRECISION Z0L(*), C0L(*), Z1L(*), C1L(*)                   
      DOUBLE PRECISION Z0R(*), C0R(*), Z1R(*), C1R(*)                   
      DOUBLE PRECISION Z0LR(*), CE0L(*), CE0R(*)                        
      DOUBLE PRECISION Z1LR(*), CE1L(*), CE1R(*)                        
                                                                        
      DOUBLE PRECISION EK(MODEN)                                        
      DOUBLE PRECISION ADA(*), SPEED(*)
      DOUBLE PRECISION ISO(*), MY(MAXMSH,MODEN)                         
      DOUBLE PRECISION ZZ(*)
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /FLAGG/ PLANE, NOVOL, NOLOSS, NOCYL, LARGE, SUMPL
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /G/ H0, H1                                                 
      COMMON /LEFT/ RKML, BETAL(3), SCATTL(2), R1L, R2L,           
     &              C2L, C2SL, H0L, H1L, ND0L, ND1L
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
      COMMON /MSHIST/ MH0(8), MSED(8), ICOUNT, USEOLD         
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /NA/ ND0, ND1, CMIN                                        
      COMMON /PARAM1/ IRANGE, IFREQ, JUMP, MODQTY            
      COMMON /RIGHT/ RKMR, BETAR(3), SCATTR(2), R1R, R2R, C2R,          
     &               C2SR, H0R, H1R, ND0R, ND1R
      COMMON /RNGPRF/ NSECT, RKM
                                                                        
                                                                        
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE                    
                                                                        
  370 FORMAT(1X,'NEWMOD, RKM,H0,H1,H0+H1', 3(F11.2,1X))              
  400 FORMAT(1X,/,' ***  ENTERING NEW REGION:',F9.3,' - ',F9.3,' km')   
                                                                        
                                                                        
C &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&                
C &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&                
                                                                        

      IF(LARGE .LE. 0.0)   THEN                                         
        FLAGPU= 1.0                                                     
        JUMP= 1                                                         
      END IF                                                            


C      PRINT *,'NEWMOD, rkml,RMARCH, RKMR ',RKML,RMARCH, RKMR        
      IF(RMARCH .GT. RKMR)   THEN                                       
                                                                        
C       Define new region (get new environment) 
      RKM= RMARCH
      CALL NEWENV( Z0, C0, Z1, C1, Z0L, C0L, C1L,
     & Z1L, Z0R, C0R, Z1R, C1R)
C        PRINT 400, RKMR, RKM                                           
                                                                        
C ***********************************************************************
C       WATER COLUMN                                                       
        CALL PROFEQ(ND0L,Z0L,C0L,ND0R,Z0R,C0R,ND0LR,Z0LR,CE0L,CE0R)      
                                                                        
C       SEDIMENT LAYER                                                     
        CALL PROFEQ(ND1L,Z1L,C1L,ND1R,Z1R,C1R,ND1LR,Z1LR,CE1L,CE1R)      
C ***********************************************************************
                                                                        
 
      END IF                                                            
                                                                        
C &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&                
C &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&                
                                                                        
                                                                        
                                                                        
      IRANGE= IRANGE + 1                                                
                                                                        
      RATIOX=(RMARCH - RKML)/ ( RKMR - RKML )                           
      RKM=RMARCH                                                        
                                                                        
      H0T= H0L + RATIOX * (H0R - H0L)                                   
      H1T= H1L + RATIOX * (H1R - H1L)                                   
      H0TH1T= H0T + H1T                                                 
C      PRINT *, ' NEWMOD, H0L,  H1L, H0L+H1L ',                      
C     & SNGL(H0L),SNGL(H1L),SNGL(H0L+H1L)                               
C      PRINT *, ' NEWMOD, H0R,  H1R, H0R+H1R ',                      
C     & SNGL(H0R),SNGL(H1R),SNGL(H0R+H1R)                               
C      PRINT *, ' NEWMOD, H0T, H1T, H0T+H1T ',                       
C     & SNGL(H0T), SNGL(H1T), SNGL(H0TH1T)                              
                                                                        
                                                                        
C      PRINT *,' SUB NEWMOD, RMARCH, RATIOX ',RMARCH,RATIOX           
                                                                        
C     WATER COLUMN                                                      
                                                                        
                                                                        
C***                                                                    
C      WATER DEPTH AND SEDIMENT DEPTH MAY BE ADJUSTED TO BE AN INTEGER M
C      OF THE MODE SAMPLING STEP DEFINED AT THE SOURCE RANGE (DZH0).    
                                                                        
      IF( FIXDZ .EQ. 0 )   THEN                                         
        H0= H0T                                                         
        DZH0= 0.0                                                       
      ELSE                                                              
        NH0= NINT(H0T/DZH0)                                             
        H0= NH0 * DZH0                                                  
      END IF                                                            
                                                                        
C      Z0(ND0)= H0                                                      
C***                                                                    
C       PRINT *,' H0L, H0, H0R ', SNGL(H0L), SNGL(H0), SNGL(H0R)        
C      PRINT *,' DZH0, NH0, H0 ',DZH0,NH0,SNGL(H0)                      
C      PRINT *,' ND0LR ',ND0LR                                          
      CALL PROFIN(ND0,Z0,C0,ND0LR,Z0LR,CE0L,CE0R,RATIOX,H0,CC0)       
                                                                        
C     DENSITY IN THE WATER LAYER                                        
      R0=1.0                                                            
                                                                        
  614 FORMAT(1X,E11.4,1X,3(A16,2X))                                     
                                                                        
                                                                        
C     SEDIMENT LAYER                                                    
C      PRINT *, ' H1L, H1T, H1R ',                                      
C     &              SNGL(H1L),SNGL(H1T),SNGL(H1R)                      
C      PRINT *,' H0R + H1R ', H0R _ H1R                                 
C***                                                                    
                                                                        
      IF( FIXDZ .EQ. 0 )   THEN                                         
        H1= H1T                                                         
        DZH1= 0.0                                                       
      ELSE                                                              
        IF( H1T .GT. 0.0 )   THEN                                       
          H1T= H0TH1T - H0                                              
          IF( H1T .GT. 0.0 )   THEN                                     
            NH1= NINT(H1T/DZH1)                                         
            H1= NH1* DZH1                                               
          END IF                                                        
        END IF                                                          
      END IF                                                            
                                                                        
      IF( H1 .GT. 0.0 )   THEN                                          
C       DENSITY AND ATTENUATION                                         
        IF( (H1L .GT. 0.0) .AND. (H1R .GT. 0.0) )   THEN                
          R1= R1L + RATIOX*(R1R-R1L)                                    
          BETA(1)= BETAL(1) + RATIOX*(BETAR(1)-BETAL(1))                
        ELSE IF(H1R .GT. 0.0)   THEN                                    
          R1= R1R                                                       
          BETA(1)= BETAR(1)                                             
        ELSE IF(H1L .GT. 0.0)   THEN                                    
          R1= R1L                                                       
          BETA(1)= BETAL(1)                                             
        END IF                                                          
C        PRINT *, ' H1L, H1   , H1R ',                                  
C     &  SNGL(H1L), SNGL(H1), SNGL(H1R)                                 
C***                                                                    
        CALL PROFIN(ND1,Z1,C1,ND1LR,Z1LR,CE1L,CE1R,RATIOX,H1,CC1)     
                                                                        
      ELSE                                                              
        H1= 0.0                                                         
        R1= 0.0                                                         
        BETA(1)= 0.0                                                    
      END IF                                                            
C      PRINT *, ' FINAL H0, H1, H0+H1 :         ',                      
C     &  SNGL(H0), SNGL(H1), SNGL(H0+H1)                                
                                                                        
C     COMPRESSIONAL AND SHEAR SPEED IN BOTTOM LAYER                     
      C2= C2L + RATIOX*(C2R-C2L)
      C2S=C2SL + RATIOX*(C2SR-C2SL)                                     
C     CORRECTING FOR POSSIBLE ROUND-OFF ERRORS IN DETERMINING CC1
      IF( H1. GT. 0.)   CC1= MIN(SNGL(CC1),C2)
                                                                        
C     SCATTER COEFF                                                     
      SCATT(1)=SCATTL(1) + RATIOX*(SCATTR(1)-SCATTL(1))                 
      SCATT(2)=SCATTL(2) + RATIOX*(SCATTR(2)-SCATTL(2))                 
                                                                        
C     ATTENUATION COEFFS                                                
      BETA(2)= BETAL(2) + RATIOX*(BETAR(2)-BETAL(2))                    
      BETA(3)= BETAL(3) + RATIOX*(BETAR(3)-BETAL(3))                    
                                                                        
C     DENSITY IN BOTTOM LAYER                                           
      R2= R2L + RATIOX*(R2R-R2L)                                        
                                                                        
             
      MINMOD= 1                                                           
      MAXMOD=MIN(MAXMOD,MODEN-2)                                        
C      WRITE(LUPRT,370) RKM, H0, H1, H0+H1                                  
                                                                        
      ICOUNT=0                                                          
      USEOLD=0                                                         
                                                                        
                                                                        
C      CPOLD= CPEIGV                                                    
                                                                        
      IF(RMARCH .EQ. RKMR)   THEN                                       
        FLAGPU= 0.0                                                     
        JUMP= 0                                                         
      END IF                                                            
                                                                        
      CALL MODE(*9999,                                                  
     &  EK, C0, Z0, C1, Z1, ISO,
     &  MY, MAXMSH, MODEN,
     &  ADA, SPEED,
     &  ZZ, Z0NRM, Z1NRM)                          
                                                                        
                                                                        
C      call orthog( EK )                                                
                                                                        
                                                                        
      IF(LARGE .LE. 0.0)   THEN                                         
        FLAGPU= 1.0                                                     
        JUMP= 1                                                         
      END IF                                                            
                                                                        
                                                                        
C      WRITE(LUPRT,*) ' NEWMOD, TIME FOR MODES AT RANGE: ',              
C     &           RKM, CPEIGV - CPOLD                                   
                                                                        
 4000 CONTINUE                                                          
                                                                        
                                                                        
      RETURN                                                            
                                                                        
 9999 RETURN 1                                                          
                                                                        
      END                                                               
