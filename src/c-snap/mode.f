C  MODE.FOR
C **********************************************************************
      SUBROUTINE MODE(*,
     & EK, C0, Z0, C1, Z1, ISO,
     & MY, MAXMSH, MODEN,
     & ADA, SPEED, ZZ, Z0NRM, Z1NRM )


      INTEGER EXTPOL
      INTEGER ALEFT, ARIGHT, CURRNT
      INTEGER FIXDZ, OPTMZ
      REAL ETIME, CPSEC(2)
      REAL CPUBEG, CPUEND, CPEIGV, CPEIGF, CPNEWP, CPFILE

C      CHARACTER*8 MODEL


      DOUBLE PRECISION CC0, CC1, CMIN, H0, H1, ROB, ROS
      DOUBLE PRECISION TWOPI, PI, FRQ, OMEGA
      DOUBLE PRECISION EK(MODEN)
      DOUBLE PRECISION ADA( * ), SPEED( * )
      DOUBLE PRECISION ISO(MODEN), MY(MAXMSH,MODEN)
      DOUBLE PRECISION ZZ( * )
      DOUBLE PRECISION Z0NRM(*), Z1NRM(*)
      DOUBLE PRECISION C0(*), C1(*), Z0(*), Z1(*)


      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2
      COMMON /APM/ NSMPL, NSMDEF
      COMMON /DENS/ R0, R1, R2
      COMMON /DENS8/ ROB, ROS
      COMMON /FACTS/ FACT0, FACT1, FLAGF0, FLAGF1
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /FRQDEP/ FRQREF, FPOW, FRQDEP
      COMMON /G/ H0, H1
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
C      COMMON /MODEL/ MODEL
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /NA/ ND0, ND1, CMIN
      COMMON /PARA4/ NMAM2, NPAM2
      COMMON /PARAM1/ ISECT, IFREQ, JUMP, MODQTY
      COMMON /PARAM2/ FRQ, EPSINP
      COMMON /PARAM3/ IMESH, NMESH, MSHRAT, OPTMZ
      COMMON /POSITN/ ALEFT, ARIGHT, CURRNT
      COMMON /RNGPRF/ NSECT, RKM
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE
      COMMON /TRIGON/ TWOPI, PI, OMEGA

  230 FORMAT(1H1 ,/,'  FREQUENCY NO. ',I3,/,'  ****************',///)
  240 FORMAT(1H ,' SAMPLE POINTS/MODE:',12X,'=',I6,//)
  242 FORMAT(1H ,' SAMPLING STEP IN DEPTH (m):',/,
     &'  WATER',11X,'=',F8.2,/,'  SEDIMENT',8X,'=',F8.2,//)
  250 FORMAT(1H ,' DEPTHS (m):',/,'  WATER',11X,'=',F8.2,
     & /,'  SEDIMENT',8X,'=',F8.2,//)
  260 FORMAT(1H ,' DENSITIES (g/cm**3):',/,'  WATER',11X,'=',F8.2,
     & /,'  SEDIMENT',8X,'=',F8.2,/,'  SUBBOTTOM',7X,'=',F8.2,//)
  261 FORMAT(1H ,' DENSITIES (g/cm**3):',/,'  WATER',11X,'=',1PE13.5,/
     & ,'  SEDIMENT',8X,'=',1PE13.5,/,'  SUBBOTTOM',7X,'=',1PE13.5,//)  
  270 FORMAT(3X,F8.2,4X,F8.2)                                           
  280 FORMAT(1H ,/ /,5X,'SOUND SPEED PROFILE',//,11X,' WATER     ',//,
     & '    DEPTH (m)  SPEED (m/s)  ')                                     
  290 FORMAT(1H ,///,'          SEDIMENT ',                             
     & //,'    DEPTH (m)  SPEED (m/s)')                                  
  301 FORMAT(1X,/,'  FILE ',A20,' ALREADY EXISTS ',/)                   
  400 FORMAT(1X ,///,'  WARNING: ',/,'  THIS "C-SNAP" VERSION IS',       
     & '  CURRENTLY RUNNING A RANGE INDEPENDENT PROBLEM.',/,            
     & '  IN THIS CASE IT ALLOWS FOR A MAXIMUM OF ',I4,' MODES',/,      
     & '  COMPUTED OVER A MAXIMUM OF ',I5,' MESH POINTS',//)            
  420 FORMAT(1X ,///,'  WARNING: ',/,'  THIS "C-SNAP" VERSION IS',       
     & '  CURRENTLY RUNNING A RANGE DEPENDENT PROBLEM.',/,              
     & '  IN THIS CASE IT ALLOWS FOR A MAXIMUM OF ',I4,' MODES',/,      
     & '  COMPUTED OVER A MAXIMUM OF ',I5,' MESH POINTS',//)            
  600 FORMAT(1H ,//,' BOTTOM SOUND SPEED   =',F8.2,' m/s',/             
     & ,' SHEAR SPEED',10X,'=',F8.2,' m/s',/)                            
  601 FORMAT(1H ,//,' BOTTOM SOUND SPEED   =',1PE13.5,' m/s',/          
     & ,' SHEAR SPEED',10X,'=',1PE13.5,' m/s',/)                         
  700 FORMAT(1H ,' ATTENUATION COEFFICIENTS (dB/WL): ',                 
     &        /,'  SEDIMENT',8X,'=',F8.2,/,'  SUBBOTTOM',7X,'=',F8.2,   
     & /,'  SHEAR  ',9X,'=',F8.2,///,'  RMS ROUGHNESSES (m):',5X,/,     
     & '  SEA SURFACE',5X,'=',F8.2,/,'  SEA FLOOR',7X,'=',F8.2,////)    
  701 FORMAT(1H ,' ATTENUATION COEFFICIENTS (dB/WL): ',                 
     &     /,'  SEDIMENT',8X,'=',1PE13.5,/,'  SUBBOTTOM',7X,'=',1PE13.5,
     & /,'  SHEAR  ',9X,'=',1PE13.5,///,'  RMS ROUGHNESSES (m):',5X,/,  
     & '  SEA SURFACE',5X,'=',1PE13.5,/,'  SEA FLOOR',7X,'=',1PE13.5,   
     & ////)                                                            
C  830 FORMAT(1H ,///,'  ***  Computing modes for profile # ',I5,/,      
C     & '  Range           ',F9.3,' km',/,                               
C     & '  Water depth    ',f9.2,'  m',/,                                
C     & '  Sediment depth ',f9.2,'  m',/)                                
 830  FORMAT(1H ,///,'  ***  Profile #',I5,' - Range',F9.3,'km',
     & ' - H0',f9.2,'m',' -  H1',f9.2,'m')                                
                                                                        
      CPUBEG= ETIME(CPSEC)                                        
                                                                        
      MAXMOD=MIN(MAXMOD,MODEN-2)                                        
                                                                        
      IF(FLAGP.LT.0.0)   THEN                                           
        WRITE(LUPRT,400) NMAM2, NPAM2                                       
        FLAGP=1.0                                                       
      END IF                                                            
                                                                        
      OMEGA=TWOPI*FRQ                                                   
                                                                        
C                                                                       
C   INPUT PARAMETERS WERE READ FROM FILE 10                             
C                                                                       
                                                                        
C   R0 IS DENSITY OF  WATER LAYER.                                      
      R0=1.                                                             
      IF(EPSINP.LT.0.0)   GO TO 1100                                    
      IF( (FLAGPU .LT. 1.)  .AND. (FIRST .EQ. 0.0 ) ) 
     &  WRITE(LUPRT,230) IFREQ                           
 1100 CONTINUE
                                                                        
       FRQDEP= 1
       IF( FRQ .LE. FRQREF )   FRQDEP= ( FRQ/FRQREF)**FPOW

      BETA(0)= BETA(-1) * 1.0D-3 * CC0/FRQ                              
COLD  BETA(0)= FRQ*1.0E-9*(.007+.2635/(2.89+(FRQ*.001)**2))*CC0         
      IF(JUMP .GT. 0)   GO TO 1200                                      
        IF(INPDZ .EQ. 0)   THEN                                         
          WRITE(LUPRT,240) NSMPL                                            
        ELSE                                                            
          DZSED= MIN( DZH1, SNGL(H1) )                                  
          WRITE(LUPRT,242) DZH0, DZSED                                      
        END IF                                                          
        WRITE(LUPRT,250) H0,H1                                              
        IF( MAX(R0,R1,R2) .LE. 99999.99 )   THEN                        
         WRITE(LUPRT,260) R0,R1,R2                                          
        ELSE                                                            
         WRITE(LUPRT,261) R0,R1,R2                                          
        END IF                                                          
                                                                        
        IF( MAX(BETA(1),BETA(2),BETA(3),SCATT(1),SCATT(2)) .LE.         
     &   99999.99 )   THEN                                              
         WRITE(LUPRT,700) FRQDEP*BETA(1), BETA(2), BETA(3),
     &                    (SCATT(J), J=1, 2)
        ELSE                                                            
         WRITE(LUPRT,701) FRQDEP*BETA(1), BETA(2), BETA(3),
     &                    (SCATT(J), J=1, 2)
        END IF                                                          
 1200 CONTINUE                                                          
                                                                        
C   MIN SOUND SPEED                                                     
      CMIN=1.0D38                                                       
      DO 1260   I=1,ND0                                                 
      CMIN=MIN(CMIN,C0(I))                                              
 1260 CONTINUE                                                          
      CORREC= -1                                                        
      IF(H1.GT.0.0)   THEN                                              
        DO 1300   I=1,ND1                                               
        CMIN=MIN(CMIN,C1(I))                                            
 1300   CONTINUE                                                        
      ELSE                                                              
                                                                        
C**C  N.B. This correction perturbs the accuracy of the mode coupling   
C**C       when the contribution from the mode tail is computed.        
C**C       It may be useful only for range independent calculations.    
C  REMOVED ON 26 JAN '93 : MORE PROBLEMS THAN ADVANTAGES                
C        IF(MODEL(1:6) .EQ. 'C-SNAP')   CORREC= 1.0                     
                                                                        
      END IF                                                            
                                                                        
      IF(FLAGF1 .GT. 0.)   CORREC=-1.                                   
                                                                        
C   BOTTOM                                                              
      IF(JUMP.GT.0)   GO TO 1400                                        
      WRITE(LUPRT,280)                                                      
      WRITE(LUPRT,270) (Z0(I),C0(I),I=1,ND0)                                
      IF(H1.LE.0.0)   GO TO 1350                                        
      WRITE(LUPRT,290)                                                      
      WRITE(LUPRT,270) (Z1(I),C1(I),I=1,ND1)                                
 1350 CONTINUE                                                          
      IF( MAX(C2,C2S) .LE. 99999.99 )   THEN                            
       WRITE(LUPRT,600) C2,C2S                                              
      ELSE                                                              
       WRITE(LUPRT,601) C2,C2S                                              
      END IF                                                            
 1400 CONTINUE                                                          
                                                                        
                                                                        
C     NORMALIZATION OF DEPTHS                                           
                                                                        
      DO 1450   I=1,ND0                                                 
      Z0NRM(I)=Z0(I)/H0                                                 
 1450 CONTINUE                                                          
      DO 1500   I=1,ND1                                                 
      Z1NRM(I)=(H0+Z1(I))/H0                                            
 1500 CONTINUE                                                          
                                                                        
      IF(FLAGPU .LT. 1)   WRITE(LUPRT,830) ISECT, RKM, H0, H1
                                                                        
C ******************************************************************** 
      ITEMP= ALEFT                                                     
      ALEFT= ARIGHT                                                    
      ARIGHT= ITEMP                                                    
C ******************************************************************** 
                                                                        
                                                                        
      CALL PORTER(FRQ, EK, ADA, SPEED,                           
     &  ISO, MY, C0, Z0NRM, C1, Z1NRM, ZZ, *9999)

      MODQTY=MAXMOD-MINMOD+1                                            
      CPUEND= ETIME(CPSEC)                                        
      CPEIGV= CPEIGV + (CPUEND - CPUBEG)                                
      RETURN
 9999 RETURN 1
      END
