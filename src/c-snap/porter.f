C  PORTER.FOR
      SUBROUTINE PORTER( FRQ, EK, ADA,
     & SPEED, ISO, MY, C0, Z0, C1, Z1, ZZ, *)
C__________________________________________________________
C                                                          |
C     This routine is acting as main for                   |
C     the routines: ISOINT, STURM, BRENT, NEWTON, CHARAC,  |
C     RICH, SPEED and EIGVEC                               |
C     The subject is to find an approximate solution for   |
C     the eigenvalues of a continuous diff. equation by    |
C     finite difference and extrapolation                  |
C     and to calculate the eigenfunctions and losses.      |
C__________________________________________________________|
C

      INCLUDE 'param.inc'


      INTEGER  EXTPOL, MREF(15)
      INTEGER FIXDZ, OPTMZ



      DOUBLE PRECISION CREF, CC0, CC1, CMIN, H0, H1, ROB, ROS
      DOUBLE PRECISION H1N
      DOUBLE PRECISION ZZ( * )
      DOUBLE PRECISION MY(MAXMSH, MODEN ), ISO( * ), ADA( * )
      DOUBLE PRECISION DH0(8), DSED(8), DH0SQ(8)
      DOUBLE PRECISION SPEED( * )
      DOUBLE PRECISION FRQ, TWOPI, PI, OMEGA, DRAT
      DOUBLE PRECISION EIGREF, EIGMIN, EIGMAX, STIFF
      DOUBLE PRECISION EK(MODEN)
      DOUBLE PRECISION CON1, CON2, CON3, CON4, CON5, SEDK
      DOUBLE PRECISION C0( * ), Z0( * ), C1( * ), Z1( * )


      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2
      COMMON /APM/ NSMPL, NSMDEF
      COMMON /ATTEN/ ALF0, ALF1, ALF2, ALF2S, ALFOS, ALFOB
      COMMON /CONST/ CON1, CON2, CON3, CON4, CON5, SEDK
      COMMON /DENS/ R0, R1, R2
      COMMON /DENS8/ ROB, ROS
      COMMON /EXPMAX/ TRESH, EPS, RRMAX, EPSDEF
      COMMON /FACTS/ FACT0, FACT1, FLAGF0, FLAGF1
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /FRQDEP/ FRQREF, FPOW, FRQDEP
      COMMON /G/ H0, H1
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF
      COMMON /ITMAX/ MAXIT
      COMMON /LUIN/ LUIN, LUWRN, LUCHK
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
      COMMON /MSHIST/ MH0(8), MSED(8), ICOUNT, USEOLD
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /NA/ ND0, ND1, CMIN
      COMMON /PARAM3/ IMESH, NMESH, MSHRAT, OPTMZ
      COMMON /PARAM4/ MESHI, MESHN
      COMMON /REFSPD/ CREF
      COMMON /TRIGON/ TWOPI, PI, OMEGA
      COMMON /XREFL/ CINTFC, RINTFC


      INCLUDE 'acommon.inc'


      DATA MREF/16,20,25,32,40,50,64,80,100,128,160,200,256,320,400/
C      DATA MREF/32,40,50,64,80,100,128,160,200,256,320,400/
C      DATA MREF/50,64,80,100,128,160,200,256,320,400,512,640/

  300 FORMAT(1X,/)
  360 FORMAT(1X,//,' ***  WARNING : DUE TO ARRAY SIZE LIMITATIONS,',/,
     & 4X,'  THE NUMBER OF DIFFERENT MESHES IS REDUCED TO    ',I5,/,
     & 4X,'  THE MODE AMPLITUDES ARE MADE AVAILABLE ON MESH #',I5,/,
     & 4X,'  WITH ',I5,' MESH POINTS IN WATER AND ',I5,
     &    ' IN SEDIMENT',/)
  370 FORMAT(1X,//,' ***  WARNING : DUE TO ARRAY SIZE LIMITATIONS,',/,
     & ' THE NUMBER OF MESHES IS REDUCED TO 1 AND THE NUMBER',/,
     & ' OF MESH POINTS TO',I6,' (WATER) AND',I6,' (SEDIMENT)')
  420 FORMAT(1X,' FINDING APPROXIMATION (BRENT ) FOR MESH ',I2,1X,
     & '( ',I5,', ',I5,' )',1X, I4,' mode(s)' )
  440 FORMAT(1X,/,1H0,'CALCULATION MESHES',
     &      /1H ,'STEP   WATER   SEDIMENT',
     &     (/1H ,I3,I9,I9))
  460 FORMAT(1X,//,' EXTRAPOLATED WAVENUMBER(S) : ',/,
     & '    MODE         WAVENUMBER    ')
  480 FORMAT(1X,I6,6X,D17.10)



      FLAGNP= 0
      CREF= CC0
      EIGREF= OMEGA*H0/CREF
      MAXIT= 10

C
      MESHN= NMESH
      MESHI= IMESH
      H1N=H1/H0
      IF(H1N.LT.1.0D-6)    THEN
        ROS= 1.0D0
        STIFF= 1.0D0
      ELSE
        ROS= DBLE(R1)/DBLE(R0)
        STIFF= (C0(ND0)/C1(1))**2/ROS                                    
      END IF                                                            
      ROB= DBLE(R2)/DBLE(R0)
      DRAT= H0/(H0+H1)
C
C     INITIALIZE REFLECTION COEFFICIENT CALCULATION
C
      IF (H1N.GT.0.0) THEN
        CINTFC=C1(ND1)
        RINTFC=R1
      ELSE
        CINTFC=C0(ND0)
        RINTFC=R0
      END IF
      IF (C2S.GT.0.0) THEN
        CALL REFL1(C2,C2S,FRQDEP*BETA(2),BETA(3))
      END IF


C     DEFINITION OF MESHES


      IREF= 1
      CALL MESHES( MREF, IREF, FRQ,
     &             NPH0, MRH0,
     &             NPSED, MRSED )

      IF(H1 .LE. 0.0)   MRSED= 0
                                                                        
      ISHIFT=0                                                          
      IF( (USEOLD .GT. 0.0) .AND. (ICOUNT .GT. 2) )   THEN             
        ICOUNT= ICOUNT-2                                                
        ISHIFT= ICOUNT                                                  
      ELSE                                                              
        ISHIFT=0                                                        
        ICOUNT=0                                                        
      END IF                                                            
                                                                        
C     IF((MSHRAT .EQ. 2) .OR. (MESHI .EQ. 1))   THEN                    
      IF( MSHRAT .EQ. 2 )   THEN                                        
        MH0(1)= NPH0                                                    
        DO 1200 I=2, MESHN                                                
C        MH0(I)= 2 * MH0(I-1)                                           
        MH0(I)= I * NPH0                                                
 1200   CONTINUE                                                        
        MSED(1)= NPSED                                                  
        DO 1220 I=2, MESHN                                                
C        MSED(I)= 2 * MSED(I-1)                                         
        MSED(I)= I * NPSED                                              
 1220   CONTINUE                                                        
                                                                        
      ELSE                                                              
                                                                        
        DO 1300 I=1, MESHN                                                
        MH0(I)= MREF(I+ISHIFT)*MRH0                                     
 1300   CONTINUE                                                        
        DO 1320 I=1, MESHN                                                
        MSED(I)= MREF(I+ ISHIFT)*MRSED                                  
 1320   CONTINUE                                                        
      END IF                                                            
                                                                        
                                                                        
      IF(FLAGF0 .GT. 0)    THEN                                         
        EPS=1.0                                                         
        WRITE(LUPRT,*) ' GIVE NUMBER OF MESHES : '                      
        READ(5,*) MESHN                                                  
        DO 1340    I= 1, MESHN                                           
        WRITE(LUPRT,*) ' GIVE POINTS (WATER,SEDIMENT) IN MESH  ',I      
        READ(5,*) MH0(I), MSED(I)                                       
 1340   CONTINUE                                                        
        MAXIT= MESHN                                                      
      END IF                                                            
      IF(USEOLD .EQ. 0.0)   ICOUNT=0                                   
                                                                        
      GO TO 1600                                                        
                                                                        
 1500 CONTINUE                                                          
      WRITE(LUPRT,*) ' RESTARTING COMPUTATION FROM MESH ',I                 
      ICOUNT=ICOUNT+I-1                                                 
      DO 1520   I=1, MESHN-1                                              
      MH0(I)=MH0(I+1)                                                   
      MSED(I)=MSED(I+1)                                                 
 1520 CONTINUE                                                          
C     IF((MSHRAT .EQ. 2) .OR. (MESHI .EQ. 1))   THEN                    
      IF( MSHRAT .EQ. 2 )   THEN                                        
C        MH0(MESHN)=MH0(MESHN-1)*2                                        
C        MSED(MESHN)=MSED(MESHN-1)*2                                      
        MH0(MESHN)= MESHN * NPH0                                          
        MSED(MESHN)= MESHN * NPSED                                        
      ELSE                                                              
        MH0(MESHN)=MH0(MESHN-3)*2                                         
        MSED(MESHN)=MSED(MESHN-3)*2                                       
      END IF                                                            
 1600 CONTINUE                                                          
      NPMAX=MH0(MESHN)+MSED(MESHN)                                        
      IF(NPMAX .GT. NPOINT-2)   THEN
        FLAGNP=1  
        MESHN=MESHN-1                                                     
        MESHI=MIN(MESHI,MESHN)
        MESHI= MAX(1,MESHI)
        IF(MESHN .GE. 1)  GO TO 1600
        MESHN=1                                                          
C        PRINT *,' NPOINT, MH0(1), MSED(1) ',
C     &  NPOINT-2, MH0(1), MSED(1)
C        PRINT *,' H0, H1 : ',H0, H1
        TEMP= 0.5 * ( NPOINT - 2) / ( MH0(1) + MSED(1) )
        MH0(1)= MH0(1) * TEMP
        MSED(1)= MSED(1) * TEMP
        MH0(1)=MH0(1)*2                                                 
        MSED(1)=MSED(1)*2                                               
        EXTPOL=0                                                        
        WRITE(LUPRT,370) MH0(1), MSED(1)                                    
      END IF                                                            
      IF(FLAGNP .GT. 0)   THEN
        WRITE(LUPRT,360) MESHN,  MESHI, MH0(MESHI), MSED(MESHI)
        PRINT 360, MESHN, MESHI, MH0(MESHI), MSED(MESHI)
      END IF
      IF(MESHN .GE. 2)   THEN                                            
        USEOLD=1.0                                                     
      ELSE                                                              
        USEOLD=0.                                                      
      END IF                                                            
C                                                                       
                                                                        
      IF(FLAGPU .LT. 1.)   THEN                                         
        WRITE(LUPRT,440) (JJ+ICOUNT,MH0(JJ),MSED(JJ),JJ=1,MESHN)         
        WRITE(LUPRT,300)                                                
      END IF                                                            
C                                                                       
                                                                        
      DO 1640 I= 1, MESHN
      DH0(I)= 1.0D0/MH0(I)
      DH0SQ(I)= 1.0D0/(DBLE(MH0(I))**2)
      IF(H1N.NE.0.0)   THEN
        DSED(I)= H1N/MSED(I)
      END IF
 1640 CONTINUE
                                                                        
      I=1                                                               
 1800 CONTINUE                                                          
                                                                        
C      IF(H1.GT.0.)   SEDK=((OMEGA*H1)/(C1(1)*MSED(I)))**2              
CCHK      IF(FIRST.NE.0.)  WRITE(LUCHK,420) I+ICOUNT,MH0(I),MSED(I),MODQTY     
      IF(FLAGPU.LT.1.0 .AND. FIRST.NE.0.0)                                
     & WRITE(LUPRT,420) I+ICOUNT,MH0(I),MSED(I),MODQTY                  
                                                                        
C*********************************************************              
                                                                        
      CALL COMPUT( *1500, *1800, *7000, *9999,                                
     & I, H1N, DRAT, DH0, DSED, DH0SQ,
     & FRQ, MODQTY,
     & ADA, SPEED, ISO, MY, C0, Z0, C1, Z1,                   
     & ZZ, EK )
                                                                        
C*************************************************                      
                                                                        
      MQTOLD= AMQTY(ARIGHT)                                             
      IF( MQTOLD .NE. MODQTY)   THEN                                    
        WRITE(LUPRT,*) ' *** WARNING : '                                
        WRITE(LUPRT,*) ' *** MODE QUANTITY ON OUTPUT MESH : ',MQTOLD    
        WRITE(LUPRT,*) ' *** MODE QUANTITY ON LAST   MESH : ',MODQTY    
C        WRITE( MODFIL, REC= 5) MODQTY, LRECL                           
      END IF                                                            
                                                                        
C *********************************************************             
      AMQTY(ARIGHT)= MODQTY

      AC0(ARIGHT)= C0(ND0)
      IF( ND1 .GT. 0 )   THEN
        AC1(ARIGHT)= C1(1)
      ELSE
        AC1(ARIGHT)= C2
      END IF
                                                                        
 7000 CONTINUE                                                          
                                                                        
      RETURN                                                            
 9999 RETURN 1

      END                                                               
