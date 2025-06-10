C   START.F
      SUBROUTINE START( IPROF, SD, RD, NSD, NRD, OPT,
     &                  CK, CKTAIL, EK, PHIS, PHIR, M,
     &                  ML, FREQ,
     &                  A, SUM2A, CPL, CPR, DELTAR,
     &                  ALFA, MODAVR,
     &                  ADA, SPEED, EIGF,
     &                  A3, B3, C3, EE, ZZ, SSOLD, EXCH)
                                                                        
C         ARRAY CPR : ANALYTICAL GAUSSIAN SOURCE FIELD
C         ARRAY PR0 : GAUSSIAN SOURCE FIELD AFTER MODAL DECOMPOSITION
C                     OF CPR AND SUBSEQUENT RECOMPOSITION.
C         ARRAY CPL : PRESSURE ALONG FIRST INTERFACE

      PARAMETER ( MAXMED= 50 )

      INCLUDE 'param.inc'
      INCLUDE 'acommon.inc'
                                                                        
      LOGICAL EXCH(*)

      INTEGER ISD( MAXNSD), IRD( MAXNRD )

      REAL ALFA(*), MODAVR(*)
      REAL WSD( MAXNSD ), WRD( MAXNRD )
      REAL      RD(*), SUM2A(*)
      REAL PHIR(MODEN,*)

C      DOUBLE PRECISION SQ2PI, SQ2PIK
      DOUBLE PRECISION DZH0, DZH1
      DOUBLE PRECISION CC0, CC1
      DOUBLE PRECISION TWOPI, PI, OMEGA
      DOUBLE PRECISION DH0I, DSEDI
      DOUBLE PRECISION EK(*)
      DOUBLE PRECISION ADA(*), SPEED(*), EIGF(*)
      DOUBLE PRECISION A3(*), B3(*), C3(*)
      DOUBLE PRECISION EE(*), ZZ(*), SSOLD(*)
      DOUBLE PRECISION KVAL, FRQ
                                                                        
      COMPLEX*8 SUMH0, SUMH1, SUMTOT
      COMPLEX CPL(*), CPR(*)
      COMPLEX   PHIS(*), PHIT( MAXNRD ), A(*),
     &          CK(*), CKTAIL(*)
      COMPLEX KTOP2, KBOT2
                                                                        
      CHARACTER MATER( MAXMED )*8
      CHARACTER  BCBOT * 1, BCTOP * 1
      CHARACTER*4 OPT
      CHARACTER*80 TITLE
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2
      COMMON /ACOEFF/ COLEFT, NSTART
      COMMON /CGAUSS/ GAUSS, TH1, TILT
      COMMON /FLAGG/ PLANE, NOVOL, NOLOSS, NOCYL, LARGE, SUMPL
      COMMON /FLAGS/ EK0, SQEK0
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /REC1/ TITLE
      COMMON /STARTR/ MODST
      COMMON /TRIGON/ TWOPI, PI, OMEGA


  100 FORMAT(1H ,I8.8,'.DAT    ')
  200 FORMAT(1X, 'Range ',F10.1,'m -  Profile',I4,' read ',F10.1,
     & 'm -   # modes=', I4)
  300 FORMAT(1X,/,'  WARNING: THE SOURCE IS ASSUMED TO BE IN THE',
     & ' SEDIMENT LAYER ',/,10X,' WITH DENSITY ',F8.2,' g/cm3 .',/ )
  310 FORMAT(1X,/,'  WARNING: THE SOURCE IS ASSUMED TO BE IN THE',
     & ' SEDIMENT LAYER ',/,10X,' WITH DENSITY ',E10.2,' g/cm3 .',/ )
  400 FORMAT(1X,/,'  WARNING: GAUSSIAN BEAM,',
     & ' HALF_BEAM AND TILT (deg): ', F10.2,2X,F10.2)
  500 FORMAT(1X,/,' *** WARNING : MODAL STARTING FIELD SAVED IN',
     & ' FILE modst.mod .',/)
  600 FORMAT(1X, A72)



      CURRNT= ARIGHT                                                    

      IF( COLEFT .EQ. 0.0 )   THEN                                          
C       Initial definition of Source Field ( IPROF = 1 )                  
                                                                        
        CALL GETHDR( SD, NSD, RD, NRD,                    
     &   WSD, ISD, WRD, IRD,                                            
     &   ZL, ML, FREQ,
     &   NL, NMAT, MATER, NMEDIA,
     &   KTOP2, DEPTHT, BCTOP,                                          
     &   KBOT2, DEPTHB, BCBOT)
                                                                        
                                                                        
        M= MIN( M, ML )                                                  
        DEPTTL= 0.0                                                      
        DEPTBL= AH0H1(CURRNT)                                        

        IF( SD .LE. AH0( CURRNT ) )   THEN
          DENS= 1.0
        ELSE
          DENS= AR1( CURRNT )
          IF (DENS .LE. 1000 )   THEN
            PRINT 300, DENS
            WRITE(LUPRT,300) DENS
          ELSE
            PRINT 310, DENS
            WRITE(LUPRT,310) DENS
          END IF
        END IF

        DO   IZ= 1, NL                                                
          CPL( IZ )= 0.0
        END DO                                                          

        SQ2PIK= SQRT(TWOPI*EK0)
        SQ2PI= SQRT(TWOPI)
        FRQ= DBLE( FREQ )                                                 
        MH0I= AMH0(CURRNT) - 1                                          
        MSEDI= AMSED(CURRNT)                                            
        DH0I= ADH0(CURRNT)
        DSEDI= ADSED(CURRNT)
        DZH0= ADH0(CURRNT)*AH0(CURRNT)
        IF(MSEDI .GT. 0)   DZH1= ADSED(CURRNT)*AH0(CURRNT)
        IF( SD .LE. AH0(CURRNT) )   THEN
          IND= NINT(SD/DZH0) + 1
        ELSE
          IND= AMH0(CURRNT) + (SD-AH0(CURRNT))/DZH1
        END IF


        IF( GAUSS .GT. 0.0 )   THEN
C         ****************************************************
C         ****************************************************
          ICURVE= 0
          PRINT 400, TH1,TILT
          WRITE(LUPRT,400) TH1,TILT
          AVGK= (TWOPI*FRQ)/CC0
C         PLT IS A FLAG TO TRIGGER THE PLOTTING OF THE SOURCE FIELD
          PLT= 0
          CALL SOURCE(SD, ICURVE, TH1, TILT, AVGK, FREQ, AH0(CURRNT),
     &    DZH0, DZH1, AMH0(CURRNT), MSEDI, CPR, ZZ, SSOLD, PLT)
C         NOTE: ARRAYS ZZ AND SSOLD ARE PASSED TO SUB SOURCE
C         AS SCRATCH WORKING SPACE

          DO   IZ= 1, NL
            PR0(IZ)= 0.0
          END DO
C         ****************************************************
C         ****************************************************
        END IF
 
        EIGF( 1 )= 0.0
                                                                        
C       *** Compute the mode alplitudes ***
                                                                        
        DO 5000 MODE= 1, M                                               
                                                                        
        CALL EIGVEC( MODE, MH0I, MSEDI, DH0I, DSEDI,                     
     &               FRQ, ALFA(MODE),                                     
     &               MINMOD, MODAVR,                                      
     &               ADA, SPEED, EIGF(2),                                 
     &               A3, B3, C3, EE, ZZ, SSOLD, EXCH,                     
     &               KVAL, EIGVL(MODE), EK(MODE) )
                                                                        
        IF(NOLOSS .GT. 0) ALFA(MODE)= 0.0                                
        CKTAIL(MODE)= CMPLX(SNGL(KVAL), -ALFA(MODE))                      
        CK(MODE)= CMPLX(SNGL(EK(MODE)), -ALFA(MODE))                     
                                                                        
        CALL GETONE( MODE, NL, WSD, ISD, WRD, IRD,           
     &     MATER, NMEDIA, EIGF,                                
     &     KTOP2, DEPTHT, BCTOP,                                       
     &     KBOT2, DEPTHB, BCBOT,                                       
     &     SD, NSD, RD, NRD, CK, CKTAIL, PHIT, PHIS )                  
                                                                        
        DO   IZ= 1, NRD                                            
          PHIR( MODE, IZ )= real(PHIT( IZ ))
        END DO                                                       
                                                                        

        IF( GAUSS .GT. 0.0 )   THEN
C         ****************************************************
C         ****************************************************
          SUMH0= 0.0
          DO   J= 2, AMH0(CURRNT)-1
            SUMH0= SUMH0 + CPR(J) * EIGF(J)
          END DO
          SUMH0= SUMH0 + 0.5 * EIGF(AMH0(CURRNT))*CPR(AMH0(CURRNT))
          SUMH0= SUMH0*DZH0

          SUMH1= 0.0
          IF( MSEDI .GT. 0 )   THEN
            SUMH1= 0.5 * EIGF(AMH0(CURRNT))*CPR(AMH0(CURRNT))
            DO   J= AMH0(CURRNT) + 1, NL-1
              SUMH1= SUMH1 + CPR(J) * EIGF(J)
            END DO
            SUMH1= SUMH1 + 0.5 * EIGF(NL)*CPR(NL)
            SUMH1= SUMH1*DZH1/AR1(CURRNT)
          END IF

          SUMTOT= (SUMH0 + SUMH1)
c warning :  verify proper handling of density at source depth
c 20/01/1998 fmc
          A(MODE)= SUMTOT

C         ****************************************************
C         ****************************************************

        ELSE                                                                        

          IF ( OPT(1:1) .EQ. 'X' ) THEN
C           ------ 'X' Plane geometry
C           A( MODE )= SQRT( TWOPI ) * PHIS( MODE ) / CK( MODE )        
            A( MODE )= SQ2PIK * PHIS( MODE ) / CK( MODE )
          ELSE                                                            
C           ------ 'R' Cylindrical coordinates                          
            A( MODE )= SQ2PI * PHIS( MODE ) / SQRT( CK( MODE ) )
          ENDIF
          A( MODE )= A( MODE ) / DENS
          SUMTOT= A(MODE)
                                                                        
        END IF



        IF( (GAUSS .GT. 0) .OR. (MODST .GT. 0) )  THEN
C         Creation of modal source field by summing up modal
C         contributions
          DO   IZ= 2, NL
            PR0( IZ )= PR0( IZ ) + SUMTOT * EIGF( IZ )
          END DO
        END IF

C       CREATION OF PRESSURE ALONG FIRST INTERFACE                       
        CALL FIRSTP( DELTAR, NL, MODE, EIGF,                            
     &              A, CK, CKTAIL, CPL,                                
     &              BCTOP, KTOP2,                                 
     &              BCBOT, KBOT2 )
                                                                        
 5000   CONTINUE                                                          


        IF( GAUSS .GT. 0.0 )   THEN
C         ****************************************************
C         ****************************************************
c         write(luprt,*) ' field at range 0 and rd= 1000m'
          fld= -20*ALOG10( ABS(PR0(IND)) )
          fldIN= -20*alog10(ABS(CPR(IND)))
cfmc          IF( IND .GT.  AMH0(CURRNT) ) 
cfmc     &    PRINT *, ' WARNING, IND IN SEDIMENT !!!!!!'
cfmc          WRITE(88,*)' INITIAL FIELD at rd=SD    :',SD,'m ,',fldIN,'dB'
cfmc          WRITE(88,*)' RECOMPOSED FIELD at rd=SD :',SD,'m ,',fld,'dB'
          PRINT *,' INITIAL FIELD at rd=SD    :',SD,'m ,',fldIN,'dB'
          PRINT *,' RECOMPOSED FIELD at rd=SD :',SD,'m ,',fld,'dB'
          WRITE(LUPRT,*)   
     &    ' INITIAL FIELD at rd=SD    :',SD,'m ,',fldIN,'dB'
          WRITE(LUPRT,*)
     &    ' RECOMPOSED FIELD at rd=SD :',SD,'m ,',fld,'dB'

C         ****************************************************
C         ****************************************************
        END IF



        IF( MODST .GT. 0 )   THEN
          OPEN(UNIT= 92, FILE= 'modst.mod', STATUS= 'UNKNOWN',
     &         FORM= 'FORMATTED')
          WRITE(92,600) TITLE(1:72)
          WRITE(92,*) ' Frequency (Hz)   :', FREQ
          WRITE(92,*) ' Source depth (m) :', SD
          WRITE(92,*) ' NUMBER OF MODES IN THE PROBLEM :', M
          WRITE(92,*) ' Water depth (m)    :', AH0(CURRNT)
          WRITE(92,*) ' Sediment depth (m) :', AH1(CURRNT)
          WRITE(92,*) ' Total depth (m)    :', DEPTHB
          Write(92,*) ' Number of depth points (water + sed ) :', NL
          WRITE(92,*) ' DEPTH AND PRESSURE TABLE :'

          ZERO= 0.0
          WRITE(92,*) ZERO, PR0(1)
          DO   IZ= 2, AMH0(CURRNT)            
            WRITE(92,*) SNGL(DZH0*(IZ-1)), PR0(IZ)
          END DO
          DO   IZ= 1, AMSED(CURRNT)
            WRITE(92,*) SNGL(DZH1*IZ + AH0(CURRNT)),
     &                  PR0(IZ+AMH0(CURRNT))
          END DO

        END IF
                                                                        
        IPROF= 1                                                            
        SUM2A( IPROF )= 0.0
                                                                        
      END IF                                                            
                                                                       
      RETURN                                                            
      END                                                               
