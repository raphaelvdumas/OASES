C   MAIN.FOR

C   DRIVER  *(SUPERSNAP)***********************************************
C

      SUBROUTINE CSNAPF
      INCLUDE 'param.inc'
      INCLUDE 'acommon.inc'

      external blkdat

      INTEGER COHRNT, INCRNT
      INTEGER EXTPOL, FIXDZ, OPTMZ
     
      LOGICAL CLTIME, MODPLT

      CHARACTER*2 TNORM, TVERT, THORI, SEGNO
      CHARACTER*3 COHINC
      CHARACTER*3 ALPHA2(6)
      CHARACTER*4 OPT
      CHARACTER*8 MODEL
      CHARACTER*80 TITLE
      integer iparm(3)

      DOUBLE PRECISION H0, H1, CMIN, CC0, CC1, CREF
      DOUBLE PRECISION ROB, ROS
      DOUBLE PRECISION FRQ, TWOPI, PI, OMEGA
      DOUBLE PRECISION F2
      DOUBLE PRECISION H0L, H0R, H1L, H1R
      DOUBLE PRECISION Z0L(402), C0L(402), Z1L(402), C1L(402)
      DOUBLE PRECISION Z0R(402), C0R(402), Z1R(402), C1R(402)
      DOUBLE PRECISION Z0LR(402), CE0L(402), CE0R(402)
      DOUBLE PRECISION Z1LR(201), CE1L(201), CE1R(201)

      REAL ETIME, CPSEC(2)
      REAL CPEIGV, CPEIGF, CPNEWP, CPFILE
      REAL CPU000, CPUBEG, CPUEND, CPUINI, CPUFRQ, CPOUTP
      REAL MAXRNG

      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2
      COMMON /ACOEFF/ COLEFT, NSTART
      COMMON /APM/ NSMPL, NSMDEF
      COMMON /CF/ ENRCHK, ERRMAX, MAXRNG
      COMMON /CONTUR/ CNTR1(4,4)
      COMMON /DENS/ R0, R1, R2
      COMMON /DENS8/ ROB, ROS
      COMMON /ENRGY/ MATCH
      COMMON /EXPMAX/ TRESH, EPS, RNGMAX, EPSDEF
      COMMON /FACTS/ FACT0, FACT1, FLAGF0, FLAGF1
      COMMON /FLAGG/ PLANE, NOVOL, NOLOSS, NOCYL, LARGE, SUMPL
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /FLAGS/ EK0, SQEK0
      COMMON /FRQDEP/ FRQREF, FPOW, FRQDEP
      COMMON /G/ H0, H1
      COMMON /CGAUSS/ GAUSS, TH1, TILT
      COMMON /LEFT/ RKML, BETAL(3), SCATTL(2), R1L, R2L,
     &              C2L, C2SL, H0L, H1L, ND0L, ND1L
      COMMON /LOGIC/ CLTIME
      COMMON /LUCONT/ LUCDR, LUBDR, LUCFR, LUBFR
      COMMON /LUIN/ LUIN, LUWRN, LUCHK
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /LUPUL/ LUTRF
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
      COMMON /MODEAD/ COHRNT, INCRNT
      COMMON /MODEL/ MODEL
      COMMON /MSHIST/ MH0(8), MSED(8), ICOUNT, USEOLD
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /NA/ ND0, ND1, CMIN
      COMMON /NN/ H0BEG, H1BEG, NFREQ
      COMMON /NORM/ N12221, N14241
      COMMON /PARA1/ NFFZ,MSP,NDEPZ,NOPTZ,ICFZ,KSRDZ
      COMMON /PARA3/ NR1Z,ISDZ,ISD2Z,NRPZ,MODNPZ
C      COMMON /PARA4/ NMAM2, NPAM2
      COMMON /PARAM1/ IRANGE, IFREQ, JUMP, MODQTY
      COMMON /PARAM2/ FRQ, EPSINP
      COMMON /PARAM3/ IMESH, NMESH, MSHRAT, OPTMZ
      COMMON /PULSED/ NFFT,FMIN,FMAX,TSTEP
      COMMON /MODSMP/ MSPH0, MODSED, MSPH1
      COMMON /REC1/ TITLE
      COMMON /REFSPD/ CREF
      COMMON /RIGHT/ RKMR, BETAR(3), SCATTR(2), R1R, R2R,
     &               C2R, C2SR, H0R, H1R, ND0R, ND1R
      COMMON /RNGPRF/ NSECT, RKM
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE
      COMMON /TLUNIT/ LUTLC, LUTLI
      COMMON /TRIGON/ TWOPI, PI, OMEGA
C
      INCLUDE 'common.inc'
C
      DATA MODPLT /.FALSE./
      DATA ((SECD(I,J),I= 1, NOPT),J=1,3)/NOPT3*0.0/
      DATA STORE/NOPT*0.0/
      DATA (((SRD(I,J,K),I= 1,NOPT),J =1,KSRD),K= 1,2)/NK2*0./
      DATA (FLAG(I,1),I= 1, NOPT)/NOPT*0.0/
      DATA ALPHA1/'TLRAN','GROUP','TLAVR','TLDEP','TLAVF',
     &            'ANGLE','PHASE','CONDR','CONFR','PROFL',
     &            'MODES','CONDA','HORWN','PARAM','FIELD',
     &            'PULSE','CONSV','FREE2'/
      DATA ALPHA2/'   ','PRT','PLT','COL','COH','INC'/
      DATA REORD1/6,12,8,9,17,15,2,13,11,14,7,16,10,5,3,4,1,18/
      DATA REORD2/11,1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18/
      DATA (AX(I,5),I= 1, NOPT)/NOPT*0.0/
      DATA (AX(I,6),I= 1, NOPT)/NOPT*1.0/
      DATA (AY(I,5),I= 1, NOPT)/NOPT*0.0/
      DATA (AY(I,6),I= 1, NOPT)/NOPT*0.0/
      DATA (AY(I,7),I= 1, NOPT)/NOPT*0.0/
C TLRAN; ISUB=1
      DATA (FLAG(1,I),I= 2, 6)/5*0.0/
      DATA AX(1,1),AX(1,2),AX(1,3),AX(1,4)/2*-1.,16.,0./
      DATA AY(1,1),AY(1,2),AY(1,3),AY(1,4)/40.,140.,12.,20./
C TLDEP; ISUB=4
      DATA (FLAG(4,I),I= 2, 6)/5*0.0/
      DATA AX(4,1),AX(4,2),AX(4,3),AX(4,4)/0.,160.,16.,20./
      DATA AY(4,1),AY(4,2),AY(4,3),AY(4,4)/0.,-1.,12.,0./
C CONDR; ISUB=8
      DATA (FLAG(8,I),I= 2, 6)/0.,1.,3*0./
      DATA AX(8,1),AX(8,2),AX(8,3),AX(8,4)/2*-1.,16.,0./
      DATA AY(8,1),AY(8,2),AY(8,3),AY(8,4)/0.,-1.,12.,0./
C CONFR; ISUB=9
      DATA (FLAG(9,I),I= 2, 6)/0.,1.,3*0./
      DATA AX(9,1),AX(9,2),AX(9,3),AX(9,4)/2*-1.,16.,0./
      DATA AY(9,1),AY(9,2),AY(9,3),AY(9,4)/0.,-1.,12.,1./
      DATA MINF9 / 0 /
C PROFL; ISUB=10
      DATA (FLAG(10,I),I= 2, 6)/0.,0.,0.,1.,0./
      DATA AS(1),AS(2),AS(3),AS(4)/1450., 1650., 3.0, 25./
      DATA AX(10,1),AX(10,2),AX(10,3),AX(10,4)/-1.,-1.,16.,0./
      DATA AY(10,1),AY(10,2),AY(10,3),AY(10,4)/ 0.,-1.,12.,0./
C MODES; ISUB=11
      DATA (FLAG(11,I),I= 2, 6)/5*0./
      DATA AX(11,1),AX(11,2),AX(11,3),AX(11,4)/-0.2,0.2,20.,.2/
      DATA AY(11,1),AY(11,2),AY(11,3),AY(11,4)/0.,0.,12.,0./
C CONDA; ISUB=12
      DATA (FLAG(12,I),I= 2, 6)/0.,1.,3*0./
      DATA AX(12,1),AX(12,2),AX(12,3),AX(12,4)/2*-1.,16.,0./
      DATA AY(12,1),AY(12,2),AY(12,3),AY(12,4)/0.,-1.,10.,0./
      DATA MINF12 / 0 /
C PULSE; ISUB=16
      DATA FLAG(16,2), FLAG(16,5) / 1.0, 1.0 /
C CONSV; ISUB=17
      DATA (FLAG(17,I),I= 2, 6)/0.,1.,3*0./
      DATA AX(17,1),AX(17,2),AX(17,3),AX(17,4)/2*-1.,16.,0./
      DATA AY(17,1),AY(17,2),AY(17,3),AY(17,4)/0.,-1.,12.,0./

      DATA MAXPOW / 13 /
      DATA TNORM, TVERT, THORI/' 1',' 0',' 0'/
      DATA SEGNO /' -'/

  121 FORMAT(1H1,//,'***************************************************
     &*********',//,1X,A80,//,'*****************************************
     &*******************',//)
  220 FORMAT(1H ,///,' OUTPUT OPTIONS: ',//,12X,'PRT',5X,'PLT',5X,'COH',
     & 5X,'INC',5X,'COL')
  240 FORMAT(1H ,A5,1X,4F8.0)
  241 FORMAT(1H ,A5,1X,2F8.0)
  242 FORMAT(1H ,A5)
  243 FORMAT(1H ,A5,1X,16X,2F8.0)
  244 FORMAT(1H ,A5,1X,5F8.0)
  250 FORMAT(1X,/,'  COMPUTING OPTION PULSE' )
  260 FORMAT(1X,/,'  Frequency (Hz)  (min, max)      :',F10.3,2X,F10.3,
     & /,'  Source depth (m)                :', F10.3,
     & /,'  Range (km)      (min, max, inc) :',F10.3,2(2x,f10.3),
     & /,'  Rec depth (m)   (min, max, inc) :',F10.3,2(2X,F10.3),/)

  320 FORMAT(1H ,' FREQ. NO. (',I4,',',I4,') :  ',F9.3,' Hz ',F9.3 )

  321 FORMAT(1H ,' FREQ. NO. (',I4,',',I4,') :  ',F9.3,' Hz ',
     & 3X,F9.3,' s' )
  322 FORMAT(1H ,' FREQ. NO. (',I4,',',I4,') :  ',F9.3,' Hz ',
     & 3X,'  MODE CUTOFF FOR THIS FREQUENCY ' )
  324 FORMAT(1H ,//,' * SUMMARY OF CPU TIME at frequency ',
     & F10.3,' Hz  :')
  326 FORMAT(1X ,    '   eigenvalues       : ',
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  328 FORMAT(1X ,    '   eigenvectors      : ',
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  330 FORMAT(1X ,    '   field computation : ', 
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  332 FORMAT(1X ,    '   print/plot output : ',
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  333 FORMAT(1X ,    '   total CPU time    : ',
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  334 FORMAT(1X ,/,' * CPU TIME FOR INITIALIZATION :',F7.3,'s  ')
  336 FORMAT(1X ,    '   CPU time at ',F10.3,' Hz : ',
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  338 FORMAT(1H ,/,' * CPU TIME for computing the EIGENFUNCTIONS :',
     & F9.3,'s',/,
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  342 FORMAT(1H ,  '* TOTAL CPU TIME :',F9.3,'s',/,
     & 3X,'( ',I3,'h  ',I2,'m  ',F7.3,'s  )')
  420 FORMAT(1X,' THE SIMPSON TRAPEZOIDAL RULE (122...221) IS ',
     & 'USED FOR MODE NORMALIZATION',/)
  440 FORMAT(1X,/,'  THE SIMPSON COMPOSITE FORMULA (142...4241) IS ',
     & 'USED FOR MODE NORMALIZATION ',/)
  460 FORMAT(1X,/,'  SELECTED ACCURACY ON EXTRAPOLATED ',
     & 'WN IS :',1PE9.1,/)
  470 FORMAT(1X,' RATIO AMONG ADJACENT',
     & ' MESHES (DEFAULT) = 1/3 ',/)
  480 FORMAT(1X,' RATIO AMONG ADJACENT MESHES : 2 ')
  482 FORMAT(1H ,/,'  THE VERTICAL GRID SIZE FOR MODE CALCULATION',
     & ' MAY VARY OVER RANGE. ',/,
     & '  THE GRID IS BASED ON (',I2,' SAMPLE POINTS ) / MODE',//)
  484 FORMAT(1H ,/,' THE VERTICAL GRID SIZE FOR MODE CALCULATION',
     & ' IS KEPT CONSTANT OVER RANGE ',/,
     & ' THE INITIAL GRID IS BASED ON (',I2,' SAMPLE POINTS/MODE',//)
  486 FORMAT(1X,' WARNING :  USING SUBOPTION "INPDZ" DZ IMPLIES ',
     & 'DOUBLING OF MESH DENSITY.')
  488 FORMAT(1X,//,' *************** ',/,' WARNING :  COMPUTING THE',
     & ' OUTGOING FIELD WITH REDUCED PRESSURE CORRECTION ',/,
     & ' *************** ',/)
  490 FORMAT(1X,//,' *************** ',/,' WARNING :  COMPUTING THE',
     & ' OUTGOING FIELD WITH IMPEDANCE MATCHING CORRECTION ',/,
     & ' *************** ',/)
  492 FORMAT(1X,//,' *************** ',/,' WARNING :  COMPUTING THE',
     & ' OUTGOING FIELD WITH PRESSURE MATCHING CONDITION',/,
     & ' *************** ',/)
  500 FORMAT(1H1 ,/,'  FREQUENCY ',F11.2,' HZ',/,
     & '  ****************',///)   
  510 FORMAT(1X, ' REGION ',I3,2X,' FINAL POWER OF 2 USED FOR RANGE',
     & ' SUBDIVISIONS :' ,I2)
  540 FORMAT(1X,//,' WARNING:  THE REQUIRED HALF_BEAMWIDTH IS TOO',
     & ' LARGE.',/,
     & ' REQUIRED HALF_BEAMWIDTH           :',F10.2,' deg',/,
     & ' CRITICAL ANGLE AT THE SOURCE DEPTH:',F10.2,' deg')
  560 FORMAT(1X,/,' COMPUTATION WILL CONTINUE WITH THE MAX POSSIBLE',
     & ' HALF_BEAMWIDTH:', F8.2,' deg',//)
  680 FORMAT(1X,'  MINIMUM DEPTH             :', F8.2,'  m',/,
     &         '   MAXIMUM DEPTH             :', F8.2,'  m',/,
     &         '   STEP INCREMENT IN DEPTH   :', F8.2,'  m',//)
  690 FORMAT(1X,'  NUMBER OF RANGE STEPS     :', I8 )
  700 FORMAT(1X,'  MINIMUM RANGE             :', F9.3,' km',/,
     &         '   MAXIMUM RANGE             :', F9.3,' km',/,
     &         '   STEP INCREMENT IN RANGE   :', F9.3,' km',/)

  710 FORMAT(1X,//,'   NUMBER OF INPUT REGIONS              :',I5)
  730 FORMAT(1X,//,'   OPTION TLRAN,',A3,/,
     &             '   SOURCE DEPTH              :',F8.2,'  m')
  739 FORMAT(1X,//,'   OPTION CONFR,',A3,/,
     &             '   SOURCE DEPTH              :',F8.2,'  m')
  740 FORMAT(1X,//,'   OPTION TLDEP,',A3,/,
     &             '   SOURCE DEPTH              :',F8.2,'  m')
  750 FORMAT(1X,//,'   OPTION CONDR,',A3,/,
     &             '   SOURCE DEPTH              :',F8.2,'  m')
  760 FORMAT(1X,    '  RECEIVER DEPTH            :',F8.2,'  m')
  770 FORMAT(1X,'  MINIMUM RECEIVER DEPTH   : ', F8.2,'  m',/,
     &         '   MAXIMUM RECEIVER DEPTH   : ', F8.2,'  m',/,
     &         '   NUMBER OF RECEIVERS      : ',I8,/)
  774 FORMAT(1X,'  NUMBER OF RECEIVERS      : ',I8,/)
  780 FORMAT(1X,'  MINIMUM RANGE            : ', F8.2,' km',/,
     &         '   MAXIMUM RANGE            : ', F8.2,' km',/,
     &         '   NUMBER OF RANGE STEPS    : ',I8,/)
  790 FORMAT(1X,////,' ***** ',/,'  DEPTH/RANGE MATRIX FOR SOURCE DEPTH'
     & ,' AT',F9.2,' m    :')
  800 FORMAT(1X,//,'    ****  OUTPUT FOR  "COHERENT" LOSS ****',/)
  810 FORMAT(1X,'  NUMBER OF RANGE SEGMENTS :    ',I5,//)                    
  820 FORMAT(1X,//,'    ****  OUTPUT FOR  "INCOHERENT" LOSS ****',/)
  830 FORMAT(1X,//,'    ****  OUTPUT FOR THE "PULSE" OPTION ****',/)
  840 FORMAT(1X,//,'  FATAL CONDITION DUE TO ARRAY SIZE LIMITATIONS:',/,
     & '  (number of receivers) * (number of range steps) is too large.'
     & ,/,
     & '  The maximum allowed number for this program version is ',I6,
     & '.',/,'   You are requesting ',I5,' range steps ',/,
     &       '   and a total of     ',I5,' depth points.',/,
     & '   EXECUTION IS TERMINATED ' )
  860 FORMAT(1X ,/,'  ERROR : THE TOTAL NUMBER OF SAMPLING POINTS',
     & ' OVER DEPTH',
     & /,'  ALLOWED FOR THIS SNAP VERSION IS ',I6,/,
     & '  THE PRESENT INPUT RUN STREAM REQUIRES : ',I6,
     & ' SAMPLING POINTS.',/,
     & '  THE EXECUTION IS TERMINATED ')

  880 FORMAT(1X,/,'  WARNING :  MAX ALLOWED POWER OF 2 FOR',
     & '  REGION SUBDIVISION',/,'  EXCEEDED IN REGION NO. ',I3,' .',/,
     & '  REQUESTED POWER OF 2    : ',I3,/,
     & '  MAX ALLOWED POWER OF 2  : ',I3,/,
     & '  CALCULATION WILL PROCEED WITH THE MAX ALLOWED POWER.',/)

  902 FORMAT(1X,' SUBOPTION "SNAP" IGNORED ')
  904 FORMAT(1X,' ***********    NEW ALGORYTHM   *******   ')
  906 FORMAT(1X,' increase either, MODEN or MSPMAX in PARAM.INC ')
  908 FORMAT(1X,' ERROR, NO RECEIVER DEPTH FOUND ')
  909 FORMAT(1X,' ERROR, NO SOURCE DEPTH FOUND ')
  910 FORMAT(1X,'  NUMBER OF RECEIVERS FOR CONDR     = ', I4,/,
     & 1X,' EXECUTION IS TERMINATED ')
  912 FORMAT(1X,' WARNING : MAX RANGE IS ADJUSTED TO:',F11.4,'km')
  914 FORMAT(1X,' NO CONDR PLOT FOR THIS SOURCE DEPTH (',F6.1,'m).')
  916 FORMAT(1X,' END OF "PULSE" OPTION ')
  918 FORMAT(1X,/,
     & ' WARNING: For this particular input run stream the half ',/
     & '          beamwidth will be computed with respect to the',/,
     & '          minimum sound speed in the water column given',/,
     & '          for the initial sound velocity profile. ',/)


cfmc      call blkdat

      CPU000= ETIME(CPSEC)

      OPT= 'R   '
      NFFZ=NFF
      MSP=MSPMAX
      NDEPZ=NDEP
      NOPTZ=NOPT
      ICFZ=ICF
      KSRDZ=KSRD

      NR1Z=NR1
      ISDZ=ISD
      ISD2Z=ISD2
      NRPZ=NRP
      MODNPZ=MODNP
      NSMPL= SMPLS
      NSMDEF= SMPLS
      NMAM2= MODEN - 2
      NPAM2= NPOINT - 2

      PI=DACOS(-1.0D0)
      TWOPI=2.0*PI

      TRESH= 1.0E17



C   SUBROUTINE FILE DEALS WITH THE INITIAL FILE ASSIGNMENTS.

      CALL FILE

C     DEFAULT CHOICE FOR ENERGY CONSERVATION AT COUPLING INTERFACES
C     IS " IMPEDANCE MATCHING " ( MATCH = 3 )
      MATCH= 3
C     FOR "REDUCED PRESSURE MATCHING" SPECIFY "MATCH   2
C     FOR "PRESSURE MATCHING" SPECIFY         "MATCH   1
    
      CALL INPUT( *6000, TITLE, FF, NSECT, MODEN, MAXMSH, 
     & ALPHA1, ALPHA2,
     & NINC, NXVAL,
     & C0, Z0, C1, Z1,
     & FLAG, AX, AY, AS, SECD, STORE, SRD,
     & RPROF, BATHY, NDIVP2, MAXPRF, HMAX)


C      ZSTEP= ( H0BEG + H1BEG ) / (MSP-1)
      ZSTEP= H0BEG / (MSP-1)

      IF(MODEL(1:6) .EQ. 'SNAP  ')   THEN
        PRINT 902
        MODEL(1:8)='C-SNAP  '
      END IF

      IF(NMESH .GT. 1)   EXTPOL= 2

      EPSDEF= 1.0E-6
      IF( NMESH .GT. 1)   WRITE(LUPRT,460) EPSDEF

      IF( NOCYL .GT. 0  )   OPT(1:2)='X '
      IF( PLANE .GT. 0.0)   OPT(1:1)='X'


C NMESH  number of meshes to be used to compute the WN.
C        the default value is 4 for the range independent version
C        and 1 for the range dependent one.
C        It can be defined through sub INPUT by adding in the input
C        file the code word  NMESH  followed by an integer number
C        specifying the desired mesh index.
C IMESH  Index of mesh when mode amps are to be output.
C        The default value is 1.
C

C N12221 and
C N14241 specify mode normalization technique (Simpson 12221, 14241)

C EXTRN  flag for eventual external mesh density specification (1 : ext

C LARGE  flag for the print out of the wave numbers, the attenuation
C        coeffs, and the echo of the environmental properties. 
C        LARGE= 0 is the default value. It allows the print out of the
C                 input profiles only.
C        LARGE= 1 extends the print out to the interpolated profiles
C                 as well.




C  01 = OPTION TLRAN
C  04 = OPTION TLDEP
C  08 = OPTION CONDR
C  09 = OPTION CONFR
C  09 = OPTION CONDA
      IF( (FLAG(01,1) .GE. 1.0) .OR.
     &    (FLAG(04,1) .GE. 1.0) .OR.
     &    (FLAG(08,1) .GE. 1.0) .OR.
     &    (FLAG(09,1) .GE. 1.0) )   THEN
        IF( MATCH .EQ. 2)   THEN
C         REDUCED PRESSURE MATCHING
          WRITE(LUPRT, 488)
          PRINT 488
        ELSE IF( MATCH .EQ. 3)   THEN
C         IMPEDANCE MATCHING       
          WRITE(LUPRT, 490)
          PRINT 490
        ELSE IF( MATCH .EQ. 4)   THEN
          PRINT 904
          WRITE(LUPRT,904)
          WRITE(LUPRT, 488)
          PRINT 488
        ELSE
C         PRESSURE MATCHING        
          WRITE(LUPRT, 492)
          PRINT 492
        END IF
      END IF



      IF( N12221 .EQ. 1 )   THEN
        WRITE(LUPRT,420)
      ELSE
        WRITE(LUPRT,440)
      END IF

      IF( INPDZ .EQ. 0 )   THEN
        IF(FIXDZ .EQ. 0)   THEN
          WRITE(LUPRT,482) NSMPL
        ELSE
          WRITE(LUPRT,484) NSMPL
          MSHRAT= 2
        END IF
      ELSE
        WRITE(LUPRT,486)
        MSHRAT= 2
      END IF

      IF(NMESH .GT. 1 )   THEN
        IF(MSHRAT .EQ. 0)   THEN
           WRITE(LUPRT,470)
        ELSE
           WRITE(LUPRT,480)
        END IF
      END IF

      CLOSE(LUIN)

      WRITE(LUPRT,121)TITLE
      WRITE(LUPRT,220)
C
C  WRITE OUT OF REQUESTED OPTIONS.
C
      DO   66   ISUB=1,NOPT
      K=REORD1(ISUB)
      IF(FLAG(K,1).LT.1.0)   GO TO   66
      GO TO(11,22,11,11,11,22,22,33,33,22,
     &      22,11,33,44,55,55,55,55      ),K
   11 WRITE(LUPRT,240)ALPHA1(K),FLAG(K,2),FLAG(K,3),FLAG(K,5),FLAG(K,6)
      GO TO 66
   22 WRITE(LUPRT,241)ALPHA1(K),FLAG(K,2),FLAG(K,3)
      GO TO 66
   33 WRITE(LUPRT,244)ALPHA1(K),FLAG(K,2),FLAG(K,3),FLAG(K,5),FLAG(K,6),
     &                FLAG(K,4)
      GO TO 66
   44 CONTINUE
      WRITE(LUPRT,242)ALPHA1(K)
      GO TO 66
   55 WRITE(LUPRT,243)ALPHA1(K),FLAG(K,5),FLAG(K,6)
   66 CONTINUE



      WRITE(LUPRT,710) NSECT
C
C  CHECK FOR PROFL OPTION.
C
      DO 1000   IOP= 1, NOPT
      IF(ALPHA1(IOP) .NE. 'PROFL')   GO TO 1000
        IF(FLAG(IOP,1) .GT. 0.0)   CALL PROFL(IOP, NSECT, RPROF,
     &                           C0, Z0, C1, Z1,
     &                           AX, AY, AS, NOPT, TITLE, ZY, CX )
        FLAG(IOP,1)= 0.0
      GO TO 1100
 1000 CONTINUE
 1100 CONTINUE

      IF(FLAG(11,3).GT.0.0)   MODPLT= .TRUE.
      JUMP=0

      ICONSV= 17
      ICON= 4
      IF(FLAG(ICONSV,1) .GT. 0.0)   CALL CONSV(ICONSV, ICON,
     &                              NSECT, RPROF, BATHY, HMAX,
     &                              C0, Z0, C1, Z1,
     &                              AX, AY, NOPT, TITLE,
     &                              FLAG, ICF)

C  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

C  Define range subdivisions in the environment

       CALL RDIV( MAXPRF, MAXPOW, NSECT, NEND, SECD, NOPT,
     &            NDIVP2, RPROF, RFILE, RCOUPL )

C  01 = OPTION TLRAN
C  04 = OPTION TLDEP
C  08 = OPTION CONDR
C  09 = OPTION CONFR
C  11 = OPTION MODES
C  09 = OPTION CONDA

      IF( (FLAG(01,1) .NE. 1.0) .AND.
     &    (FLAG(04,1) .NE. 1.0) .AND.
     &    (FLAG(08,1) .NE. 1.0) .AND.
     &    (FLAG(09,1) .NE. 1.0) .AND.
     &    (FLAG(11,1) .NE. 1.0) )   GO TO 4620



C  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C  CHECK OF CALCULATION TYPE FOR RANGE DEPENDENT CASE
C  ONLY TLRAN, TLDEP, CONDR, AND CONFR (IOP=1,4,8,9)
C
      IF( FLAG(01,5)+FLAG(04,5)+FLAG(08,5)+FLAG(09,5)+
     &    FLAG(12,5) .GT. 0)   COHRNT= 1
      IF( FLAG(01,6)+FLAG(04,6)+FLAG(08,6)+FLAG(09,6)+
     &    FLAG(12,6) .GT. 0)   INCRNT= 1

C  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

C   TIME FOR INTERFACING WITH INPUT FILE AND PREPARATORY

      CPUINI= ETIME(CPSEC) - CPU000

      DO 4600   IFREQ= 1, NFREQ

       CPUFRQ= ETIME(CPSEC)
       CPEIGV= 0.0
       CPEIGF= 0.0
       FIRST= 0.0
       IF( FLAG(11,1) .GT. 0.0 )   MODFLG= 1.0
       IF( FRQREF .LE. 0.0 )    THEN
         FRQDEP= 1
       ELSE
         FRQDEP= ( FF(IFREQ)/FRQREF)**FPOW
       END IF
       FRQ=DBLE(FF(IFREQ))
       F2= FRQ*FRQ*1.0D-6

C       WRITE(LUPRT,500) FF(IFREQ)
       PRINT 500, FF(IFREQ)

       BETA(-1)= F2 *
     &           (0.11 / (1.0 + F2) + 44.0 / (4100.0 + F2) + 3.0D-4)

      IF(IFREQ .GT. 1)   THEN
        DO 1550   IOP= 1, NOPT
        DO 1550   JSD= 1, KSRD
        SRD(IOP,JSD,1)= ABS( SRD(IOP,JSD,1) )
 1550   CONTINUE
      END IF


 1600 CONTINUE

C     SELECT A SOURCE DEPTH
      CALL DEFSD( ALPHA1,
     & REORD2, FLAG, SRD, SECD, AY, NOPT, ICF, KSRD,
     & QRNG, NRANGE, QRD, MSPMAX, ZSTEP,
     & RNGTMP, RDTMP,
C     OUTPUT PARAMETERS:
     & SD, OPTSD,
     & RNG, NRNGC,
     & RDAR, NRDC )


      FIRST= 0.0

      IF( SD .GT. 0.0 )   THEN
        ISHIFT= 0
        IF(RDAR(1) .EQ. 0.0)   ISHIFT= 1
        NRCVRS= NRDC
        NR= NRNGC

        PRINT 790, SD
        WRITE(LUPRT,790) SD
        REWIND LUTLC
        REWIND LUTLI

C   Check array size limits
        MAXSIZ= 2 * IDUMMY
        IF( NR*(NRCVRS-ISHIFT) .GT. MAXSIZ)   THEN
          WRITE(LUPRT,840) MAXSIZ, NR, (NRCVRS-ISHIFT)
          PRINT 840, MAXSIZ, NR, (NRCVRS-ISHIFT)
          WRITE(LUPRT,770) RDAR(1), RDAR(NRCVRS), NRCVRS
          PRINT 770, RDAR(1), RDAR(NRCVRS), NRCVRS
          print 906
          STOP
        END IF

        PRINT 770, RDAR(1), RDAR(NRCVRS), NRCVRS
        PRINT 780, RNG(1)*1.0E-3, RNG(NR)*1.0E-3, NR
        PRINT 810, NEND - 1
        WRITE(LUPRT,770)  RDAR(1), RDAR(NRCVRS), NRCVRS
        WRITE(LUPRT,780)  RNG(1)*1.0E-3, RNG(NR)*1.0E-3, NR
        WRITE(LUPRT,810) NEND - 1
        IF( MAXRNG .EQ. 0.0 )   THEN
          RNGMAX= RNG(NR)
        ELSE
          RNGMAX= MAXRNG
        END IF

      ELSE
        IF( MODFLG .LT. 1 )   GO TO 4600
      END IF

      REWIND(10)
      READ(10) MINMOD, MAXMOD
      IF(MAXMOD.EQ.0)   THEN
       WRITE(LUPRT,*) ' ***   MAXMOD IS EQUAL TO ZERO   **** '
       WRITE(LUPRT,*) ' EXECUTION IS TERMINATED '
       STOP
      END IF

      MAXREF=MAXMOD


C ******************
      EPSINP= 1.0E-3
C ******************


      CPUBEG= ETIME(CPSEC)
      ICOUNT=0
      USEOLD=0
      IRANGE= 1

C ****************************************************************

C     Reading environmental properties to define the LEFT extreme of 
C     the first region. They will be used to create the starting field.
C     For NSECT .GT. 1, also the environmental properties to define the
C     right extreme of the first region are read.
      RKM= -1.0
      CALL NEWENV( Z0, C0, Z1, C1, Z0L, C0L, C1L,
     & Z1L, Z0R, C0R, Z1R, C1R)


      IF(HBEAM .GT. 0.0)   THEN
C       Computing the critical angle at the source depth
        if( sd .eq. 0.0 )   then
          PRINT 918
          WRITE(LUPRT,918)
          CSD= 1.0E38
          DO   7263   IJK= 1, ND0
          CSD= MIN(CSD, REAL(C0(IJK)) )
 7263     CONTINUE
        else
          CSD= CZS( SD, Z0, C0, ND0 )
        end if
c        crtang= ACOS( CZS( SD, Z0, C0, ND0 )/C2 )
        crtang= ACOS( CSD/C2 )
C       Computing the max phase velocity BPHVEL associated with
C       the half beamwidth HBEAM (beam width with the horizontal axis).
C       The quantity "omega/bphvel" will be used as the lower limit
C       in the eigenvalue search.
        IF( (HBEAM*PI/180.0) .GT. crtang )   THEN
          PRINT 540, HBEAM, crtang*180/PI
          WRITE(LUPRT,540) HBEAM, crtang*180/PI
          PRINT 560,  crtang*180/PI
          WRITE(LUPRT,560) crtang*180/PI
          HBEAM= 0.0
        ELSE
c          BPHVEL=  CZS(SD, Z0, C0, ND0 )/COS( HBEAM*PI/180.0 )
          BPHVEL=  CSD/COS( HBEAM*PI/180.0 )
        END IF
      END IF

      IF ( PLANE .GT. 0.0 )   THEN
        EK0= (TWOPI*FRQ)/CZS( SD, Z0, C0, ND0 )
        SQEK0= SQRT(EK0)
      ELSE
        EK0= 1.0
        SQEK0= 1.0
      END IF

C ****************************************************************
      ALEFT=  2
      ARIGHT= 1
C **********************************************************************

      CALL MODE(*4600,
     & EK, C0, Z0, C1, Z1, ISO,
     & MY, MAXMSH, MODEN,
     & ADA, SPEED,
     & ZZ, Z0NRM, Z1NRM)

      IF( MODFLG .GT. 0) THEN
C     IOP= 11 IDENTIFIES OPTION "MODES" 
      IOP= 11

      CALL MODES( FF(IFREQ), TITLE, AX, AY,
     & MODPLT, XTS, MSP, IOP, FLAG, NOPT, ICF,
     & ZSTEP, MODQTY,
     &             ALFA, MODAVR,
     &             ADA,  SPEED, EIGF,
     &             A3, B3, C3,
     &             EE, ZZ, SSOLD, EXCH, EK)
 
       MODFLG= 0
       IF( LARGE .LE. 0 )    FLAGPU= 1.0
      END IF

      IF( SD .EQ. 0 )   GO TO 4600 

      IF( LARGE .LE. 0 )   JUMP= 1

      COLEFT= 0.0

      MAXMOD=MIN(MODCUT,MODEN)

C ******************
      EPSINP= 1.0E-3
C ******************


      ICOUNT=0
      USEOLD=0

C ******************************************************************

      IF( NSECT .GT. 1 )   THEN

C       WATER COLUMN
        CALL PROFEQ(ND0L,Z0L,C0L,ND0R,Z0R,C0R,ND0LR,Z0LR,CE0L,CE0R)


C       SEDIMENT LAYER
        CALL PROFEQ(ND1L,Z1L,C1L,ND1R,Z1R,C1R,ND1LR,Z1LR,CE1L,CE1R)
      END IF

C **********************************************************************



CFMC       CPUBEG= ETIME(CPSEC)
       COLEFT= 0.0
       IPROF= 0


       CALL EVALCM( RFILE, RCOUPL, NEND, IPROF, PHIR,
     &  SD, RDAR( 1 + ISHIFT ), NRCVRS - ISHIFT,
     &  NR, CK, MAXREF-MINMOD+1, OPT, RNG, SUM2A, FF(IFREQ),
     &  Z0L,C0L,Z1L,C1L,Z0R,C0R,Z1R,C1R,
     &  ND0LR, Z0LR, CE0L, CE0R,
     &  ND1LR, Z1LR, CE1L, CE1R,
     &  Z0,C0,Z1,C1,Z0NRM,Z1NRM,
     &  ALFA,EK,CKTAIL,MODAVR,ISO,MY,ADA,SPEED,EIGF,
     &  A3,B3,C3,EE,ZZ,SSOLD,EXCH)

CFMC        CPUEND= ETIME(CPSEC)
CFMC        WRITE(LUPRT,*) '* TOTAL TIME FROM EVALCM : ',
CFMC     &  (CPUEND-CPUBEG)


      CPOUTP= ETIME(CPSEC)

      DO 4102   IADD= 0, 1
      IF(IADD .EQ. 0)   THEN
        IF(COHRNT .LT. 1)   GO TO 4102
        PRINT 800
        WRITE(LUPRT,800)
        COHINC= 'COH'
        LUSORT= LUTLC
      ELSE
        IF(INCRNT .LT. 1)   GO TO 4102
        PRINT 820
        WRITE(LUPRT,820)
        COHINC= 'INC'
        LUSORT= LUTLI
      END IF
C     RETRIEVING TL DATA
      CALL RETRV(LUSORT, PRSS, NRCVRS-ISHIFT, NR)


      DO 4100   K = 1, NOPT

      IOP= REORD2(K)

      IF(FLAG(IOP,1) .LT. 1.0)   GO TO 4100

      GO TO(4001,4002,4003,4004,4005,4006,4007,4008,4009,4100,
     &      4011,4012,4100,4100,4015,4100,4100,4100          ),IOP


C***********************************************************************
C***********************************************************************


C     OPTION "TLRAN"
 4001 CONTINUE

      IF(FLAG(IOP,5+IADD) .LT. 1.0)   GO TO 4100

      ISTART= 0
      DO 3401   JSD= 1, KSRD
      SDABS= ABS(SRD(IOP,JSD,1))
      IF( ABS(SDABS/SD - 1.0) .LT. 1.0E-5 )   THEN
        WRITE(LUPRT,730) COHINC, SD
        PRINT 730, COHINC, SD
        IF(ISTART .EQ. 0)   THEN
          ISTART= 1
          JCOUNT= 0
          IF( RNG(1) .EQ. 0.0 )   ISTART= 2
          RMARCH= SECD(IOP,1)
          IF( RMARCH .EQ. 0.0  )   THEN
            JCOUNT= 1
            INDX(1)= JCOUNT
            RMARCH= RMARCH + SECD(IOP,3)
          END IF

          DO 3101   IR = ISTART, NR
          IF( ABS( RMARCH/RNG(IR) - 1.0 ) .LE. 1.0E-5)   THEN
            JCOUNT= JCOUNT + 1
            INDX(JCOUNT)= IR
             RMARCH = SECD(IOP,1) +  JCOUNT * SECD(IOP,3)
             IF( (RMARCH/SECD(IOP,2) - 1.0) .GT. 1.0E-5 )   GO TO 3201
          END IF
 3101     CONTINUE
 3201     CONTINUE
          PRINT 690, JCOUNT
          PRINT 700, SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &               SECD(IOP,3)*1.0E-3
          WRITE(LUPRT, 690)  JCOUNT
          WRITE(LUPRT,700)  SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &                      SECD(IOP,3)*1.0E-3
        END IF


        DO 3301  IRCVR= 1 + ISHIFT, NRCVRS
        IF(ABS(SRD(IOP,JSD,2)/RDAR(IRCVR) - 1.0) .LT. 1.0E-5) THEN
          PRINT 760, SRD(IOP,JSD,2)
          CALL TLCPLE(PRSS, NR, NRCVRS-ISHIFT, IRCVR-ISHIFT,
     &    INDX, JCOUNT, TLOSS)
          IF(IPROF .GT. 1 .AND. SUMPL .GT. 0.0)   THEN
            NC=2
            CALL OUTPL(IOP, SECD, TLOSS, FLAG, AX, AY, SD,
     &      RDAR(IRCVR), NOPT, ICF, FF(IFREQ), JCOUNT, TITLE,
     &      NC, COHINC)
            CALL OUTSUM(BIASUM, IPROF, RCOUPL, SUM2A,
     &                AY(IOP,1), AY(IOP,2))
          ELSE
            NC= 1
            CALL OUTPL(IOP, SECD, TLOSS, FLAG, AX, AY, SD,
     &      RDAR(IRCVR), NOPT, ICF, FF(IFREQ), JCOUNT, TITLE,
     &      NC, COHINC)
          END IF

          GO TO 3401
        END IF
 3301   CONTINUE
        PRINT 908
        STOP ' SUB MAIN '
      END IF
 3401 CONTINUE
      GO TO 4100
C***********************************************************************
C***********************************************************************

 4002 CONTINUE
      GO TO 4100
 4003 CONTINUE
      GO TO 4100

C***********************************************************************
C***********************************************************************


C     OPTION "TLDEP"
 4004 CONTINUE

      IF(FLAG(IOP,5+IADD) .LT. 1.0)   GO TO 4100

      DO 3604   JSD= 1, KSRD
      SDABS= ABS(SRD(IOP,JSD,1))
      IF( ABS(SDABS/SD - 1.0) .LT. 1.0E-5 )   THEN
        WRITE(LUPRT,740) COHINC, SD
        PRINT 740, COHINC, SD

        ISTART= 1
        IF( RNG(1) .EQ. 0.0 )   ISTART= 2
        JCOUNT= 0
        RMARCH= SECD(IOP,1)
        IF( RMARCH .EQ. 0.0  )   THEN
          JCOUNT= 1
          INDX(1)= JCOUNT
          RMARCH= RMARCH + SECD(IOP,3)
        END IF

        DO 3104   IR = ISTART, NR
        IF( ABS( RMARCH/RNG(IR) - 1.0 ) .LE. 1.0E-5)   THEN
          JCOUNT= JCOUNT + 1
          INDX(JCOUNT)= IR
          RMARCH = SECD(IOP,1) + JCOUNT * SECD(IOP,3)
          IF( (RMARCH/SECD(IOP,2) - 1.0) .GT. 1.0E-5 )   GO TO 3204
        END IF
 3104   CONTINUE
 3204   CONTINUE

        ISTART= 1
        IF( RDAR(1) .EQ. 0.0 )   ISTART= 2
        LCOUNT= 0
C       (AY(IOP,6), AY(IOP,7)) = (min, max) values in depth interval.
        ZMARCH= AY(IOP,6)
        IF( ZMARCH .EQ. 0.0  )   THEN
          LCOUNT= 1
          INDY(1)= LCOUNT
          ZMARCH= ZMARCH + ZSTEP
        END IF

        DO 3304   JRD= ISTART, NRCVRS
        IF( ABS( ZMARCH/RDAR(JRD) - 1.0 ) .LE. 1.0E-5 )   THEN
          LCOUNT= LCOUNT + 1
          INDY(LCOUNT)= JRD
          ZMARCH= AY(IOP,6) + LCOUNT * ZSTEP
          IF( (ZMARCH/AY(IOP,7) - 1.0 ) .GT. 1.0E-5 )   GO TO 3404
C          IF( ZMARCH .GT. AY(IOP,7) )   GO TO 3404
        END IF
 3304   CONTINUE
 3404   CONTINUE

        COLEFT= 0.0

        PRINT 774, LCOUNT
        PRINT 680, RDAR(INDY(1)), RDAR(INDY(LCOUNT)), ZSTEP
        PRINT 690, JCOUNT
        PRINT 700, SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &             SECD(IOP,3)*1.0E-3
        WRITE(LUPRT, 774) LCOUNT
        WRITE(LUPRT,680)  RDAR(INDY(1)), RDAR(INDY(LCOUNT)), ZSTEP
        WRITE(LUPRT, 690) JCOUNT
        WRITE(LUPRT,700)  SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &                    SECD(IOP,3)*1.0E-3

        CALL TLDEPC( SD, RNG, PRSS, NRCVRS, NR,
     &  RDAR, ISHIFT,
     &  FF(IFREQ), TITLE, IOP, TLOSS, INDX, INDY, JCOUNT, LCOUNT,
     &  ZSTEP, FLAG, AX, AY, NOPT, ICF, COHINC )

      END IF
 3604 CONTINUE
      GO TO 4100
C***********************************************************************
C***********************************************************************

 4005 CONTINUE
      GO TO 4100
 4006 CONTINUE
      GO TO 4100
 4007 CONTINUE
      GO TO 4100

C***********************************************************************
C***********************************************************************


C     OPTION "CONDR"
 4008 CONTINUE

      IF(FLAG(IOP,5+IADD) .LT. 1.0)   GO TO 4100

      DO 3408   JSD= 1, KSRD
      SDABS= ABS(SRD(IOP,JSD,1))
      IF( ABS(SDABS/SD - 1.0) .LT. 1.0E-5 )   THEN
        WRITE(LUPRT,750) COHINC, SD
        PRINT 750, COHINC, SD

        ISTART= 1
        IF( RNG(1) .EQ. 0.0 )   ISTART= 2
        JCOUNT= 0
        RMARCH= SECD(IOP,1)
        IF( RMARCH .EQ. 0.0  )   THEN
          JCOUNT= 1
          INDX(1)= JCOUNT
          RMARCH= RMARCH + SECD(IOP,3)
        END IF


        DO 3008   IR = ISTART, NR
        IF( ABS( RMARCH/RNG(IR) - 1.0 ) .LE. 1.0E-5)   THEN
          JCOUNT= JCOUNT + 1
          INDX(JCOUNT)= IR
          RMARCH = SECD(IOP,1) + JCOUNT * SECD(IOP,3)
          IF( (RMARCH/SECD(IOP,2) - 1.0) .GT. 1.0E-5 )   GO TO 3108
        END IF
 3008   CONTINUE
 3108   CONTINUE

        ISTART= 1
        IF( RDAR(1) .EQ. 0.0 )   ISTART= 2
        LCOUNT= 0
C       (AY(IOP,6), AY(IOP,7)) = (min, max) values in depth interval.
        ZMARCH= AY(IOP,6)
        IF( ZMARCH .EQ. 0.0  )   THEN
          LCOUNT= 1
          INDY(1)= LCOUNT
          ZMARCH= ZMARCH + ZSTEP
        END IF

        DO 3208   JRD= ISTART, NRCVRS
        IF( ABS( ZMARCH/RDAR(JRD) - 1.0 ) .LE. 1.0E-5 )   THEN
          LCOUNT= LCOUNT + 1
          INDY(LCOUNT)= JRD
          ZMARCH= AY(IOP,6) + LCOUNT * ZSTEP
          IF( (ZMARCH/AY(IOP,7) - 1.0 ) .GT. 1.0E-5 )   GO TO 3308
C          IF( ZMARCH .GT. AY(IOP,7) )   GO TO 3308
        END IF
 3208   CONTINUE
 3308   CONTINUE

        IF( LCOUNT .LT. 1 )   THEN
          PRINT 910, LCOUNT
          STOP
        END IF

        PRINT 774, LCOUNT
        PRINT 680, RDAR(INDY(1)), RDAR(INDY(LCOUNT)), ZSTEP
        IF( RNG(NR) .LT. SECD(IOP,2) )  THEN
          SECD(IOP,2)= SECD(IOP,1) + (JCOUNT-1) * SECD(IOP,3)
C          WRITE(LUPRT,912)   SECD(IOP,2)*1.0E-3 
C          PRINT 912, SECD(IOP,2)*1.0E-3
        END IF
        PRINT 690, JCOUNT
        PRINT 700, SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &             SECD(IOP,3)*1.0E-3
        WRITE(LUPRT, 774) LCOUNT
        WRITE(LUPRT,680)  RDAR(INDY(1)), RDAR(INDY(LCOUNT)), ZSTEP
        WRITE(LUPRT, 690) JCOUNT
        WRITE(LUPRT,700)  SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &                    SECD(IOP,3)*1.0E-3


        COLEFT= 0.0

        CPUBEG= ETIME(CPSEC)
        CALL CONDRC( NR, FF(IFREQ), TITLE, IOP, SD, RDAR,
     &  NRCVRS, ISHIFT,
     &  INDX, INDY, JCOUNT, LCOUNT, TLOSS,
     &  SECD, FLAG, AX, AY,
     &  NOPT, ICF, PRSS,
     &  RPROF, BATHY, NSECT, HMAX, COHINC )
        CPUEND= ETIME(CPSEC)
        CPUCDR= (CPUEND - CPUBEG)
        WRITE(LUPRT,*) '* TOTAL TIME FOR CONDRC : ', CPUCDR
CCHK        WRITE(LUCHK,*) '* TOTAL TIME FOR CONDRC : ', CPUCDR
      END IF
 3408 CONTINUE
      GO TO 4100

C***********************************************************************
C***********************************************************************


C     OPTION "CONFR"
 4009 CONTINUE

      IF(FLAG(IOP,5+IADD) .LT. 1.0)   GO TO 4100

      ISTART= 0
      DO 3409   JSD= 1, KSRD
      SDABS= ABS(SRD(IOP,JSD,1))
      IF( ABS(SDABS/SD - 1.0) .LT. 1.0E-5 )   THEN
        FOUND= 1.0
        WRITE(LUPRT,739) COHINC, SD
        PRINT 739, COHINC, SD
        IF(ISTART .EQ. 0)   THEN
          ISTART= 1
          JCOUNT= 0
          IF( RNG(1) .EQ. 0.0 )   ISTART= 2
          RMARCH= SECD(IOP,1)
          IF( RMARCH .EQ. 0.0  )   THEN
            JCOUNT= 1
            INDX(1)= JCOUNT
            RMARCH= RMARCH + SECD(IOP,3)
          END IF

          DO 3109   IR = ISTART, NR
          IF( ABS( RMARCH/RNG(IR) - 1.0 ) .LE. 1.0E-5)   THEN
            JCOUNT= JCOUNT + 1
            INDX(JCOUNT)= IR
             RMARCH = SECD(IOP,1) +  JCOUNT * SECD(IOP,3)
          END IF
 3109     CONTINUE
          PRINT 690, JCOUNT
          PRINT 700, SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &               SECD(IOP,3)*1.0E-3
          WRITE(LUPRT, 690) JCOUNT
          WRITE(LUPRT,700)  SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3,
     &                      SECD(IOP,3)*1.0E-3
        END IF


        IF(MINF9 .EQ. 0)   MINF9= IFREQ
        NC= 1
        FOUND= 0.0
        DO 3309  IRCVR= 1 + ISHIFT, NRCVRS
        IF(ABS(SRD(IOP,JSD,2)/RDAR(IRCVR) - 1.0) .LT. 1.0E-5) THEN
          FOUND= 1.0
          PRINT 760, SRD(IOP,JSD,2)
          CALL TLCPLE(PRSS, NR, NRCVRS-ISHIFT, IRCVR-ISHIFT,
     &    INDX, JCOUNT, TLOSS)

          CALL CONFR(TLOSS, JCOUNT, FF(IFREQ),
     &    COHINC, NMARCH,
     &    TITLE, IOP,
     &    MINF9, NFREQ, FF,
     &    FLAG, AX, AY, SRD, SECD,
     &    NOPT, ICF, KSRD)
        END IF
 3309   CONTINUE

C ***********
C  THIS PROGRAM VERSION ALLOWS ONLY FOR ONE SOURCE/RECEIVER DEPTH COMBINATION.
C  THE LIMITATION IS DUE TO THE PROBLEM OF DEFINING APPROPRIATE FILE NAMES
C  FOR THE OUTPUT FILES (ONE FILE FOR EVERY SD/RD COMBINATION).
C ***********

        IF( FOUND .EQ. 0.0 )   THEN
          PRINT 908
          STOP ' SUB MAIN '
        ELSE
          GO TO 4100
        END IF
      END IF
 3409 CONTINUE
      PRINT 909
      STOP ' SUB MAIN '

 4010 CONTINUE
      GO TO 4100
 4011 CONTINUE
 
      GO TO 4100
 4012 CONTINUE
      GO TO 4100
 4013 CONTINUE
      GO TO 4100
 4014 CONTINUE
      GO TO 4100
 4015 CONTINUE
      GO TO 4100


 4100 CONTINUE
 4102 CONTINUE

      CPOUTP= ETIME(CPSEC) - CPOUTP
      CPUFRQ= ETIME(CPSEC) - CPUFRQ

C      IF( NFREQ .GT. 1 )   THEN

      WRITE(LUPRT,324)   FF(IFREQ)

      TEMP= CPEIGV
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,326)   NHOUR, NMIN, TSEC

      TEMP= CPEIGF
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,328)   NHOUR, NMIN, TSEC

      TEMP= CPUFRQ - (CPEIGV+CPEIGF+CPOUTP)
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,330)   NHOUR, NMIN, TSEC

      TEMP= CPOUTP
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,332)   NHOUR, NMIN, TSEC

      TEMP= CPUFRQ
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,333)   NHOUR, NMIN, TSEC
      PRINT 336, FF(IFREQ), NHOUR, NMIN, TSEC 
C      END IF
      GO TO 1600
 4600 CONTINUE
      REWIND 10


 4620 CONTINUE



C  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C     "PULSE" OPTION
      IOP= 16
      IF(FLAG(IOP,1) .GT. 0.0)   THEN
C  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


       PRINT 250
       WRITE(LUPRT,250)

       COHRNT= 1
       FLAGPU=1

       IF( FMIN .EQ. FMAX )   THEN
         DELTAF= FMIN
         NFFT=1024
         TSTEP=1.0/(NFFT*DELTAF)
         NFREQ= 1
         JFMIN=2
         LXP1= 2
         JFMAX= 2
       ELSE
         DELTAF= 1.0/(NFFT*TSTEP)
         JFMIN= FMIN/DELTAF+1
         JFMIN= MAX(JFMIN,1)
         IF(JFMIN .EQ. 1) THEN
           LXP1= 2
         ELSE
           LXP1= JFMIN
         END IF
         JFMAX= nint(FMAX/DELTAF)+1
         JFMAX= MIN(JFMAX,NFFT/2)
         NFREQ= JFMAX-LXP1+1
       END IF

       SD= SRD(IOP,1,1)
C       PRINT 790, SD
C       WRITE(LUPRT,790) SD

       IF(SECD(IOP,1) .EQ. SECD(IOP,2))   THEN
         NR= 1
         SECD(IOP,3)= 0.0
       ELSE
         NR= (SECD(IOP,2)-SECD(IOP,1)+1.0E-3*SECD(IOP,3))/SECD(IOP,3)+1
       END IF
       IF(PULRD(1) .EQ. PULRD(2))   THEN
         NRCVRS= 1
         PULRD(3)= 0.0
       ELSE
         NRCVRS=(PULRD(2)-PULRD(1)+1.0E-3*PULRD(3))/PULRD(3)+1
       END IF

C   Check array size limits
       MAXSIZ= 2 * IDUMMY
       IF( NR*(NRCVRS-ISHIFT) .GT. MAXSIZ)   THEN
         WRITE(LUPRT,840) MAXSIZ, NR, (NRCVRS-ISHIFT)
         PRINT 840, MAXSIZ, NR, (NRCVRS-ISHIFT)
         WRITE(LUPRT,770) RDAR(1), RDAR(NRCVRS), NRCVRS
         PRINT 770, RDAR(1), RDAR(NRCVRS), NRCVRS
         print 906
         STOP
       END IF

       DO 1111  IRNG= 1, NR
       RNG(IRNG)= SECD(IOP,1) + (IRNG-1)*SECD(IOP,3)
 1111  CONTINUE
       DO 1112  IRD= 1, NRCVRS
       RDAR(IRD)= PULRD(1) + (IRD-1)*PULRD(3)
 1112  CONTINUE
       ISHIFT= 0
       IF(RDAR(1) .EQ. 0.0)   ISHIFT= 1


        PRINT 260, FMIN, FMAX, SD,
     &             SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3, 
     &             SECD(IOP,3)*1.0E-3,
     &             PULRD(1),  PULRD(2),  PULRD(3) 
        WRITE(LUPRT,260) FMIN, FMAX, SD,
     &             SECD(IOP,1)*1.0E-3, SECD(IOP,2)*1.0E-3, 
     &             SECD(IOP,3)*1.0E-3,
     &             PULRD(1),  PULRD(2),  PULRD(3) 



C      PRINT 770, RDAR(1), RDAR(NRCVRS), NRCVRS
C      PRINT 780, RNG(1)*1.0E-3, RNG(NR)*1.0E-3, NR
C      PRINT 810, NEND - 1

C      WRITE(LUPRT,770)  RDAR(1), RDAR(NRCVRS), NRCVRS
C      WRITE(LUPRT,780)  RNG(1)*1.0E-3, RNG(NR)*1.0E-3, NR
C      WRITE(LUPRT,810) NEND - 1


       JUMP=-1
       EPSINP=1.0E-3

       JFMIN=LXP1
       NFREQ= JFMAX-JFMIN+1


C   LUTLC IS USED AS INTERMEDIATE FILE.
      REWIND LUTLC
  

       DO 5800   IFREQ= LXP1, JFMAX

       CPUBEG= ETIME(CPSEC)

       JUMP= JUMP + 1
       REWIND(10)
       READ(10) MINMOD, MAXMOD
       MAXREF=MAXMOD

       FIRST= 0.0
       FRQCY= (IFREQ-1) * DELTAF
       IF( FRQREF .LE. 0.0 )    THEN
         FRQDEP= 1
       ELSE
         FRQDEP= ( FRQCY/FRQREF )**FPOW
       END IF
       FRQ= DBLE(FRQCY)
       F2= FRQ*FRQ*1.0D-6

C       PRINT 320, IFREQ, JFMAX, FRQCY

       BETA(-1)= F2 *
     &           (0.11 / (1.0 + F2) + 44.0 / (4100.0 + F2) + 3.0D-4)

       ICOUNT=0
       USEOLD=0
       IRANGE= 1

C ****************************************************************
C      Reading environmental properties to define the LEFT extreme of 
C      the first region. They will be used to create the starting field.
C      For NSECT .GT. 1, also the environmental properties to define the
C      right extreme of the first region are read.
       RKM= -1.0
       CALL NEWENV( Z0, C0, Z1, C1, Z0L, C0L, C1L,
     & Z1L, Z0R, C0R, Z1R, C1R)

       IF ( PLANE .GT. 0.0 )   THEN
         EK0= (TWOPI*FRQ)/CZS( SD, Z0, C0, ND0 )
         SQEK0= SQRT(EK0)
       ELSE
         EK0= 1.0
         SQEK0= 1.0
       END IF

C ****************************************************************
       ALEFT=  2
       ARIGHT= 1
C **********************************************************************

       CALL MODE(*5600,
     & EK, C0, Z0, C1, Z1, ISO,
     & MY, MAXMSH, MODEN,
     & ADA, SPEED,
     & ZZ, Z0NRM, Z1NRM)


       IF( LARGE .LE. 0 )   JUMP= 1

       COLEFT= 0.0

       MAXMOD=MIN(MODCUT,MODEN)

       ICOUNT=0
       USEOLD=0

C ******************************************************************

       IF( NSECT .GT. 1 )   THEN

C        WATER COLUMN
         CALL PROFEQ(ND0L,Z0L,C0L,ND0R,Z0R,C0R,ND0LR,Z0LR,CE0L,CE0R)


C        SEDIMENT LAYER
         CALL PROFEQ(ND1L,Z1L,C1L,ND1R,Z1R,C1R,ND1LR,Z1LR,CE1L,CE1R)
       END IF

C **********************************************************************

       COLEFT= 0.0
       IPROF= 0

       CALL EVALPR( RFILE, RCOUPL, NEND, IPROF, PHIR,
     &  SD, RDAR( 1 + ISHIFT ), NRCVRS - ISHIFT, ISHIFT,
     &  NR, CK, MAXREF-MINMOD+1, OPT, RNG, SUM2A, FRQCY,
     &  Z0L,C0L,Z1L,C1L,Z0R,C0R,Z1R,C1R,
     &  ND0LR, Z0LR, CE0L, CE0R,
     &  ND1LR, Z1LR, CE1L, CE1R,
     &  Z0,C0,Z1,C1,Z0NRM,Z1NRM,
     &  ALFA,EK,CKTAIL,MODAVR,ISO,MY,ADA,SPEED,EIGF,
     &  A3,B3,C3,EE,ZZ,SSOLD,EXCH, PRDEP)

       CPUEND= ETIME(CPSEC)
       WRITE(LUPRT,*) '* TOTAL TIME FROM EVALPR: ',
     & (CPUEND-CPUBEG)
       PRINT 321, IFREQ, JFMAX, FRQCY, (CPUEND-CPUBEG)
       WRITE(LUPRT, 321) IFREQ, JFMAX, FRQCY, (CPUEND-CPUBEG)
       GO TO 5800
 5600  CONTINUE

       PRINT 322, IFREQ, JFMAX, FRQCY
       WRITE(LUPRT, 322) IFREQ, JFMAX, FRQCY
       JFMIN= JFMIN+1

 5800  CONTINUE

      NOUT=0
      IF( TNORM .NE. ' 0' ) then
       NOUT=NOUT+1
       iparm(nout)=6
      end if
      IF( TVERT .NE. ' 0' ) then
       NOUT=NOUT+1
       iparm(nout)=2
      end if
      IF( THORI .NE. ' 0' ) then
       NOUT=NOUT+1
       iparm(nout)=3
      end if
       
C
      LBUFF=NPLOTS*NOUT
      NFREQ= MX-LXP1+1

C  HEADER OF OUTPUT FILE (C-SNAP.TRF = LUTRF = 83)
C
      write(lutrf) 'PULSETRF'
      WRITE(LUTRF) 'C-SNAP'
      WRITE(LUTRF) nout
      write(lutrf) (iparm(jj),jj=1,nout)
      WRITE(LUTRF) TITLE
      WRITE(LUTRF) SEGNO
      WRITE(LUTRF) FF(1)
      WRITE(LUTRF) SD
      WRITE(LUTRF) PULRD(1),PULRD(2),NRCVRS
      write(lutrf) secd(iop,1)*1e-3,secd(iop,3)*1e-3,nr
      WRITE(LUTRF) NFFT,JFMIN,JFMAX,TSTEP
      WRITE(LUTRF) IFIX(PLANE)
      omegim=0e0
      write(lutrf) omegim
      idum=1
      write(lutrf) idum
      write(lutrf) idum
      write(lutrf) idum
      idum=0
      write(lutrf) idum
      write(lutrf) idum
      dummy=0e0
      do 444 ii=1,5
       write(lutrf) dummy
 444   continue
C >>> read the trf values from temporary file
c       WRITE(LUTRF) 'C-SNAP'
c       WRITE(LUTRF) TNORM, TVERT, THORI
c       WRITE(LUTRF) TITLE
c       WRITE(LUTRF) SEGNO
c       WRITE(LUTRF) FF(1)
c       WRITE(LUTRF) SD
c       WRITE(LUTRF) PULRD(1), PULRD(2), NRCVRS
c       WRITE(LUTRF) NFFT, JFMIN, JFMAX, TSTEP, SECD(IOP,1)*1.0E-3,
c     &              SECD(IOP,3)*1.0E-3, NR
C  IGEOM IS FLAG FOR PLANE (1) OR CYLINDRICAL GEOMETRY (0)
c       WRITE(LUTRF) IFIX(PLANE)


       PRINT 830
       WRITE(LUPRT,830)
       NFREQ= JFMAX-JFMIN+1
       CALL SORTP(NRCVRS, NR, NFREQ, LUTRF, LUTLC,
     & WORK, IDUMMY)

       PRINT 916
      END IF


 6000 CONTINUE

      WRITE(LUPRT,334)   CPUINI
      TEMP= ETIME(CPSEC) - CPU000
      NHOUR= TEMP/3600.
      TLEFT= MOD(TEMP,3600.)
      NMIN= TLEFT/60.
      TSEC= MOD(TLEFT,60.)
      WRITE(LUPRT,342) TEMP, NHOUR, NMIN, TSEC

C      STOP ' END OF  * MAIN *  '
      END
