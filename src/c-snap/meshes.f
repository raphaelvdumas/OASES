C   MESHES.FOR
      SUBROUTINE MESHES( MREF, IREF, FRQ,
     &                   NPH0, MRH0,
     &                   NPH1, MRH1 )

C__________________________________________________________ 
C                                                          |
C     This routine estimates the max number of modes       |
C     in both the water and sediment layers and the        |
C     number of discretization points to be used in the    |
C     finite difference scheme.                            |
C__________________________________________________________|


      INTEGER FIXDZ, OPTMZ, MREF(*)


      DOUBLE PRECISION FRQ, CMIN
      DOUBLE PRECISION CC0, CC1, H0, H1

      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2
      COMMON /APM/ NSMPL, NSMDEF
      COMMON /G/ H0, H1
      COMMON /FACTS/ FACT0, FACT1, FLAGF0, FLAGF1
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MESH/ DZH0, DZH1, INPDZ, FIXDZ
      COMMON /MSHIST/ MH0(8), MSED(8), ICOUNT, USEOLD
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /NA/ ND0, ND1, CMIN
      COMMON /NORM/ N12221, N14241
      COMMON /PARAM1/ IRANGE, IFREQ, JUMP, MODQTY
      COMMON /PARAM3/ IMESH, NMESH, MSHRAT, OPTMZ
      COMMON /PARAM4/ MESHI, MESHN
      COMMON /RNGPRF/ NSECT, RKM

C     NPH0   number of subdivisions in layer H0
C     NPH1   number of subdivisions in layer H1
C     MRH0   multiplier of first mesh in layer H0
C     MRH1   multiplier of first mesh in layer H1
C     NSMPL  input parameter: it specifies the number of points/mode.
C     FACT0  input parameter: it is a multiplier for NSMPL in H0
C     FACT1  input parameter: it is a multiplier for NSMPL in H1


  100 FORMAT(1X,/,
     & '  NUMBER OF SAMPLES/MODE                  :' ,I4  ,/)
  101 FORMAT(1X,/,
     & '  FIXED DZ, INPUT VALUES :  DZH0, DZH1  ',2(F8.2,2X),/)
  110 FORMAT('  ESTIMATED NUMBER OF MODES IN LAYER H0   :' ,F9.2)
  111 FORMAT('  ESTIMATED NUMBER OF MODES IN LAYER H1   :' ,F9.2)
  112 FORMAT('  ESTIMATED NUMBER OF MODES IN LAYER H0+H1:' ,F9.2)

  120 FORMAT('  ESTIMATED DZ IN LAYER H0              :' ,F9.2,' m')
  121 FORMAT('  ESTIMATED DZ IN LAYER H1              :' ,F9.2,' m')
  122 FORMAT('  ESTIMATED DZ IN LAYER H0+H1           :' ,F9.2,' m')
  123 FORMAT('  RESULTING ESTIMATED DZ ON FIRST MESH  :' ,F9.2,' m')

  160 FORMAT('  DZ IN LAYER H0 ON FIRST MESH          :' ,F9.2,' m')
  161 FORMAT('  DZ IN LAYER H1 ON FIRST MESH          :' ,F9.2,' m')

  200 FORMAT('  PREDICTED/USED # OF POINTS ON FIRST MESH IN LAYER H0 :',
     &    I6,' /',I6,/,
     & '  FINAL DZ ON FIRST MESH IN LAYER H0           :',F9.2,' m')
  201 FORMAT('  PREDICTED/USED # OF POINTS ON FIRST MESH IN LAYER H1 :',
     &    I6,' /',I6,/,
     & '  FINAL DZ ON FIRST MESH IN LAYER H1           :',F9.2,' m')
  220 FORMAT('  NUMBER OF POINTS USED ON FIRST MESH IN LAYER H0 :',I6)
  221 FORMAT('  NUMBER OF POINTS USED ON FIRST MESH IN LAYER H1 :',I6)
  300 FORMAT(1X, ' OUTPUT MESH FOR MODEAMPS CANNOT EXCEED THE TOTAL',
     & ' NUMBER OF MESHES.',/,'  REVISE INPUT RUN STREAM.',/,
     & '  TOTAL NUMBER OF MESHES : ',I2,';    OUTPUT MESH : ',I2)
  320 FORMAT(1X,/, ' *** MESSAGE FROM SUB MESHES :',//,
     & '  ESTIMATED NUMBER OF MESHES                  :' ,I4  ,/,
     & '  MODEAMP POSSIBLY MADE AVAILABLE ON MESH No. :' ,I4  ,/)

C TMH0   ESTIM # OF MODES IN LAYER H0
C TMH1   ESTIM # OF MODES IN LAYER H1
C TMTOT  ESTIM # OF MODES IN LAYER HTOT= H0 + H1


      IF( RKM .EQ. 0.0 )   THEN
        IF( MESHI .GT. MESHN )   THEN
          WRITE(LUPRT,300) MESHN, MESHI
          STOP ' SUB MESH_SIZE '
        ELSE
          IF(JUMP .LE. 0)   WRITE(LUPRT,320) MESHN, MESHI
        END IF

        IF( JUMP .LE. 0 )   THEN
          IF( H0*DZH0 + H1*DZH1 .EQ. 0.)    THEN
            WRITE(LUPRT,100)  NSMPL
          ELSE
            WRITE(LUPRT,101)  DZH0, DZH1
          END IF
        END IF
      END IF

      MBASE= MREF(IREF)
      IF( INPDZ .GT. 0 )   MBASE= 1
      RBASE= MBASE

C     ESTIMATED MAX NUMBER OF MODES IN LAYER H0

      TMH0= 0.0
      IF( H0 .GT. 0.0 )   THEN
        TMH0= ANINT(( 2.0*FRQ*H0/CC0 ) *
     &        ( LOG( SQRT(1.0 - (CC0/C2)) + 1.0 ) ) / LOG(2.0))
        IF(TMH0 .GT. 0.0)   THEN
          TDH0= H0/(NSMPL*TMH0)
        ELSE
          TDH0= MAX(H0,H1)
        END IF
      END IF


      H0SNGL= H0
      H1SNGL= H1

      IF( INPDZ .LT. 1 )   THEN

C       ESTIMATED MAX NUMBER OF MODES IN LAYER H1
        TMH1= 0.0
        TDH1= MAX(H0,H1)
        IF( H1 .GT. 0.0 )   THEN
          TMH1= ANINT(( 2.0*FRQ*H1/CC1 ) *
     &        ( LOG( SQRT(1.0 - MIN(1.0D0,(CC1/C2))) + 1.0 ) ) /
     &          LOG(2.0))
          IF(TMH1 .GT. 0.0)   TDH1= H1/(NSMPL*TMH1)
        END IF

C       ESTIMATED MAX NUMBER OF MODES IN LAYERS H0+H1
        HTOT= H0 + H1
        CAVRG= (H0*CC0 + H1*CC1)/HTOT
        TMTOT= ANINT(( 2.0*FRQ*HTOT/CAVRG ) *
     &         ( LOG( SQRT(1.0 - (CAVRG/C2)) + 1.0 ) ) / LOG(2.0))
        IF(TMTOT .GT. 0.0)   THEN
          TDTOT= HTOT/(NSMPL*TMTOT)
        ELSE
          TDTOT= HTOT
        END IF

        IF( FLAGPU .LT. 1.0 )   THEN
          IF( H0 .GT. 0.0 )   WRITE(LUPRT,110)  TMH0
          IF( H1 .GT. 0.0 )   WRITE(LUPRT,111)  TMH1
          IF( H0 .GT. 0.0 .AND.  H1 .GT. 0.0 )  WRITE(LUPRT,112) TMTOT
        END IF

        DZTOT= MIN(TDH0,TDH1,TDTOT)

        IF( FLAGPU .LT. 1.0 )   THEN
          IF( H0 .GT. 0.0)   WRITE(LUPRT,120)  TDH0
          IF( H1 .GT. 0.0)   WRITE(LUPRT,121)  TDH1
          IF( H0 .GT. 0.0 .AND. H1 .GT. 0.0 )   WRITE(LUPRT,122)  TDTOT
          WRITE(LUPRT,123)  DZTOT
        END IF

        IF( H0 .GT. 0.0 )   THEN
          RNPH0= ANINT(H0SNGL/DZTOT)
          NPH0= MAX( 2., RNPH0 )
        ELSE
          NPH0= 0
          MRH0= 0
        END IF

        IF( H1 .GT. 0.0 )   THEN
          RNPH1= ANINT(H1SNGL/DZTOT)
          NPH1= MAX( RNPH1, 1.0 )
        ELSE
          NPH1= 0
          MRH1= 0
        END IF

      ELSE
C       case with  INPDZ .GT. 0

        MRH0= 1
        MRH1= 1
        DZH0= DZH0 * 2 ** (MESHI - 1)
        DZH1= DZH1 * 2 ** (MESHI - 1)

        IF( H0 .GT. 0.0 )   THEN
          IF( FLAGPU .LT. 1.0 )   WRITE(LUPRT,160) DZH0
          RNPH0= ANINT(H0SNGL/DZH0)
          NPH0= MAX( 2., RNPH0 )
        ELSE
C         THIS CASE TO BE IMPLEMENTED YET
          NPH0= 0
          MRH0= 0
        END IF

        IF( H1 .GT. 0.0 )   THEN
          IF( FLAGPU .LT. 1.0 )   WRITE(LUPRT,161) DZH1
          RNPH1= ANINT(H1SNGL/DZH1)
          NPH1= MAX( RNPH1, 1.0 )
        ELSE
          NPH1= 0
          MRH1= 0
        END IF

      END IF


      IF(N14241 .EQ. 1)   THEN
        NPH0= NPH0 + MOD(NPH0,2)
        NPH1= NPH1 + MOD(NPH1,2)
      END IF


C  INSERIRE CONTROLLO CON 40 E 50 OLTRECHE' CON 32
      IF( MSHRAT .NE. 2 )   THEN
        IF( H0 .GT. 0.0 )   THEN
          MRH0= MAX( ANINT(RNPH0/RBASE), 1.0 )
          FDZH0=  H0SNGL/(MRH0*MBASE )
          IF( FLAGPU .LT. 1.0 )   WRITE(LUPRT,200) NPH0, MRH0*MBASE,
     &                            FDZH0
        ELSE
          MRH0= 0
          FDZH0= 0.0
        END IF

        IF( H1 .GT. 0.0 )   THEN
          MRH1= MAX( ANINT(RNPH1/RBASE), 1.0 )
          FDZH1=  H1SNGL/(MRH1*MBASE )
          IF( FLAGPU .LT. 1.0 )   WRITE(LUPRT,201) NPH1, MRH1*MBASE,
     &                            FDZH1
        ELSE
          MRH1= 0
          FDZH1= 0.0
        END IF
      ELSE
        IF( FLAGPU .LT. 1.0 )   THEN
          IF( H0 .GT. 0.0 )   WRITE(LUPRT,220) NPH0
          IF( H1 .GT. 0.0 )   WRITE(LUPRT,221) NPH1
        END IF
      END IF


      CALL REVIEW( MREF, IREF, MRH0, MRH1, FRQ )


C  ADOPT PRESENT DZ FOR SUBSEQUENT PROFILES ?
      IF(INPDZ .GT. 0)   RETURN
      IF( FIXDZ .GT. 0 )   THEN
        INPDZ= 1
        DZH0= H0 / NPH0
        DZH1= DZH0
        IF( NPH1 .GT. 0.0 )   DZH1= H1 / NPH1
      END IF


      RETURN
      END
