C  MODPRT.FOR
      SUBROUTINE MODPRT(ZSTEP, XTS, EIGF, MH0I, MSP, FREQ,
     & MINMOD, MAXMOD, MODE )


      CHARACTER*9 LAMP

      REAL XTS( 12, MSP )

      DOUBLE PRECISION EIGF(*)
      DOUBLE PRECISION DEL, PZ

      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT

      DATA LAMP/'AMPLITUDE'/
      DATA IC / 10 /


  500 FORMAT(1H1,' MODE AMPLITUDES FOR SOURCE FREQUENCY =',F9.2,' Hz')
  510 FORMAT(10X,12(3X,I3,'MODE'))
  520 FORMAT(1X,' DEPTH (m)',1X,12(1X,A9))
  530 FORMAT(F8.2,2X,12F10.3)
                                                                        
                                                                        

      INDEX= MOD(MODE-MINMOD,IC) + 1

C     SAMPLING THE EIGENFUNCTIONS
      XTS(INDEX,1)= 0.0
      DEL= DBLE(MH0I)/DBLE(MSP-1)

      DO 1000   I= 2, MSP
      PZ= DEL*(I-1)
      N1= INT(PZ)
      N2= N1+1
      IF(N1.EQ.0)   THEN
       XTS(INDEX,I)= (PZ-N1)*EIGF(N2)
      ELSE
       XTS(INDEX,I)= (PZ-N1)*(EIGF(N2)-EIGF(N1))+EIGF(N1)
      END IF
 1000 CONTINUE
      XTS(INDEX,MSP)= EIGF(MH0I)

      IF( (INDEX .EQ. IC) .OR. (MODE .EQ. MAXMOD) )   THEN
        ISTART= MODE - INDEX + 1
        WRITE(LUPRT,500) FREQ
        WRITE(LUPRT,510) (K,K=ISTART, MODE)
        WRITE(LUPRT,520) (LAMP,K=ISTART,MODE)
        DO 2000   JP=1,MSP
 2000   WRITE(LUPRT,530) (JP-1)*ZSTEP, (XTS(K,JP),K=1,INDEX)
      END IF

      RETURN

      END
