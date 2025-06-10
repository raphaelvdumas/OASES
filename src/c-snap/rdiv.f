      SUBROUTINE RDIV( MAXPRF, MAXPOW, NSECT, NEND, SECD, NOPT, 
     &                 NDIVP2, RPROF, RFILE, RCOUPL )

      INTEGER NDIVP2(*)
      REAL RPROF(*), RFILE (*), RCOUPL(*)
      REAL SECD(NOPT,3)

  660 FORMAT(1X,//,' ***  THE REQUIRED TOTAL NUMBER OF RANGE',
     & ' SUBDIVISIONS IS ',I5,'.',/,
     & '      THE MAX ALLOWED QUANTITY IS ',I5,/,
     & '      EXECUTION IS TERMINATED ')

  880 FORMAT(1X,/,'  WARNING :  MAX ALLOWED POWER OF 2 FOR',
     & '  REGION SUBDIVISION',/,'  EXCEEDED IN REGION NO. ',I3,' .',/,
     & '  REQUESTED POWER OF 2    : ',I3,/,
     & '  MAX ALLOWED POWER OF 2  : ',I3,/,
     & '  CALCULATION WILL PROCEED WITH THE MAX ALLOWED POWER.',/)


      IF ( RPROF( 1 ) .NE. 0.0 )
     &   STOP 'FATAL ERROR: First profile must start at zero range'

      MAXPOW= 12
      RNGMAX= 0.0
      DO 1000   IOP= 1, NOPT
      
      RNGMAX= MAX( RNGMAX, SECD(IOP,2) )
 1000 CONTINUE
      RNGMAX= MAX( RNGMAX+(RNGMAX-RPROF(NSECT)), RPROF(NSECT) ) + 1.0      
C  Define range subdivisions

        NEND= 1
        RFILE(1)= 0.0
        RCOUPL(1)= 0.0

      IF(NSECT .GT. 1)   THEN

        DO 1450   ISECT= 1, NSECT-1
        NSTART= NEND
        NLOOP=  NDIVP2(ISECT)
        IF( NLOOP .GT. MAXPOW )   THEN
          PRINT 880,  ISECT-1, NLOOP, MAXPOW
          NLOOP= MAXPOW
        END IF

        IF(NDIVP2(ISECT) .GE. 0)   THEN
          NPOW2= 2 ** NLOOP
          NEND= NSTART + NPOW2

          IF(NEND .GT. MAXPRF)   THEN
            PRINT 660, NEND, MAXPRF
            STOP
          END IF

          RSTEP=  (RPROF(ISECT+1) - RPROF(ISECT)) / NPOW2
C          PRINT *,'J, RPROF(J), RPROF(J+1) ', 
C     &    ISECT,RPROF(ISECT), RPROF(ISECT+1)
          DO 1400   I= NSTART + 1, NEND - 1
          RFILE(I)= RPROF(ISECT) + (I-NSTART) * RSTEP
C          PRINT *,' I, RFILE(I) ',I, RFILE(I)
 1400     CONTINUE
          RFILE(NEND)= RPROF(ISECT+1)
C          PRINT *,' I, RFILE(I) ',NEND, RFILE(NEND)
C     *** Compute ranges (in meters) where new profiles are used ***
          DO 1420   I= NSTART + 1, NEND
          RCOUPL(I)= 500.0 * ( RFILE(I-1) + RFILE(I))
 1420     CONTINUE

        ELSE 
          NEND= NSTART + 1
          RFILE(NEND)= RPROF(ISECT+1)
C          PRINT *,' I, RFILE(I) ',NEND, RFILE(NEND)
C     *** Compute ranges (in meters) where new profiles are used ***
          RCOUPL(NEND)= 1000.0 * RFILE(NEND)
        END IF

 1450   CONTINUE

      END IF

      NEND= NEND + 1
      RFILE(NEND)= RNGMAX
      RCOUPL(NEND)= 500.0 * (RFILE(NEND) + RFILE(NEND-1))
C      PRINT *,' RDIV, NEND,RNGMAX,RFILE(NEND) : ',
C    & NEND,RNGMAX,RFILE(NEND)
C      DO 2222   IJ= 1, NEND
C      PRINT *,' I, RCOUPL(I) ',IJ, RCOUPL(IJ)
C 2222 CONTINUE


      END
