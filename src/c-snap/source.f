C   SOURCE.FOR   FROM [FERLA.CONSV]SOURCE2.FOR
      SUBROUTINE SOURCE(SD, ICURV, beamw, tilt, AVGK, FREQ, H0,
     &                  DZH0, DZH1, NW, NB, PRS, ABSPR, YCOORD,
     &                  PLT)

C     THIS CODE BUILDS THE WIDE ANGLE GAUSSIAN INITIAL FIELD.
C     BY DEFAULT, BY CHOOSING beamw= 0. WE COMPUTE THE 
C     STANDARD GAUSSIAN STARTING FIELD, WHICH CORRESPONDS
C     TO A WIDE ANGLE GAUSSIAN FIELD WITH beamw= 30 DEG.

C      INPUT:
C              ZS    - SOURCE INPUT DEPTH (m)
C              ICURV - FLAG (0: BEAM TILT; 1: BEAM CURVATURE)
C              DZH0  - DEPTH MESH INCREMENT IN THE WATER LAYER (m)
C              DZH1  - DEPTH MESH INCREMENT IN THE SEDIMENT (m)
C              beamw - HALF BEAM WIDTH (DEG)
C              tilt  - BEAM TILT ANGLE FROM HORIZONTAL (DEG)
C              AVGK  - AVERAGE WAVE NUMBER
C              NW    - NUMBER OF MESH POINTS IN WATER
C              NB    - NUMBER OF MESH POINTS IN SEDIMENT
C              H0    - WATER DEPTH
C
C     OUTPUT:
C     (NOTE: THE FIRST POINT IN THE ARRAY CORRESPONDS TO DEPTH= DZH0)
C              PRS   - COMPLEX SOURCE FIELD   
C     LOCAL VARIABLES:
C                    - GA  GAUSSIAN AMPLITUDE
C                    - GW  GAUSSIAN WIDTH
C                    - ZRM  DEPTH OF SOURCE FIELD MESH (M)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      REAL ABSPR(*), YCOORD(*)

      REAL SD, beamw, tilt, AVGK, FREQ, PR, PI, PLT

      COMPLEX PRS(*)



      PIIR= DACOS(-1.0D0)/180.0D0
      ZS= SD
      IF( ZS .LE. H0 )   THEN
        IND= ZS/DZH0 + 1
      ELSE
        IND= NW + (ZS-H0)/DZH1 + 1
      END IF


C*******************************************************************************
C   GAUSSIAN SOURCE                                                            *
C                                                                              *
C   PRS(M)=SQRT(AVGK)*TAN(beamw)*(EXP(-AVGK**2/2.*(ZR(M)-ZS)**2*TAN(beamw)**2) *
C                               -EXP(-AVGK**2/2.*(ZR(M)+ZS)**2*TAN(beamw)**2)) *
C TILT FOR PAREQ                *EXP(i*AVGK*(ZR(M)-ZS)*SIN(tilt))              *
C TILT FOR C-SNAP (ACTUAL ONE)  *EXP(i*AVGK*(ZR(M)-ZS)*SIN(-tilt))             *
C i= cmplx(0,1)                                                                *
C                                                                              *
C*******************************************************************************
C  THIS EXPRESSION IS USED IN PAREQ:      RTH2=tilt*PIIR
C  THE NEXT EXPRESSION IS USED IN C-SNAP:
      RTH2= -tilt*PIIR
      GW=SQRT(2.0)/AVGK
      GA=SQRT(AVGK)
      IF(ABS(beamw).GT.1.E-12) THEN
        RTH1=beamw*PIIR
        TANTH1=TAN(RTH1)
        TANTH1S=TANTH1*TANTH1
        GA=GA*TANTH1
      END IF

      IF(ABS(beamw).LT.1.E-12.AND.ABS(tilt).LT.1.E-12) THEN
C       STANDARD GAUSSIAN SOURCE

        DO   M= 1, NW
          ZRM= DZH0*FLOAT(M-1)
          PR= GAUSS(GA,ZRM,ZS,GW)-GAUSS(GA,-ZRM,ZS,GW)               
          PRS(M)= CMPLX( PR, 0.0 )
        END DO
        DO   M= NW+1, NW + NB
          ZRM= H0 + DZH1*FLOAT(M-NW)
          PR= GAUSS(GA,ZRM,ZS,GW)-GAUSS(GA,-ZRM,ZS,GW)               
          PRS(M)= CMPLX( PR, 0.0 )
        END DO

      ELSE
C       WIDE ANGLE GAUSSIAN SOURCE
        SITH2=SIN(RTH2)
        IF(ICURV.EQ.1.AND.ABS(tilt).GT.1.E-12) THEN
          ZSOSIN=ZS/SITH2
          ZSOTAN=(ZS/TAN(RTH2))**2
        END IF
        SITH2=SITH2*AVGK

        DO 2000 M= 1, NW + NB
        ZRM= DZH0*FLOAT(M-1)
        IF(M .GT. NW)   ZRM= H0 + DZH1*FLOAT(M-NW)
        ZDIFF=ZRM-ZS
        ZSUM=ZRM+ZS
        ARG= -(ZDIFF/GW)**2*TANTH1S
        IF(ARG .LT. -709.0) THEN
          P1=0.0
        ELSE
          P1=EXP(ARG)
        END IF
        ARGIM= -(ZSUM/GW)**2*TANTH1S
        IF(ARGIM .LT. -709.0) THEN
          P2=0.0
        ELSE
          P2=EXP(ARGIM)
        END IF
        P=GA*(P1-P2)
        IF(ICURV.EQ.0) THEN
C         BEAM TILT
          ZDIFFS=ZDIFF*SITH2
        ELSE
C         BEAM CURVATURE
          IF(tilt.LT.0.) THEN
            ZDIFFS=(-SQRT(ZSOTAN+ZRM**2)-ZSOSIN)*AVGK
          ELSE
            ZDIFFS=(SQRT(ZSOTAN+ZRM**2)-ZSOSIN)*AVGK
          END IF
        END IF

        PR= P*COS(ZDIFFS)
        PI= P*SIN(ZDIFFS)
        PRS(M)= CMPLX(PR, PI)
C       WRITE(27,*) M,ZRM
C       WRITE(27,*) ARG,PR(M)

 2000   CONTINUE
      END IF    

      IF( PLT .GT. 0 )   THEN
        DO 3000   I= 1, NW+NB
        ABSPR(I)= ABS(PRS(I))
 3000   CONTINUE


        CALL PRSDEP(SD, beamw, ABSPR, YCOORD, DZH0, DZH1,
     &  NW, NB, FREQ )
C      , IOP,
C     & FLAG, AX, AY, NOPT, ICF )
      END IF


      RETURN
      END




      FUNCTION GAUSS(GA,Z,GD,GW)

C     THIS CODE CALCULATES THE GAUSS FUNCTION.

C     INPUT  - GA  GAUSSIAN AMPLITUDE
C              GD  GAUSSIAN DEPTH
C              GW  GAUSSIAN WIDTH
C              Z   DEPTH

C     OUTPUT - GAUSS = GA * EXP(-((Z - GD) / GW)**2)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      U= (Z-GD)/GW
      U=- DMIN1(42.0D0,U*U)
      GAUSS= GA*EXP(U)
      RETURN
      END
