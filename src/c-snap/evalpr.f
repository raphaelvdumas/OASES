C  EVALPR.FOR

      SUBROUTINE EVALPR( RFILE, RCOUPL, NPROF, IPROF, PHIR,
     & SD, RD, NRD, ISHIFT, NR, CK, M, OPT, RNG, SUM2A, FREQ,
     &  Z0L, C0L, Z1L, C1L, Z0R, C0R, Z1R, C1R,
     &  ND0LR, Z0LR, CE0L, CE0R,
     &  ND1LR, Z1LR, CE1L, CE1R,
     &  Z0, C0, Z1, C1, Z0NRM, Z1NRM,
     &  ALFA, EK, CKTAIL, MODAVR, ISO, MY, ADA, SPEED, EIGF,
     &  A3, B3, C3, EE, ZZ, SSOLD, EXCH, PRDEP)



C     Computes pressure field using coupled mode theory
C     Normalized to pressure of point source at 1 meter                 

C     OPT = X     Cartesian   (X, Z) coordinates                        
C     OPT = R     Cylindrical (R, Z) coordinates                        
                                                                        
C     Note number of propagating modes is reset after first segment.    
C     Thus M restricts the number of modes in the source field but      
C     thereafter energy is allowed to couple into higher-order modes.   
                                                                        
C     Note flaws:                                                       
C        should half-space contribution include imaginary part?         
C                                                                       
                                                                        
                                                                        
                                                                        
C     COMPLEX ARG, FACT                                                 
      COMPLEX CI, CZERO                                                        
      COMPLEX PRDEP( * )
      PARAMETER ( CI= ( 0.0, 1.0 ), CZERO= ( 0.0, 0.0 ),
     &            PI= 3.1415926 )                   
                                                                        
      INTEGER COHRNT, INCRNT                                            
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
      INCLUDE 'bcommon.inc'                                             
                                                                        
      CHARACTER*4 OPT                                                   
                                                                        
      REAL      RD( * ), RCOUPL( * ), RNG( * ), SUM2A( * ),              
     &          RFILE( * )                                                
                                                                        
      REAL PHIR(MODEN,*)
      COMPLEX   PHIS( MODEN ), A( MODEN ), SUM,       
     &          CK( * ), CKTAIL( * )                       
                                                                        
      COMPLEX CPL( NPOINT ), CPR( NPOINT )
                                                                        
      COMMON /ACOEFF/ COLEFT, NSTART                                    
      COMMON /MODEAD/ COHRNT, INCRNT                                   
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT         
      COMMON /TLUNIT/ LUTLC, LUTLI                                      
                                                                        
                                                                        
  100 FORMAT(1H ,I8.8,'.DAT    ')                                         
  200 FORMAT(1X,' Marching toward range ',F10.1,'m -  # modes=', I4)          
  300 FORMAT(1X,' Crossing interface at',F9.3,                          
     &                ' km        new mode_set at',F9.3,' km')          
  400 FORMAT(1X,/,'  * * WARNING FOR NEXT SOURCE FREQUENCY: ',
     & ' MODE CUTOFF AT ',F8.3,' km')
                                                                        
                                                                        
C Next two lines are introduced as a reminder in case we generate a Xfer
C file
C          ARG= CMPLX( 0., -PI/4. )                                     
C          FACT= CEXP(ARG)                                              
                                                                        
                                                                        
C     *** Ranges (in meters) where new profiles are used are obtained
C     *** from array RCOUPL    
                                                                        
                                                                        
      IF ( RCOUPL( 1 ) .NE. 0.0 )                                      
     &   STOP 'FATAL ERROR: First profile must start at zero range'     
                                                                        
      RCOUPL(  NPROF + 1 ) = 1.0E9                                     
                                                                        
C     *** Evaluate mode excitation coefficients, A(mode) ***            
                                                                        
      IPROF = 0                                                           
      NSD   = 1                                                         
                                                                        
      CALL START( IPROF, SD, RD, NSD, NRD, OPT,               
     &            CK, CKTAIL, EK, PHIS, PHIR, M,                          
     &            M1, FREQ,
     &            A, SUM2A, CPL, CPR, RCOUPL(2),                       
     &            ALFA, MODAVR,
     &            ADA, SPEED, EIGF,                                       
     &            A3, B3, C3, EE, ZZ, SSOLD, EXCH)                            
      IF( IPROF .GT. 1 )   THEN                                         
        ISTART= 2                                                       
      ELSE                                                              
        ISTART= 1                                                       
      END IF                                                            
                                                                        
      RNG1 = RNG( 1 )                                                     
      IF ( RNG( 1 ) .EQ. 0.0 )   RNG( 1 ) = RNG(2)                        
                                                                        
                                                                        
C     *** March forward in range ***                                    
                                                                        
                                                                        
      IR = ISTART                                                             
                                                                        
 2400 CONTINUE                                                             
C
                                                                        
C        *** Crossing into new range segment? ***                       
                                                                        
         IF ( RNG( IR ) .GT. RCOUPL( IPROF + 1 ) ) THEN                
                                                                        
            IPROF = IPROF + 1                                           
                                                                        
C           ------ Advance to interface                                 
            IF ( IR .EQ. 1 ) THEN                                       
               CALL ADVPHA( RCOUPL( IPROF )                , A, CK, M )
            ELSE                                                        
               CALL ADVPHA( RCOUPL( IPROF ) - RNG( IR - 1 ), A, CK, M )
            ENDIF                                                       
                                                                        
C           ------ Here's where we cross over                           
            IF ( IPROF  .LE. NPROF ) THEN                               
                                                                        
CP               PRINT 300, RCOUPL(IPROF)*1.0E-3, RFILE(IPROF)           
               CALL NEWMOD( *4000, RFILE(IPROF),                      
     &         Z0L,C0L,Z1L,C1L,Z0R,C0R,Z1R,C1R,                         
     &         ND0LR, Z0LR, CE0L, CE0R,                                 
     &         ND1LR, Z1LR, CE1L, CE1R,                                 
     &         Z0,C0,Z1,C1,Z0NRM,Z1NRM,                                 
     &         EK,ISO,MY,MAXMSH,MODEN,
     &         ADA, SPEED, ZZ)
                                                                        
               CALL NEWPRO( NPROF, IPROF, RCOUPL,                 
     &         CK, CKTAIL, EK, PHIR, M, RD, NRD,                          
     &         A, SUM2A(IPROF), FREQ, CPL, CPR,                        
     &             ALFA, MODAVR,                                        
     &             ADA,  SPEED, EIGF,                                   
     &             A3, B3, C3,                                          
     &             EE, ZZ, SSOLD, EXCH)                                 
                                                                        
CP               WRITE(LUPRT,200) RNG(IR), M                                    
            ENDIF                                                       
                                                                        
C           ------ Are there other segments to cross?                   
 2600       CONTINUE                                                    
            IF ( RNG( IR ) .GT. RCOUPL( IPROF + 1 ) ) THEN             
               IPROF = IPROF + 1                                        
               CALL ADVPHA( RCOUPL( IPROF ) - RCOUPL( IPROF - 1 ),    
     &                      A, CK, M )                                  
               IF ( IPROF  .LE. NPROF )   THEN                          
                                                                        
CP                 PRINT 300, RCOUPL(IPROF)*1.0E-3, RFILE(IPROF)         
                 CALL NEWMOD( *4000, RFILE(IPROF),                    
     &           Z0L,C0L,Z1L,C1L,Z0R,C0R,Z1R,C1R,                       
     &           ND0LR, Z0LR, CE0L, CE0R,                               
     &           ND1LR, Z1LR, CE1L, CE1R,                               
     &           Z0,C0,Z1,C1,Z0NRM,Z1NRM,                               
     &           EK, ISO, MY, MAXMSH, MODEN,
     &           ADA, SPEED, ZZ)
                                                                        
                 CALL NEWPRO( NPROF, IPROF, RCOUPL,               
     &           CK, CKTAIL, EK, PHIR, M, RD, NRD,                        
     &           A, SUM2A(IPROF), FREQ, CPL, CPR,                       
     &             ALFA, MODAVR,                                        
     &             ADA,  SPEED, EIGF,                                   
     &             A3, B3, C3,                                          
     &             EE, ZZ, SSOLD, EXCH)                                 
                                                                        
               END IF                                                   
                                                                        
CP               WRITE(LUPRT,200) RNG(IR), M                                    
               GOTO 2600       
            ENDIF                                                       
                                                                        
C           ------ Advance the remaining distance                       
            CALL ADVPHA( RNG( IR ) - RCOUPL( IPROF ), A, CK, M )       
                                                                        
         ELSE                                                           
                                                                        
            IF ( IR .EQ. 1 ) THEN                                       
               CALL ADVPHA( RNG( IR )                , A, CK, M )       
            ELSE                                                        
               CALL ADVPHA( RNG( IR ) - RNG( IR - 1 ), A, CK, M )       
            ENDIF                                                       
                                                                        
         ENDIF                                                          
                                                                        
                                                                        
C        *** For each rcvr add up modal contributions ***               
                                                                        
                                                                        
         PRDEP(1)= CZERO
         DO 2800 IRD = 1+ISHIFT, NRD                                         
           SUM = CZERO                                                  
           DO 2700 MODE = 1, M                                        
             SUM = SUM + A( MODE ) * PHIR( MODE, IRD - ISHIFT )
cfmc             SUM = SUM + A( MODE ) * PHIR( MODE, IRD )
 2700      CONTINUE                                                   
           IF ( OPT(1:1) .EQ. 'R' ) SUM = SUM / SQRT( RNG( IR ) )     
           PRDEP(IRD)= CI*SUM
 2800    CONTINUE                                                     
         WRITE(LUTLC) (PRDEP(IRD), IRD=1, NRD+ISHIFT)
                                                                        
                                                                        
C        ------ Next range step                                         
C
      IR = IR + 1                                                         
      IF ( IR .LE. NR )   GO TO 2400                                      
      RNG( 1 ) = RNG1                                                     
      RETURN                                                            
                                                                        
 4000 CONTINUE                                                          
      RNG( 1 ) = RNG1
      PRINT 400, 1.0e-3*RNG(IR)
      DO 4200   IRNG= IR, NR
      WRITE(LUTLC) ( CZERO, IRD=1, NRD)
 4200 CONTINUE

      RETURN                                                            
      END
