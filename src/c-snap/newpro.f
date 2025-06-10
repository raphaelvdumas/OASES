C   NEWPRO.FOR                                                          
      SUBROUTINE NEWPRO( NPROF, IPROF, RCOUPL,                    
     &                   CK, CKTAIL, EK, PHIR, MR, RD, NRD,               
     &                   A, SUM2, FREQ, CPL, P,                        
     &             ALFA, MODAVR,                                        
     &             ADA,  SPEED, EIGF,                                   
     &             A3, B3, C3,                                          
     &             EE, ZZ, SSOLD, EXCH)                                 
                                                                        
       DOUBLE PRECISION DH0I, DSEDI, EKM                                
       DOUBLE PRECISION FRQ                                             
                                                                        
C     For a given profile number:                                       
C         read in modes for current segment                             
C         project the pressure field onto the new modes                 
C         extract values of the modes at rcvr depths                    
                                                                        
      COMPLEX CZERO
      PARAMETER ( CZERO= (0.0, 0.0 ), MAXMED = 50 )
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
      INTEGER EXTPOL
      INTEGER    IRD( MAXNRD )                             

      REAL ETIME, CPSEC(2)
      REAL CPUBEG, CPUEND
      REAL CPEIGV, CPEIGF, CPNEWP, CPFILE

      CHARACTER  BCBOTR * 1, BCTOPR * 1                                 
                                                                        
      CHARACTER*8 MODEL                                                   
                                                                        
      REAL      RD( * ), W( MAXNRD ),
     &          RHO( MAXMED ), DEPTH( MAXMED )                            
      REAL RCOUPL( * )                                                   
                                                                        
      LOGICAL EXCH(*)                                                   
      REAL ALFA(*), MODAVR(*)                                           
                                                                        
      DOUBLE PRECISION EK( * )                                          
      DOUBLE PRECISION ADA(*), SPEED(*), EIGF(*)                        
      DOUBLE PRECISION A3(*), B3(*), C3(*)                              
      DOUBLE PRECISION EE(*), ZZ(*), SSOLD(*)                           
                             
      REAL PHIR(MODEN,*)
      COMPLEX*8 SUM                                           
      COMPLEX CK( * ), CKTAIL( * )                                      
      COMPLEX   P( * ), CPL( * ), A( * ),                         
     &          GAMTR, GAMBR, PHITR, PHIBR,                             
     &          KTOP2R, KBOT2R, TAIL, PHITMP,                           
     &          PHITLN( MODEN ), PHIBLN( MODEN ),                           
     &          GAMTLN( MODEN ), GAMBLN( MODEN )                            
      COMPLEX SNAPCK                                                      
      COMPLEX PEKRT                                                     
      COMPLEX AEXT, CI                                                    

      COMMON /ENRGY/ MATCH
      COMMON /FLAGG/ PLANE, NOVOL, NOLOSS, NOCYL, LARGE, SUMPL
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT    
      COMMON /MODEL/ MODEL                                                
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE                    
                                                                        
      DATA CI / (0.0, 1.0 )/                                              
                                                                        
c  200 FORMAT(1X,' Evaluating excitation coeffs for interface at ',      
c     & F9.3,' km',/,                                                    
c     & '  Left extreme ',F9.3,' km      Right extreme ',f9.3,' km',/,   
c     & '  SUM2**2 = ',E12.6)                                            
  200 FORMAT(1X,' Evaluating excitation coeffs for interface at ',      
     & F9.3,' km')
                                                                        
                                                                        
      CPUBEG= ETIME(CPSEC)                                       
                                                                        
      IF ( NRD. GT. MAXNRD )   THEN                                       
       WRITE(LUPRT,*)  'NRD > MAXNRD in routine NEWPRO',NRD,MAXNRD            
       STOP                                                               
      END IF                                                              
                                                                        
                                                                        
C     *** Get pressure along left the interface ***                     
                                                                        
      IF( MATCH .EQ. 3)   THEN
C       IMPEDANCE MATCHING 
        CALL PLEFTS( RHO, DEPTH, NMEDIA,                    
     &  MR, CPL, P, NL, NR, NTOT,                                
     &  BCTOPR, RHOTR, KTOP2R, DEPTTR,                                
     &  BCBOTR, RHOBR, KBOT2R, DEPTBR,                                
     &  ML, FREQ )
      ELSE IF( MATCH .EQ. 2)   THEN
C       REDUCED PRESSURE MATCHING
        CALL PLEFTR( RHO, DEPTH, NMEDIA,                    
     &  MR, CPL, P, NL, NR, NTOT,                                
     &  BCTOPR, RHOTR, KTOP2R, DEPTTR,                                
     &  BCBOTR, RHOBR, KBOT2R, DEPTBR,                                
     &  ML, FREQ )
      ELSE
C       PRESSURE MATCHING
        CALL PLEFT( RHO, DEPTH, NMEDIA,                    
     &  MR, CPL, P, NL, NR, NTOT,                                
     &  BCTOPR, RHOTR, KTOP2R, DEPTTR,                                
     &  BCBOTR, RHOBR, KBOT2R, DEPTBR,                                
     &  ML, FREQ )
      END IF                                                                  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                  
      IF( IPROF .LT. NPROF )   THEN                                     
         DELTAR= RCOUPL( IPROF + 1 ) - RCOUPL( IPROF )                
      ELSE                                                              
         DELTAR= 0.0                                                    
      END IF                                                            
      DO 1000   IZ= 1, NR                                               
      CPL( IZ )= CZERO
 1000 CONTINUE                                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                  
                                                                        
                                                                        
C     ***  Read in eigenfunctions and extract receiver values ***       
                                                                        
C     Compute weights for mode interpolation at rcvr depths             
                                                                        
      CALL WEIGHT( ZR, NTOT, RD, NRD, W, IRD )                          
                                                                        
                                                                        
      sum2 = 0.0                                                        
      MODFIL = 30                                                         
                                                                          
                                                                        
       FRQ= DBLE( FREQ )                                                
       MH0I= AMH0(CURRNT) - 1                                         
       MSEDI= AMSED(CURRNT)                                           
       DH0I= ADH0(CURRNT)                                             
       DSEDI= ADSED(CURRNT)                                           
       EIGF( 1 )= 0.0                                                   
                                                                        
      DO 9000 MODE = 1, MR                                              
                                                                        
       CALL EIGVEC( MODE, MH0I, MSEDI, DH0I, DSEDI,                     
     &             FRQ, ALFA(MODE),                                     
     &             MINMOD, MODAVR,                                      
     &             ADA, SPEED, EIGF(2),                                 
     &             A3, B3, C3, EE, ZZ, SSOLD, EXCH,                     
     &             EKM, EIGVL(MODE), EK(MODE) )
                                                                        
       IF( NOLOSS .GT. 0 )   ALFA(MODE)= 0.0                            
       CKTAIL(MODE)= CMPLX(SNGL(EKM), -ALFA(MODE))                      
       CK(MODE)= CMPLX(SNGL(EK(MODE)), -ALFA(MODE))                     
                                                                        
      IF( MODEL(1:7) .EQ. 'C-SNAP ' )   THEN                              
        SNAPCK = CMPLX( SNGL(EKM**2), 0.0 )
C        SNAPCK = CMPLX( REAL( CKTAIL( MODE )**2 ), 0.0 )                  
      ELSE                                                                
        SNAPCK = CK( MODE ) ** 2                                          
      END IF                                                              
                                                                        
         IF ( BCTOPR .EQ. 'A' ) THEN                                    
            PHITR = CMPLX( SNGL(EIGF( 1 )), 0.0 )
            GAMTR = PEKRT( SNAPCK - KTOP2R )                              
         ENDIF                                                          
                                                                        
         IF ( BCBOTR .EQ. 'A' ) THEN                                    
            PHIBR = CMPLX( SNGL(EIGF( NR )), 0.0 )
            GAMBR = PEKRT( SNAPCK - KBOT2R )                              
         ENDIF                                                          
                                                                        
C        --- Compute new amplitudes:                                    
C           A = Integral[ P( z ) * PHI( z ) dz ]                        
C           (Repeat for each mode PHI to produce excitation coef. A)    
C           Integral is done using trapezoidal rule                     
                                                                        
         SUM = CZERO
                                                                        
         DO 6000 IZ = 1, NTOT                                           
                                                                        
C           ------ Calculate PHITMP at that depth                          
                                                                        
            PHITMP = CZERO
            ZT = ZR( IZ )                                               
                                                                        
            IF ( ZT .GT. DEPTBR ) THEN                                 
               IF ( BCBOTR .EQ. 'A' ) PHITMP = PHIBR *                  
     &                          EXP( -GAMBR * ( ZT - DEPTBR ) )        
            ELSE IF ( ZT .LT. DEPTTR ) THEN                            
               IF ( BCTOPR .EQ. 'A' ) PHITMP = PHITR *                  
     &                          EXP( -GAMTR * ( DEPTTR - ZT ) )        
            ELSE                                                        
               PHITMP = CMPLX( SNGL(EIGF( IZ )), 0.0 )
            ENDIF                                                       
                                                                        
            SUM = SUM + P(IZ ) * PHITMP                                 
 6000    CONTINUE                                                       
                                                                        
                                                                        
C        *** Compute the contribution from tails in the halfspaces ***  
                                                                        
         IF ( BCTOPR .EQ. 'A' ) THEN                                    
            SUM   = SUM + TAIL( ZR( 1    ), PHITL, GAMTL, DEPTTL, ML,  
     &                             PHITR / RHOTR, GAMTR, DEPTTR )      
         ENDIF                                                          
                                                                        
         IF ( BCBOTR .EQ. 'A' ) THEN                                    
            SUM   = SUM + TAIL( ZR( NTOT ), PHIBL, GAMBL, DEPTBL, ML,  
     &                             PHIBR / RHOBR, GAMBR, DEPTBR )      
         END IF                                                         
                                                                        
                                                                        
         A( MODE ) = SUM                                                
         sum2 = sum2 + abs( sum )**2                                    
                                                                        
C        *** Subtabulate modes at receiver depths ***                   
                                                                        
         DO 8000 IR = 1, NRD                                            
                                                                        
            PHIR( MODE, IR ) = 0.
            IF      ( RD( IR ) .LT. DEPTTR ) THEN                      
C              ------ Rcvr in upper halfspace                           
               IF ( BCTOPR .EQ. 'A' ) PHIR( MODE, IR ) = real(PHITR *        
     &                     EXP( -GAMTR * ( DEPTTR   - RD( IR  ) ) ) )   
            ELSE IF ( RD( IR ) .GT. DEPTBR ) THEN                      
C              ------ Rcvr in lower halfspace                           
               IF ( BCBOTR .EQ. 'A' ) PHIR( MODE, IR ) = real(PHIBR *        
     &                     EXP( -GAMBR * ( RD( IR ) - DEPTBR ) ))       
            ELSE                                                        
               IZ = IRD( IR )                                           
               PHIR( MODE, IR ) = real(CMPLX( SNGL(EIGF( IZ )) +
     &         W( IR ) * ( SNGL(EIGF( IZ+1 )) -                         
     &                     SNGL(EIGF( IZ )) ), 0.0 ) )                          
            ENDIF                                                       
                                                                        
                                                                        
 8000    CONTINUE                                                       
                                                                        
                                                                        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                        
                                                                        
C   Compute pressure at the new interface "deltar" km away              
C      IF( IPROF .LT. NPROF )   THEN                                    
                                                                        
        AEXT= A( MODE ) * EXP( -CI * CK( MODE ) * DELTAR )              
        DO 8200   IZ= 1, NR                                             
        CPL( IZ )= CPL( IZ ) + SNGL(EIGF( IZ )) *  AEXT                 
 8200   CONTINUE                                                        
                                                                        
C        ------ Halfspace information ------                            
                                                                        
        PHITLN( MODE ) = AEXT *  SNGL(EIGF( 1 ))                        
        PHIBLN( MODE ) = AEXT *  SNGL(EIGF( NR ))                       
                                                                        
        GAMTLN( MODE ) = CZERO
        GAMBLN( MODE ) = CZERO
                                                                        
        IF ( BCTOPR .EQ. 'A' )   GAMTLN( MODE ) = GAMTR                    
        IF ( BCBOTR .EQ. 'A' )   GAMBLN( MODE ) = GAMBR                    
                                                                        
C      END IF                                                           
                                                                        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
                                                                        
                                                                        
 9000 CONTINUE                                                          
                                                                        
                                                                        
        DO 8300   MODE = 1, MR                                          
        PHITL(MODE)= PHITLN(MODE)                                       
        PHIBL(MODE)= PHIBLN(MODE)                                       
        GAMTL(MODE)= GAMTLN(MODE)                                       
        GAMBL(MODE)= GAMBLN(MODE)                                       
 8300   CONTINUE                                                        
                                                                        
        DEPTTL= DEPTTR                                                
        DEPTBL= DEPTBR                                                
                                                                        
                                                                        
      CLOSE( MODFIL )                                                   
                                                                        
C      WRITE(LUPRT,200)  RCOUPL(IPROF)*1.0E-3, RFILE(IPROF-1),
C     &                  RFILE(IPROF), SUM2
      IF(FLAGPU .LT. 1.0)   WRITE(LUPRT,200)  RCOUPL(IPROF)*1.0E-3
                                                                        
C        CALL STATST( A, MODAVR, MR, SUM2 )
        CALL STATST( A, MR, SUM2 )                             
                                                                        
                                                                        
      CPUEND= ETIME(CPSEC)                                       
      CPNEWP= CPNEWP + CPUEND - CPUBEG                                  
C ***********************************************                       
      ML= MR                                                            
C ***********************************************                       
                                                                        
      RETURN                                                            
      END
