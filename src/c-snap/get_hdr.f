C  GET_HDR.FOR                                                          
      SUBROUTINE GETHDR( SD, NSD, RD, NRD,              
     &   WSD, ISD, WRD, IRD,
     &   ZT, M, FREQ,                                
     &   NTOT, NMAT, MATER, NMEDIA,                            
     &   KTOP2, DEPTHT, BCTOP,                                          
     &   KBOT2, DEPTHB, BCBOT)                                          
                                                                        
                                                                        
C     Read in modes and extract values at rcvr depths                   
                                                                        
C     INPUT:                                                            
C        RD     vector of receiver depths where modes are to be evaluate
C        NRD number of such receivers                                   
C        SD     vector of SOURCE depths where modes are to be evaluated 
C        NSD    number of such receivers                                
                                                                        
C     OUTPUT:                                                           
C        M      number of modes                                         
C        FREQ   frequency                                               
                                                                        
                                                                        
      PARAMETER ( MAXMED = 50 )                                         
      INCLUDE 'param.inc'                                               
                                                                        
      INTEGER   IRD( * ), ISD( * )                         
                                                                        
      REAL      RD( * ), ZT( * ), WRD( MAXNRD ), SD( * ),WSD( MAXNSD ), 
     &          DEPTH( MAXMED ), RHO( MAXMED )                          
      COMPLEX CST, CSB
      COMPLEX   KTOP2, KBOT2           
      CHARACTER MATER( MAXMED )*8,           
     &          BCTOP*1, BCBOT*1                             
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
 
  300 FORMAT(1X,/,'  WARNING FROM SUB GET_HDR: possible loss of',
     &            ' accuracy as',/,
     & '  modes are not tabulated near requested pt. (',F8.2,' m)',/,
     & '  Nearest depths: ',F8.2,' m',6X,F8.2,' m',/,
     & '  Default tolerance (TOL = 1500.0 / ( 6.0 * FREQ ) :',
     & F8.2,' m')


      IF ( NRD .GT. MAXNRD ) THEN                                       
         WRITE( LUPRT, * ) ' FATAL ERROR: Too many receiver depths'    
         WRITE( LUPRT, * ) ' MAX ALLOWED NUMBER IS ', MAXNRD           
         WRITE( LUPRT, * ) ' REQUESTED QUANTITY IS ', NRD              
         STOP               ' FATAL ERROR: Too many receiver depths'    
      ENDIF                                                             
                                                                        
C     *** Read the header data from the mode file ***                   
                                                                        
      CALL MODHDR( FREQ, NMEDIA, NTOT, NMAT,
     &   MATER, DEPTH, RHO,                                          
     &   BCTOP, CST, RHOT, DEPTHT,                                           
     &   BCBOT, CSB, RHOB, DEPTHB,                                           
     &   M, ZT, KTOP2, KBOT2 )
C      PRINT *, 'RESULTING N, M :',NTOT, M                              
                                                                        
C     *** Locate indices of receiver points ***                         
                                                                        
      CALL WEIGHT( ZT, NTOT, RD, NRD, WRD, IRD )                        
                                                                        
C     *** Loop over receiver depths to check for safe interpolation *** 
                                                                        
C     Receivers must be within                                          
C     a fraction of a wavelength of tabulated pts.                      
                                                                        
      TOL = 1500.0 / ( 6.0 * FREQ ) 
                                                                        
      DO 1000 IR = 1, NRD                                               
                                                                        
         IZ = IRD( IR )                                                 
         WT = ABS( MIN( WRD( IR ), 1.0 - WRD( IR ) ) )                  
                                                                        
         IF ( RD( IR ) .LT. DEPTHT ) THEN                               
C           ------ Rcvr in upper halfspace                              
            IF ( CST .NE. 0.0 .OR. BCTOP(1:1) .NE. 'A' ) THEN           
               WRITE( *, * ) 'Receiver depth: ', RD( IR )               
               WRITE( *, * ) 'Highest valid depth: ', DEPTHT            
               STOP 'FATAL ERROR: Rcvr above highest valid depth'       
            ENDIF                                                       
                                                                        
         ELSE IF ( RD( IR ) .GT. DEPTHB ) THEN                          
C           ------ Rcvr in lower halfspace                              
cfmc 01/09/97  removed to fill the CONDR matrix in a RD environment
c              when HMAX .gt. h0beg+h1beg 
c            IF ( CSB .NE. 0.0 .OR. BCBOT(1:1) .NE. 'A' ) THEN
c               WRITE( *, * ) 'Receiver depth: ', RD( IR )               
c               WRITE( *, * ) 'Lowest valid depth: ', DEPTHB             
c               STOP 'FATAL ERROR: Rcvr below lowest valid depth'        
cfmc 01/09/97            ENDIF   
                                                                        
         ELSE IF ( WT * ( ZT( IZ + 1 ) - ZT( IZ ) ) .GT. TOL ) THEN     
c            WRITE( *, * ) 'Nearest depths: ', ZT( IZ ), ZT( IZ + 1 )    
c            WRITE( *, * ) 'Tolerance: ', TOL                            
c            STOP 'FATAL ERROR: Modes not tabulated near requested pt.'  
            WRITE(LUPRT,300)  RD(IR), ZT(IZ), ZT(IZ+1), TOL
            print 300, RD(IR), ZT(IZ), ZT(IZ+1), TOL
         ENDIF                                                          
                                                                        
 1000 CONTINUE                                                          
                                                                        
      IF ( NSD .GT. MAXNSD ) THEN                                       
         WRITE( LUPRT, * ) ' FATAL ERROR: Too many source depths'      
         WRITE( LUPRT, * ) ' MAX ALLOWED NUMBER IS ', MAXNSD           
         WRITE( LUPRT, * ) ' REQUESTED QUANTITY IS ', NSD              
         STOP               ' FATAL ERROR: Too many source depths'      
      ENDIF                                                             
                                                                        
      CALL WEIGHT( ZT, NTOT, SD, NSD, WSD, ISD )                        
                                                                        
C     *** Loop over source depths to check for safe interpolation ***   
                                                                        
C     Sources must be within                                            
C     a fraction of a wavelength of tabulated pts.                      
                                                                        
      DO 1200 IS = 1, NSD                                               
                                                                        
         IZ = ISD( IS )                                                 
         WT = ABS( MIN( WSD( IS ), 1.0 - WSD( IS ) ) )                  
                                                                        
         IF ( SD( IS ) .LT. DEPTHT ) THEN                               
C           ------ Source in upper halfspace                            
            IF ( CST .NE. 0.0 .OR. BCTOP(1:1) .NE. 'A' ) THEN           
               WRITE( *, * ) 'Source depth: ', SD( IS )                 
               WRITE( *, * ) 'Highest valid depth: ', DEPTHT            
               STOP 'FATAL ERROR: Source above highest valid depth'     
            ENDIF                                                       
                                                                        
         ELSE IF ( SD( IS ) .GT. DEPTHB ) THEN                          
C           ------ Source in lower halfspace                            
            IF ( CSB .NE. 0.0 .OR. BCBOT(1:1) .NE. 'A' ) THEN           
               WRITE( *, * ) 'Source depth: ', SD( IS )                 
               WRITE( *, * ) 'Lowest valid depth: ', DEPTHB             
               STOP 'FATAL ERROR: Source below lowest valid depth'      
            ENDIF                                                       
                                                                        
         ELSE IF ( WT * ( ZT( IZ + 1 ) - ZT( IZ ) ) .GT. TOL ) THEN     
c            WRITE( *, * ) 'Nearest depths: ', ZT( IZ ), ZT( IZ + 1 )    
c            WRITE( *, * ) 'Tolerance: ', TOL                            
c            STOP 'FATAL ERROR: Modes not tabulated near requested pt.'  
            WRITE(LUPRT,300)  SD(IS), ZT(IZ), ZT(IZ+1), TOL
            print 300, SD(IS), ZT(IZ), ZT(IZ+1), TOL
         ENDIF                                                          
                                                                        
 1200 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
