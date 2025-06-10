C   GETONE.FOR                                                          
                                                                        
      SUBROUTINE GETONE( MODE, NTOT, WSD, ISD, WRD, IRD,           
C     &   N, MATER, NMEDIA, COMP, EIGF,     
     &   MATER, NMEDIA, EIGF,                                  
     &   KTOP2, DEPTHT, BCTOP,                                          
     &   KBOT2, DEPTHB, BCBOT,                                          
     &   SD, NSD, RD, NRD, CK, CKTAIL, PHIT, PHIS )                     
                                                                        
C     Read in a single eigenfuntion and extract receiver values         
C     Results are returned in PHIT                                      
                                                                        
      COMPLEX CZERO
      PARAMETER ( CZERO= ( 0.0, 0.0 ) )

      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
      LOGICAL    TUFLUK                                                 
      INTEGER    ISD( * ), IRD( * )                             
      REAL       SD( * ), RD( * ), WSD( * ), WRD( * )                   
      DOUBLE PRECISION EIGF( * )                                        
      COMPLEX    PHIS( * ), PHIT( * ),                                  
     &           GAMT, GAMB, KTOP2, KBOT2, SNAPCK,                      
     &           CK( * ), CKTAIL( * )                                   
C      COMPLEX*16 PEKRT                                                 
      COMPLEX PEKRT                                                     
C      CHARACTER  MATER( * )*8, COMP*( *), BCTOP*1, BCBOT*1       
      CHARACTER  MATER( * )*8, BCTOP*1, BCBOT*1              
      CHARACTER*8 MODEL                                                  

      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MODEL/ MODEL                                               
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
CX      CALL SINMOD( MODFIL, MODE, PHI, NMAT, LRECL )                      
                                                                        
                                                                        
                                                                        
C     *** Is there an elastiC medium in the problem? ***                
                                                                        
      TUFLUK = .FALSE.                                                  
                                                                        
      DO 2000 MED = 1, NMEDIA                                            
         IF ( MATER( MED ) .EQ. 'ELASTIC' ) TUFLUK = .TRUE.             
 2000 CONTINUE                                                           
                                                                        
C     ------ Extract the component specified by 'COMP'                  
                                                                        
      IF ( TUFLUK ) THEN                                                
C        CALL EXTRACT( PHI, MODE, N, MATER, NMEDIA, COMP )    
        PRINT *,' ERROR IN SUB GETONE '                                 
        PRINT *, ' SUB EXTRACT NOT IN ORDER '                           
        STOP                                                            
       END IF                                                           
                                                                        
C     *** Extract values at receiver depths ***                         
                                                                        
      GAMT = CZERO
      GAMB = CZERO
                                                                        
      IF( MODEL(1:7) .EQ. 'C-SNAP ' )   THEN                             
        SNAPCK = CMPLX( REAL( CKTAIL( MODE ) )**2, 0.0 )                
      ELSE                                                               
        SNAPCK = CK( MODE ) ** 2                                         
      END IF                                                             
      IF ( BCTOP(1:1) .EQ. 'A' ) GAMT = PEKRT( SNAPCK - KTOP2 )          
      IF ( BCBOT(1:1) .EQ. 'A' ) GAMB = PEKRT( SNAPCK - KBOT2 )          
C      IF ( BCTOP(1:1) .EQ. 'A' ) GAMT = PEKRT( CK( MODE )** 2 - KTOP2 ) 
C      IF ( BCBOT(1:1) .EQ. 'A' ) GAMB = PEKRT( CK( MODE )** 2 - KBOT2 ) 
                                                                        
      DO 3000 IR = 1, NRD                                                
                                                                        
         IF ( RD( IR ) .LT. DEPTHT ) THEN                               
C           ------ Rcvr in upper halfspace                              
C            PHIT( IR ) = MODSET( 1, MODE) *                            
            PHIT( IR ) = SNGL(EIGF( 1 )) *                              
     &                     EXP( -GAMT * ( DEPTHT - RD( IR  ) ) )        
         ELSE IF ( RD( IR ) .GT. DEPTHB ) THEN                          
C           ------ Rcvr in lower halfspace                              
C            PHIT( IR ) = MODSET( NTOT, MODE) *                         
            PHIT( IR ) = SNGL(EIGF( NTOT )) *                           
     &                     EXP( -GAMB * ( RD( IR ) - DEPTHB ) )         
         ELSE                                                           
            IZ = IRD( IR )                                              
C            PHIT( IR ) = MODSET( IZ, MODE) +                           
C     &                   WRD( IR ) * ( MODSET( IZ+1, MODE) -           
C     &                   MODSET( IZ, MODE) )                           
            PHIT( IR ) = SNGL(EIGF( IZ )) +                             
     &                   WRD( IR ) * ( SNGL(EIGF( IZ+1 )) -             
     &                   SNGL(EIGF( IZ )) )                             
         ENDIF                                                          
                                                                        
 3000 CONTINUE                                                           
                                                                        
      DO 4000 IS = 1, NSD                                                
                                                                        
         IF ( SD( IS ) .LT. DEPTHT ) THEN                               
C           ------ Source in upper halfspace                            
C            PHIS( MODE ) = MODSET( 1, MODE) *                          
            PHIS( MODE ) =  SNGL(EIGF( 1 )) *                           
     &                     EXP( -GAMT * ( DEPTHT - SD( IS  ) ) )        
         ELSE IF ( SD( IS ) .GT. DEPTHB ) THEN                          
C           ------ Source in lower halfspace                            
C            PHIS( MODE ) = MODSET( NTOT, MODE) *                       
            PHIS( MODE ) =  SNGL(EIGF( NTOT )) *                        
     &                     EXP( -GAMB * ( SD( IS ) - DEPTHB ) )         
         ELSE                                                           
            IZ = ISD( IS )                                              
C            PHIS( MODE ) = MODSET( IZ, MODE) +                         
C     &                   WSD( IS ) * ( MODSET( IZ+1, MODE) -           
C     &                   MODSET( IZ, MODE) )                           
            PHIS( MODE ) = SNGL(EIGF( IZ )) +                           
     &                   WSD( IS ) * ( SNGL(EIGF( IZ+1 )) -             
     &                   SNGL(EIGF( IZ )) )                             
         ENDIF                                                          
                                                                        
 4000 CONTINUE                                                           
                                                                        
      RETURN                                                            
                                                                        
      END                                                               
