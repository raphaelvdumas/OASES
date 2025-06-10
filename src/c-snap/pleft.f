C  PLEFT.FOR                                                            

C   PRESSURE MATCHING

      SUBROUTINE PLEFT( RHO, DEPTH, NMEDIA,              
     &   MR, CPL, P, NL, NR, NTOT,                    
     &   BCTOP, RHOT, KTOP2, DEPTTR,                                   
     &   BCBOT, RHOB, KBOT2, DEPTBR,                                   
     &   ML, FREQ )                                                     
                                                                        
                                                                        
                                                                        
C     Computes the pressure field along the interface.                  
C     Also returns information needed for the tails in the halfspaces   
                                                                        
C      PARAMETER ( MAXMED = 50, MAXNRD = 501 )                          
      PARAMETER ( MAXMED = 50 )                                         
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
C     N(1)= MH0 ;   N(2)= MSED ;
C      INTEGER    N( MAXMED )
C      REAL       RHOL( MAXMED )
      REAL       RHO( MAXMED ),
     &           DEPTH( MAXMED )
C      REAL       DEPTHL( MAXMED )
      COMPLEX CST, CSB
      COMPLEX    CPL( * ), P( * ), KTOP2, KBOT2
      CHARACTER  MATER( MAXMED ) * 8, BCBOT * 1, BCTOP * 1
                                                                        
  100 FORMAT(1H ,I8.8,'.DAT    ')                                         
                                                                        
                                                                        
                                                                        
C     *** Get modal info at end of last segment ***                     
C ************************************************************************* 
C      CURRNT= ALEFT                                                       
C ************************************************************************* 
      ML= AMQTY(CURRNT)                                            
      NL= ALTOT(CURRNT)                                            
      NLMAX= NL                                                        
      NMEDIA= ANMED(CURRNT)                                          
      DEPTTL= 0.0                                                     
C      DEPTHL(1)= 0.0                                                   
C      RHOTL= 0.0                                                       
C      RHOL(1)= 1.0                                                     
                                                                        
      IF(NMEDIA .GT. 1 )   THEN                                        
C        DEPTHL(2)= AH0(CURRNT)                                     
C        RHOL(2)= AR1(CURRNT)                                         
      END IF                                                           
                                                                        
C      RHOBL= AR2(CURRNT)                                           
      DEPTBL= AH0H1(CURRNT)                                        
                                                                        
      ZL(1)= 0.0
      DO 1200   IZ= 1, AMH0(ALEFT) - 1
      ZL(IZ+1)= ADH0(ALEFT)*IZ * AH0(ALEFT)
 1200 CONTINUE
      IF( AH1(ALEFT) .GT. 0.0 )   THEN
       ISTART= AMH0(ALEFT)
       DO 1400   IZ= 1, AMSED(ALEFT)
       ZL(ISTART+IZ)= ADSED(ALEFT)*IZ*AH0(ALEFT) + AH0(ALEFT)
 1400  CONTINUE
      END IF

                                                                        
C     *** Get modal data in new segment ***                             
C ************************************************************************* 
      CURRNT= ARIGHT                                                      
C ************************************************************************* 
      CALL MODHDR( FREQ, NMEDIA, NR, NMAT,
     &   MATER, DEPTH, RHO,                                          
     &   BCTOP, CST, RHOT, DEPTTR,                                          
     &   BCBOT, CSB, RHOB, DEPTBR,                                          
     &   MR, ZR, KTOP2, KBOT2 )                                 


C     *** Upslope? Extend the Z vector with data from ZL ***

      NTOT = NR

      DO 500 IZL = 1, NL                                                
         IF ( ZL( IZL ) .GT. ZR( NTOT ) ) THEN                          
            NTOT  = NTOT + 1                                            
            ZR( NTOT ) = ZL( IZL )                                      
         ENDIF                                                          
  500 CONTINUE                                                          
                                                                        
                                                                        
C     *** Retabulate the pressure on the new grid ***                   
                                                                        
      IZL = 1                                                           
      MED= 1                                                            
      RHOMED= RHO(1)                                                    
                                                                        
      DO 7000 IZ = 1, NTOT                                              
         ZT = ZR( IZ )                                                  
                                                                        
C        ------ Get medium density in right segment
         IF      ( ZT .LT. DEPTTR ) THEN                               
            RHOMED = RHOT                                               
         ELSE IF ( ZT .GT. DEPTBR ) THEN                               
            RHOMED = RHOB                                               
         ELSE IF ( MED .LT. NMEDIA ) THEN                               
            IF   ( ZT .GT. DEPTH( MED + 1 ) ) THEN                      
               MED    = MED + 1                                         
               RHOMED = RHO( MED )                                      
            ENDIF                                                       
         ENDIF                                                          
                                                                        
 4000    IF ( ZT .GT. ZL( IZL + 1 ) .AND. IZL .LT. NL - 1 ) THEN        
            IZL = IZL + 1                                               
            GOTO 4000                                                   
         ENDIF                                                          
                                                                        
C        ------ Calculate P at that depth                               
                                                                        
         IF      ( ZT .GT. DEPTBL ) THEN                               
            IF ( BCBOT .EQ. 'A' ) THEN                                  
               P( IZ ) = CMPLX( 0.0, 0.0 )
               DO 5000 MODE = 1, ML                                     
                  P( IZ ) = P( IZ ) + PHIBL( MODE ) *                   
     &                 EXP( -GAMBL( MODE ) * ( ZT - DEPTBL ) )         
 5000          CONTINUE                                                 
               NLMAX= NLMAX + 1                                         
               CPL(NLMAX)= P(IZ)                                        
               ZL(NLMAX)= ZT                                            
            ENDIF                                                       
         ELSE IF ( ZT .LT. DEPTTL ) THEN                               
            IF ( BCTOP .EQ. 'A' ) THEN                                  
               P( IZ ) = CMPLX( 0.0, 0.0 )
               DO 6000 MODE = 1, ML                                     
                  P( IZ ) = P( IZ ) + PHITL( MODE ) *                   
     &                 EXP( -GAMTL( MODE ) * ( DEPTTL - ZT ) )         
 6000          CONTINUE                                                 
            ENDIF                                                       
         ELSE                                                           
                                                                        
                                                                        
            I1= MAX(1,IZL-1)                                            
            I1= MIN(I1,NL-3)                                            
            CALL LAGCMP( 4, ZL(I1), CPL(I1), ZT, P(IZ) )              
         ENDIF                                                          
                                                                        
         IF ( IZ .EQ. 1 ) THEN                                          
C           ------ First point                                          
            H = 0.5 * ( ZR( 2      ) - ZR( 1        ) ) / RHOMED        
         ELSE IF ( IZ .EQ. NTOT ) THEN                                  
C           ------ Last point                                           
            H = 0.5 * ( ZR( NTOT   ) - ZR( NTOT - 1 ) ) / RHOMED        
         ELSE                                                           
            IF ( ZR( IZ - 1 ) .LT. DEPTH( MED + 1 ) .AND.               
     &           ZR( IZ + 1 ) .GE. DEPTH( MED + 1 ) ) THEN              
C              ------ Point just above or below the interface           
               H = 0.5 * ( ZR( IZ + 1 ) / RHO( MED + 1 )                
     &                   - ZR( IZ - 1 ) / RHO( MED     )                
     &                   - DEPTH( MED + 1 ) / RHO( MED + 1 )            
     &                   + DEPTH( MED + 1 ) / RHO( MED     ) )          
            ELSE                                                        
               H = 0.5 * ( ZR( IZ + 1 ) - ZR( IZ   - 1 ) ) / RHOMED     
            ENDIF                                                       
         ENDIF                                                          
                                                                        
         P( IZ ) = H * P( IZ )                                          
 7000 CONTINUE                                                          
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
