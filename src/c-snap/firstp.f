C  FIRSTP.FOR                                                           
      SUBROUTINE FIRSTP( DELTAR, NL, MODE, EIGF,                        
     &   A, CK, CKTAIL, CPL,                                            
     &   BCTOP, KTOP2,                                            
     &   BCBOT, KBOT2 )                                           
                                                                        
                                                                        
C     Computes the pressure field along the interface.                  
C     Also returns information needed for the tails in the halfspaces   
                                                                        
      COMPLEX CZERO
      PARAMETER ( MAXMED = 50 )                                         
      PARAMETER ( CZERO= ( 0.0, 0.0 ), PI = 3.141592 )
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
C     N(1)= MH0 ; N(2)= MSED ;                                          
C      INTEGER    N( MAXMED )                                           
                                                                        
      CHARACTER*8 MODEL                                                   
      CHARACTER  BCBOT*1, BCTOP*1                                       
                                                                        
                                                                        
      DOUBLE PRECISION EIGF( * )                                        
                                                                        
                                                                        
      COMPLEX AEXT, CI                                                  
      COMPLEX    CK( * ), CKTAIL( * ), CPL( * ), A( * )                   
      COMPLEX KTOP2, KBOT2                                              
      COMPLEX SNAPCK                                                      
C      COMPLEX*16 PEKRT                                                 
      COMPLEX PEKRT                                                     
                                                                        
                                                                        
      COMMON /MODEL/ MODEL                                                
                                                                        
      DATA CI / (0.0, 1.0 )/                                            
                                                                        
                                                                        
                                                                        
C     *** Compute pressure at the interface ***                         
                                                                        
          AEXT= A( MODE ) * EXP( -CI * CK( MODE ) * DELTAR )            
          DO 2500 IZ = 1, NL                                            
C          CPL( IZ ) = CPL( IZ ) + AEXT * MODSET( IZ, MODE )            
          CPL( IZ ) = CPL( IZ ) + AEXT * SNGL(EIGF( IZ ))          
 2500     CONTINUE                                                      
                                                                        
C        ------ Halfspace information ------                            
                                                                        
                                                                        
C          PHITL( MODE ) = AEXT * MODSET(  1, MODE )                    
C          PHIBL( MODE ) = AEXT * MODSET( NL, MODE )                    
           PHITL( MODE ) = AEXT * SNGL(EIGF( 1 ))                       
C          PHITL( MODE ) = 0.0                                          
           PHIBL( MODE ) = AEXT * SNGL(EIGF( NL ))                      
                                                                        
          GAMTL( MODE ) = CZERO
          GAMBL( MODE ) = CZERO
                                                                        
          IF( MODEL(1:7) .EQ. 'C-SNAP ' )   THEN                           
            SNAPCK = CMPLX( REAL( CKTAIL( MODE ) )**2, 0.0 )               
          ELSE                                                             
            SNAPCK = CK( MODE ) ** 2                                       
          END IF                                                           
                                                                        
          IF ( BCTOP .EQ. 'A' )                                         
     &      GAMTL( MODE ) = PEKRT( SNAPCK - KTOP2 )                        
          IF ( BCBOT .EQ. 'A' )                                         
     &      GAMBL( MODE ) = PEKRT( SNAPCK - KBOT2 )                        
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
