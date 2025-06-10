C    FNDVEC.FOR                                                       
                                                                        
      SUBROUTINE FNDVEC( MODQTY, DH0, DSED, MY, ZZ )

C     Saving data for final computation of the eigenvector                                                                        
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
      DOUBLE PRECISION MY(MAXMSH,MODEN)                                 
      DOUBLE PRECISION H0, H1, CC0, CC1                                 
      DOUBLE PRECISION DH0(8), DSED(8)                                  
      DOUBLE PRECISION ZZ(NPOINT)                                       
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /G/ H0, H1                                                 
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MSHIST/ MH0(8), MSED(8), ICOUNT, USEOLD         
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /PARAM4/ MESHI, MESHN
                                                                        
                                                                        
  640 FORMAT(1X,/,'  ***  ERROR DETECTED IN SUB FNDVEC  ***',/,       
     & '  ****  TOO MANY DATA POINTS FOR ARRAY ZZ. *** ',/,            
     & '  ****  either select DZH0 > (H0+H1)/',I4,'  or increase',      
     & ' the array size.',/,                                            
     & '    Required number of data points    : ',I4,/,                 
     & '    Max allowed number of data points : ',I4,/,                 
     & '    H0           = ', F10.3,' m',/,                             
     & '    H1           = ', F10.3,' m',/,                             
     & '    allowed DZH0 > ', F10.3,' m',/,                             
     & '  ****  EXECUTION IS TERMINATED ***** ')                        
                                                                        

       LTOT= MH0(MESHI) + MSED(MESHI)                                   
C       NTOT= MAX( LTOT + 1, 100, 2*(MAXMOD-MINMOD)+1 )                  
                                                                        
       AH0(ARIGHT)= H0                                                  
       AH1(ARIGHT)= H1                                                  
       AMH0(ARIGHT)= MH0(MESHI) + 1                                     
       AMSED(ARIGHT)= MSED(MESHI)                                       
       IF(H1 .LE. 0.0)    THEN                                          
        NMEDIA = 1                                                      
                                                                        
       ELSE                                                             
         NMEDIA = 2                                                     
         AR1(ARIGHT)= R1                                                 
       END IF                                                           
                                                                        
                                                                        
C ********************************************************************* 
                                                                        
        ANMED(ARIGHT)= NMEDIA                                           
        ALTOT(ARIGHT)= LTOT +  1                                        
        AMINM(ARIGHT)= MINMOD                                           
        AC2(ARIGHT)= C2                                                 
        AC2S(ARIGHT)= C2S                                               
        AR2(ARIGHT)= R2                                                 
        AH0H1(ARIGHT)= SNGL(H0+H1)                                      
C ********************************************************************* 
                                                                        
                                                                        
       IF(LTOT+1 .LE. NPOINT)  THEN                                     
       
         ZZ( 1 ) = 0.0                                                    
         DO 1200   IZ= 1, MH0(MESHI)                                      
         ZZ( IZ + 1 )= DH0( MESHI ) * IZ * H0                             
 1200    CONTINUE                                                         
                                                                        
         ISTART = MH0( MESHI ) + 1                                        
         DO 1400   IZ= 1, MSED(MESHI)                                     
         ZZ( ISTART + IZ )= DSED( MESHI ) * IZ * H0 + H0                  
 1400    CONTINUE                                                         
       
       ELSE                                                             
         
         WRITE(LUPRT,640) NPOINT,LTOT+1, NPOINT,                        
     &   H0, H1, (H0+H1)/(NPOINT-1)                                     
         PRINT 640, NPOINT, LTOT+1, NPOINT,                             
     &   H0, H1, (H0+H1)/(NPOINT-1)                                     
         STOP                                                           
       
       END IF                                                           
C ********************************************************************* 
                                                                        
       MAXMOD=0                                                         
C       IFIN= MH0( MESHI ) + MSED ( MESHI )                              
                                                                        
       ADH0(ARIGHT)= DH0(MESHI)                                         
       ADSED(ARIGHT)= DSED(MESHI)                                       
                                                                        
       DO 2400   M=1,MODQTY                                             
        EIGVL(M)= MY(MESHI,M)                                           
        IF(MAXMOD .GT. 0)   GO TO 2600                                    
 2400 CONTINUE                                                          
      MAXMOD=MINMOD+MODQTY-1                                            
                                                                        
 2600 CONTINUE                                                          
                                                                        
C ********************************************************************* 
      AMQTY( ARIGHT )= MODQTY                                           
C ********************************************************************* 
                                                                        
      RETURN                                                            
      END                                                               
