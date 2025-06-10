C    TLDEP_CP.FOR                                                       
                                                                        
      SUBROUTINE TLDEPC( SD, RNG, PRSS, NRCVRS, NR, RDAR,             
     & ISHIFT, FREQ, TITLE, IOP, TLOSS, INDX, INDY, NINDX, NINDY,       
     & ZSTEP, FLAG, AX, AY, NOPT, ICF, COHINC )               
C                                                                       
      INTEGER INDX(*), INDY(*)                                          
                                                                        
      CHARACTER*3 XBTYPE, YBTYPE, COHINC                                
      CHARACTER*4 TITLE(20)
      CHARACTER*6 TLDB                                                  
      CHARACTER*30 DLAB(10)                                             
      CHARACTER*80 TITXUP, TITLEX, TITLEY, OPTION                       
                                                                        
      REAL RNG( * ), TLOSS( * ), RDAR( * )                              
      REAL FLAG(NOPT,ICF), AX(NOPT,6), AY(NOPT,7)                       
                                                                        
      REAL PRSS( NRCVRS-ISHIFT, NR )                                    
                                                                        
      COMMON /FLAGS/ PLANE, NOVOL
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT         
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
      COMMON /TITL/ TITXUP, TITLEX, TITLEY, XBTYPE, YBTYPE              

      DATA TLDB/'TL(dB)'/                                               
                                                                        
  300 FORMAT(1H1,///,'   TRANSMISSION LOSS VERSUS DEPTH BY COHERENT',
     & ' ADDITION OF MODES.',//,'   FREQUENCY    =',F8.1,' Hz',                
     &    /,'   RANGE        =',F8.1,' km',//)
  301 FORMAT(1H1,///,'   TRANSMISSION LOSS VERSUS DEPTH BY INCOHERENT',
     & ' ADDITION OF MODES.',//,'   FREQUENCY    =',F8.1,' Hz',              
     &    /,'   RANGE        =',F8.1,' km',//)
  310 FORMAT(1H ,' SOURCE DEPTH:',1X,F8.1,'  m')                    
  320 FORMAT(1H ,/,'   DEPTH(m)',10X,10(A6,6X))                         
  400 FORMAT(1H ,1X,F7.2,5X,5(4X,F8.2))                                 
  820 FORMAT('Loss (dB)$')                                              
  840 FORMAT('Depth (m)$')                                              
  850 FORMAT('F = ',F7.1,'Hz$')                                         
  860 FORMAT('R = ',F7.1,'Km$')                                         
  870 FORMAT('SD= ',F7.1,'m$')                                          
  880 FORMAT('C-SNAP TLDEP ',A3)                                        
                                                                        

C  DEFINITION OF CONSTANTS.                                             
                                                                        
      PRT=FLAG(IOP,2)                                                   
      PLT=FLAG(IOP,3)                                                   
                                                                        
      IF(PLT .GT. 0.0)   THEN                                           
       DX=0.0                                                           
       XOFF=0.0                                                         
       XBTYPE='LIN'                                                     
       XDIV=1.0                                                         
       WRITE(TITLEX,820)                                                
       DY= ZSTEP                                                        
       YOFF= RDAR(INDY(1)+ISHIFT)                                       
       YBTYPE='LIN'                                                     
       YDIV=1.0                                                         
       WRITE(TITLEY,840)                                                
       WRITE(DLAB(1),850)   FREQ                                        
       TITXUP=' '                                                       
       OPTION=' '                                                       
       WRITE(OPTION,880)   COHINC                                       
       WRITE(DLAB(3),870)   SD                                          
      END IF                                                            
                                                                        
      DO 7000   IRX= 1, NINDX                                           
      IR= INDX(IRX)                                                     
      RKM=RNG(IR)*1.0E-3                                                
      DO 2000   KK = 1, NINDY-ISHIFT
      TLOSS(KK)= PRSS(INDY(KK),IR)                                      
 2000 CONTINUE                                                          
                                                                        
                                                                              
      IF( PRT .GT. 0.0 )   THEN                                         
C        IF( ISHIFT .EQ. 1 )   TLOSS(1)= 1.0E38                         
                                                                        
        IF( COHINC .EQ. 'COH' )   THEN                                  
          WRITE(LUPRT,300)   FREQ, RKM                                  
        ELSE                                                            
          WRITE(LUPRT,301)   FREQ, RKM                                  
        END IF                                                          
                                                                        
        WRITE(LUPRT,310)   SD                                           
        WRITE(LUPRT,320)   TLDB                                         
                                                                        
        IF(ISHIFT .EQ. 1)   WRITE(LUPRT,400) RDAR(1)                    
        DO 2300   ID= 1, NINDY-ISHIFT                                   
        WRITE(LUPRT,400)   RDAR(INDY(ID+ISHIFT)), TLOSS(ID)             
 2300   CONTINUE                                                        
      END IF                                                            
                                                                        
      IF( PLT .GT. 0.0)   THEN                                          
        WRITE(DLAB(2),860)   RNG(IR)*1.0E-3                             
        CALL PLTFIL(TITLE,         
     &             0,               
     &             OPTION,          
     &             3,               
     &             1,               
     &             (AX(IOP,2)-AX(IOP,1))/AX(IOP,3), 
     &             (AY(IOP,2)-AY(IOP,1))/AY(IOP,3), 
     &             AX(IOP,2),       
     &             AX(IOP,1),       
     &             AX(IOP,4),       
     &             AY(IOP,2),       
     &             AY(IOP,1),       
     &             AY(IOP,4),       
     &             DLAB)
C                                                                       
C                                                                       
        CALL PLTF1(1,               
     &           NRCVRS-ISHIFT,     
     &           1,                 
     &           XOFF,              
     &           YOFF)              
        CALL PLTF2(TLOSS, 1, NRCVRS-ISHIFT )                            
C        CALL PLTF2(RDAR, 1+ISHIFT, NRCVRS)                             
      END IF                                                            
                                                                        
 7000 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
