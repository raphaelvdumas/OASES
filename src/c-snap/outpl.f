C   OUTPL.FOR                                                           
      SUBROUTINE OUTPL(IOP, SECD, TLOSS, FLAG, AX, AY, SD, RD,          
     & NOPT, ICF, FREQ, NP, TITIN, NC, COHINC)                          
                                                                        
      CHARACTER*3 XTYP, YTYP, COHINC                                    
      CHARACTER*30 DLAB(10)                                             
      CHARACTER*80 TITIN, TITXUP                                        
      CHARACTER*80 TITLE, TITLEX, TITLEY, OPTION                        
                                                                        
      REAL SECD(NOPT,3), TLOSS(1)                                       
      REAL FLAG(NOPT,ICF), AX(NOPT,6), AY(NOPT,7)                       
                                                                       
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /TITL/ TITLE, TITLEX, TITLEY, XTYP, YTYP                   
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
                                                                        
                                                                        
  300 FORMAT(1H ,F10.2,2X,F11.2)                                        
  400 FORMAT(1H ,///,'   TRANSMISSION LOSS VERSUS RANGE BY COHERENT',
     & ' ADDITION OF MODES',//,'   FREQUENCY      =',F9.1,' Hz',               
     & /,'   SOURCE DEPTH   =',F9.1,' m',/,'   RECEIVER',
     & ' DEPTH =',F9.1,' m',//,'   RANGE(km)      TL(dB)')                      
  401 FORMAT(1H ,///,'   TRANSMISSION LOSS VERSUS RANGE BY INCOHERENT',
     & ' ADDITION OF MODES',//,'   FREQUENCY      =',F9.1,' Hz',               
     & /,'   SOURCE DEPTH   =',F9.1,' m',/,'   RECEIVER',
     & ' DEPTH =',F9.1,' m',//,'   RANGE(km)      TL(dB)')                      
  402 FORMAT(1H1)                                                       
  820 FORMAT('Range (km)$  ')                                           
  840 FORMAT('Loss (dB)$')                                              
  850 FORMAT('SD= ',F7.1,'m$')                                          
  860 FORMAT('RD= ',F7.1,'m$')                                          
  870 FORMAT('F = ',F7.1,'Hz$')                                         
  880 FORMAT('C-SNAP TLRAN ',A3)                                        
                                                                        

      PRT=FLAG(IOP,2)                                                   
      PLT=FLAG(IOP,3)                                                   

      IF(PLT.GT.0.)   THEN                                              
       OPTION=' '                                                       
       TITXUP= TITIN                                                    
       TITLE= ' '                                                       
       XDIV=1.0E-3                                                      
       YDIV=1.0                                                         
       XTYP='LIN'                                                       
       YTYP='LIN'                                                       
       WRITE(TITLEX,820)                                                
       WRITE(TITLEY,840)                                                
       WRITE(DLAB(1),870)   FREQ                                        
       DX= SECD(IOP,3)                                                  
       DY=0.0                                                           
       XOFF= SECD(IOP,1)                                                
       YOFF=0.0                                                         
      END IF                                                            

      IF(PRT.GT.0.0)   WRITE(LUPRT,402)                                 
      WRITE(DLAB(2),850)   SD                                           
      WRITE(DLAB(3),860)   RD                                           
                                                                        
      IF(PRT.LT.1.0)   GO TO 2800                                       
      IF( COHINC .EQ. 'COH' )   THEN
        WRITE(LUPRT,400)   FREQ,SD,RD                                     
      ELSE
        WRITE(LUPRT,401)   FREQ,SD,RD           
      END IF
      RMARCH= SECD(IOP,1)*1.0E-3                                        
      DELTAR= SECD(IOP,3)*1.0E-3                                        
      DO  2600   L=1,NP                                                 
      RKM= RMARCH + (L-1)*DELTAR                                        
 2600 WRITE(LUPRT,300)   RKM,TLOSS(L)                                   
 2800 IF(PLT.LT.1.0)   GO TO 3000                                       
                                                                        
      WRITE(OPTION,880) COHINC                                          
                                                                        
      CALL PLTFIL(TITXUP,          
     &             0,               
     &             OPTION,          
     &             3,               
     &             NC,               
     &             (AX(IOP,2)-AX(IOP,1))/AX(IOP,3), 
     &             (AY(IOP,2)-AY(IOP,1))/AY(IOP,3), 
     &             AX(IOP,1),       
     &             AX(IOP,2),       
     &             AX(IOP,4),       
     &             AY(IOP,2),       
     &             AY(IOP,1),       
     &             AY(IOP,4),       
     &             DLAB)

      CALL PLTF1(1,               
     &           NP,              
     &           NINT(AX(IOP,6)), 
     &           XOFF,            
     &           YOFF)            
      CALL PLTF2(TLOSS,1,NP)                                            
 3000 CONTINUE                                                          
      RETURN                                                            
      END                                                               
