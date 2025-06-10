C     PRSDEP.FOR 
                                                                        
      SUBROUTINE PRSDEP( SD, hbeam, ABSPR, dpth, DZH0, DZH1,
     & NW, NB, FREQ )
C     & FLAG, AX, AY, NOPT, ICF )               
                                                                        
      CHARACTER*3 XBTYPE, YBTYPE, COHINC                                
      CHARACTER*4 TITLE(20)
      CHARACTER*6 TLDB                                                  
      CHARACTER*30 DLAB(10)                                             
      CHARACTER*80 TITXUP, TITLEX, TITLEY, OPTION                       
                                                                        
      DOUBLE PRECISION DZH0, DZH1

      REAL  ABSPR( * ), dpth( * )                   
C      REAL FLAG(NOPT,ICF), AX(NOPT,6), AY(NOPT,7)                       
      REAL AX(1,6), AY(1,7)                       
                                                                        
      COMMON /FLAGS/ PLANE, NOVOL
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT         
      COMMON /REC1/ TITLE
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
      COMMON /TITL/ TITXUP, TITLEX, TITLEY, XBTYPE, YBTYPE              

      DATA COHINC/'   '/
      DATA TLDB/'PRESSR'/                                               
                                                                        
  300 FORMAT(1H1,///,'   GAUSSIAN SOURCE FIELD',
     & //,'   FREQUENCY    =',F8.1,' Hz',                
     &  /,'   HALF BEAM    =',F8.1,' deg',//)
  310 FORMAT(1H ,' SOURCE DEPTH:',1X,F8.1,'  m')                    
  320 FORMAT(1H ,/,'   DEPTH(m)',10X,10(A6,6X))                         
  400 FORMAT(1H ,1X,F7.2,5X,5(4X,F8.2))                                 
  820 FORMAT('Loss (dB)$')                                              
  840 FORMAT('Depth (m)$')                                              
  850 FORMAT('F = ',F7.1,'Hz$')
  860 FORMAT('TH= ',F7.1,'deg$')
  870 FORMAT('SD= ',F7.1,'m$')
  880 FORMAT('C-SNAP TLDEP ',A3)                                        
                                                                        

C  DEFINITION OF CONSTANTS.                                             

       TITXUP(1:80)=' '
       TITXUP(1:22)= 'GAUSSIAN SOURCE FIELD '

C
C      PRT=FLAG(IOP,2)                                                   
C      PLT=FLAG(IOP,3)                                                   
C                                                                        
      prt= 0.0
      PLT= 1.0
      IF(PLT .GT. 0.0)   THEN                                           
       DX=0.0                                                           
       XOFF=0.0                                                         
       XBTYPE='LIN'                                                     
       XDIV=1.0                                                         
       WRITE(TITLEX,820)                                                
       DY= 0.
       YOFF= 0.0
       YBTYPE='LIN'                                                     
       YDIV=1.0                                                         
       WRITE(TITLEY,840)                                                
       WRITE(DLAB(1),850)   FREQ                                        
       OPTION='COH '                                                       
       WRITE(OPTION,880)   COHINC                                       
       WRITE(DLAB(3),870)   SD                                          
      END IF                                                            
                                                                        
      RKM= 0.0

C      PRMIN=  1.0E38
CPR      PRMAX= -1.0E38
      DO 2000   ID= 1, NW
      dpth(ID)= (ID-1)*DZH0
      ABSPR(ID)= -20.0*ALOG10(MAX(1.0E-30,ABSPR(ID)) )
CPR      PRMAX= MAX(PRMAX, ABSPR(ID))
 2000 CONTINUE
      H0= (NW-1)*DZH0                                                        
      DO 2100   ID= NW+1, NW+NB
      dpth(ID)= H0 + (ID-1-NW)*DZH1
      ABSPR(ID)= -20.0*ALOG10( MAX(1.0E-30,ABSPR(ID)) )
CPR      PRMAX= MAX(PRMAX, ABSPR(ID))
 2100 CONTINUE                                                        
                                                                              
       

      IF( PRT .GT. 0.0 )   THEN                                         
                                                                        
        WRITE(LUPRT,300)   FREQ, RKM                                  
                                                                        
        WRITE(LUPRT,310)   SD                                           
        WRITE(LUPRT,320)   TLDB                                         
                                                                        
        DO 2300   ID= 1, NW+NB
        WRITE(LUPRT,400)   dpth(ID),  ABSPR(ID)
 2300   CONTINUE
      END IF



      IOP= 1
CFR      PRMAX= 200.
      AX(IOP,1)= 0.0
      AX(IOP,2)= 200.0
      AX(IOP,3)= 16.0 
      AX(IOP,4)= ABS(AX(IOP,2)-AX(IOP,1))/10.0
      AY(IOP,1)= 0.0
      AY(IOP,2)= dpth(NW+NB)
      IF(AY(IOP,2) .GT. 0.0 )   THEN
        I=NINT(AY(IOP,2))
        AY(IOP,2)= I
      END IF
      AY(IOP,3)= 12.0 
      AY(IOP,4)= AY(IOP,2)/4


                                                                        
      IF( PLT .GT. 0.0)   THEN                                          
        WRITE(DLAB(2),860)   hbeam
        CALL PLTFIL(TITLE,         
     &             0,               
     &             OPTION,          
     &             3,               
     &             1,               
     &             AX(IOP,3),
     &             AY(IOP,3),
     &             AX(IOP,2),       
     &             AX(IOP,1),       
     &             AX(IOP,4),       
     &             AY(IOP,2),       
     &             AY(IOP,1),       
     &             AY(IOP,4),       
     &             DLAB)


        CALL PLTF1(1,               
     &           NW+NB,
     &           1,                 
     &           XOFF,              
     &           YOFF)              
        CALL PLTF2(ABSPR, 1, NW + NB )
        CALL PLTF2(dpth, 1, NW + NB )
      END IF                                                            
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
