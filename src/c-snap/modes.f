      SUBROUTINE MODES( FREQ, TITLE, AX, AY,                         
     & MODPLT, XTS, MSP, IOP, FLAG, NOPT, ICF,                                  
     & ZSTEP, MODQTY,
     &             ALFA, MODAVR,
     &             ADA,  SPEED, EIGF,
     &             A3, B3, C3,
     &             EE, ZZ, SSOLD, EXCH, EK)
                                                                        
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
      CHARACTER*3 XBTYPE, YBTYPE
      CHARACTER*30 DLAB(10)
      CHARACTER*80 TITLE, TITLEX, TITLEY, OPTION, TITXUP
                                                                        
      LOGICAL EXCH(*), MODPLT
      REAL ALFA(*), MODAVR(*)
                                                                        
      REAL FLAG(NOPT,ICF), AX(NOPT,6), AY(NOPT,7)                       
      REAL XTS(12,MSP)
                                                                        
      DOUBLE PRECISION DH0I, DSEDI, EKM
      DOUBLE PRECISION FRQ
      DOUBLE PRECISION H0, H1                                           
                                                                        
      DOUBLE PRECISION ADA(*), SPEED(*), EIGF(*)
      DOUBLE PRECISION A3(*), B3(*), C3(*)
      DOUBLE PRECISION EE(*), ZZ(*), SSOLD(*)
      DOUBLE PRECISION EK(*)

      COMMON /DENS/ R0, R1, R2
      COMMON /G/ H0, H1
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /XAXN/ DX,XDIV                                             
      COMMON /YAXN/ DY,YDIV                                             
      COMMON /TITL/ TITXUP,TITLEX,TITLEY,XBTYPE,YBTYPE                  
                                                                        
  320 FORMAT(1X,5(F10.7,2X))                                            
  420 FORMAT(I6,3X,' MININUM ORDER MODE',/,                             
     & I6,3X,' NUMBER OF MESH POINTS IN THE WATER LAYER  ',/,           
     & I6,3X,' NUMBER OF MESH POINTS IN THE SEDIMENT LAYER ')           
  820 FORMAT(1H ,'Mode amplitude$')                                     
  840 FORMAT(1H ,'Depth (m)$')                                          
  850 FORMAT(1X,F9.3)                                                   
C                                                                       
                                                                        
      PRT= FLAG(IOP,2)
      FRQ= DBLE( FREQ )

      CURRNT= ARIGHT
      MH0I= AMH0(CURRNT) - 1
      MSEDI= AMSED(CURRNT)
      DH0I= ADH0(CURRNT)
      DSEDI= ADSED(CURRNT)
      NPH0= MH0I + 1
C      NPH1= MSEDI

c      CALL GETHDG( RANGE, COMP, MODFIL,                                
c     &              H0G, R0G, H1G, R1G,
c     &              MINMD, MODQTY, LRECL, NTOT, NPH0, NPH1)                  

                                                                        
c      H0= H0G                                                           
c      H1= H1G                                                           
c      R0= R0G                                                           
c      R1= R1G                                                           
                                                                        
      IF( H1 .GT. 0. .AND. R1 .EQ. 0. )   THEN                          
       WRITE(LUPRT,*) ' ERROR IN SUB MODES : H1,R1 ? '                      
      END IF
C      MH0I= NPH0 - 1
C      MSEDI= NPH1
      MINMD= MINMOD
      MAXMD= MINMD + MODQTY - 1                                               
                                                                        
      IF( MODPLT )   THEN                                     
        WRITE(LUPLT,420) MINMD, MH0I, MSEDI                             
        WRITE(DLAB(1),850) FREQ                                         
        WRITE(DLAB(2),850) H0                                           
        WRITE(DLAB(3),850) H1                                           
        WRITE(DLAB(4),850) R1                                           
        XLEFT=AX(IOP,1)                                                 
        XRIGHT=AX(IOP,2)                                                
        XSCALE=AX(IOP,3)                                                
c        XINC=AX(IOP,4)                                                  
        YUP=AY(IOP,1)                                                   
        YDOWN=AY(IOP,2)                                                 
        YSCALE=AY(IOP,3)                                                
c        YINC=AY(IOP,4)                                                  
        XBTYPE='LIN'                                                    
        YBTYPE='LIN'                                                    
        XDIV=1.0                                                        
        YDIV=1.0                                                        
c        HGT=0.14                                                        
        MODQTY=MAXMD-MINMD+1                                            
        WRITE(TITLEX,820)                                               
        WRITE(TITLEY,840)                                               
c        XOFF=0.0                                                        
c        YOFF=0.0                                                        
        DX=0.0                                                          
        DY=0.0                                                          
        TITXUP=' '                                                      
        OPTION=' '                                                      
        OPTION(1:12)='C-SNAP MODES'                                     
        CALL PLTFIL(TITLE,         
     &               0,             
     &               OPTION,        
     &               4,             
     &               MODQTY,        
     &               (XRIGHT-XLEFT)/XSCALE, 
     &               (YDOWN-YUP)/YSCALE,    
     &               AX(IOP,1),     
     &               AX(IOP,2),     
     &               AX(IOP,4),     
     &               AY(IOP,2),     
     &               AY(IOP,1),     
     &               AY(IOP,4),     
     &               DLAB)
      END IF                                                            
                                                                        
                                                                        
C     *** Compute the modes ***                                         
                                                                        

       EIGF( 1 )= 0.0



      DO 5900 MODE = 1, MODQTY


       CALL EIGVEC( MODE, MH0I, MSEDI, DH0I, DSEDI,
     &             FRQ, ALFA(MODE),
     &             MINMOD, MODAVR,
     &             ADA, SPEED, EIGF(2),
     &             A3, B3, C3, EE, ZZ, SSOLD, EXCH,
     &             EKM, EIGVL(MODE), EK(MODE) )

c **********
                                                                        
      NTOT= NPH0 + MSEDI

      IF( MODPLT )   THEN                                     
C       DO 2400   I1=1, NPH0, 51                                        
        DO 2400   I1=2, NPH0, 51                                        
        I2= MIN0(I1+51-1, NPH0)                                         
        WRITE(LUPLT,320) (EIGF(JK), JK= I1,I2)                          
 2400   CONTINUE                                                        
        IF( MSEDI .GT. 0)   THEN                                        
          DO 2600   I1= NPH0 + 1, NTOT, 51                              
          I2= MIN0(I1+51-1, NTOT )                                      
          WRITE(LUPLT,320) (EIGF(JK)/R1, JK= I1,I2)                        
 2600     CONTINUE                                                      
        END IF                                                          
      END IF                                                            
                                                                        
      IF( PRT .GT. 0.0)   THEN                                          
        CALL MODPRT( ZSTEP, XTS, EIGF(2), MH0I, MSP, FREQ,               
     &  MINMD, MAXMD, MODE+MINMOD-1 )                                      
      END IF                                                            
                                                                        
 5900 CONTINUE                                                          
                                                                        
      RETURN                                                            
                                                                        
      END                                                               
