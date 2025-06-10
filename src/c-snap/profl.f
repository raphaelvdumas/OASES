C  PROFL.FOR                                                            
                                                                        
      SUBROUTINE PROFL( IOP, NSECT, RPROF, C0, Z0, C1, Z1,              
     & AX, AY, AS, NOPT, TITLE, ZY, CX )                                
                                                                        
      CHARACTER*3 XBTYPE, YBTYPE, ZBTYPE                                
      CHARACTER*30 DLAB(10)                                             
      CHARACTER*80 TITLE, TITLEX, TITLEY, TITLEZ, TITXUP, OPTION        
                                                                        
      REAL RPROF( * )                                                   
C      REAL BETA(-1:3), SCATT(2)                                         
      REAL AX(NOPT,6), AY(NOPT,7), AS(5)                                
      REAL ZY( * ), CX( * )                                             
                                                                        
C      DOUBLE PRECISION CC0, CC1                                 
      DOUBLE PRECISION H0, H1
      DOUBLE PRECISION C0( * ), C1( * ), Z0( * ), Z1( * )               
                                                                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /TITL/ TITXUP,TITLEX, TITLEY, XBTYPE, YBTYPE               
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
      COMMON /ZAXC/ TITLEZ, ZBTYPE                                      
      COMMON /ZAXN/ DZ, ZDIV, ZMIN, ZMAX, ZLEN, ZINC                    
                                                                        
  110 FORMAT(1H ,//,'  REVISE Y AXIS FOR OPTION "PROFL".')              
  120 FORMAT(1H ,//,'   ERROR IN PROFL SUBROUTINE ')                    
  200 FORMAT(1H ,///,' *** REGION N. ',I4,' STARTING AT ',F10.4,' km',  
     & '   H0, H1 : ',2F10.2)                                           
  220 FORMAT('SV (m/s)         $')                                      
  240 FORMAT('Range (km)$')                                             
  260 FORMAT('Depth (m)$')                                              
                                                                        
                                                                        
                                                                        
                                                                        
      IF( NSECT .EQ. 1 )   THEN                                         
        CALL PROFRI( IOP, C0, Z0, C1, Z1, AS, AY, NOPT, TITLE,        
     &                 ZY, CX )                                         
        RETURN                                                          
      END IF                                                            
                                                                        
                                                                        
                                                                        
      TITXUP=' '                                                        
      OPTION=' '                                                        
                                                                        
      XBTYPE='LIN'                                                      
      YBTYPE='LIN'                                                      
      ZBTYPE='LIN'                                                      
                                                                        
      XDIV=1.0                                                          
      YDIV=1.0                                                          
      ZDIV=1.0                                                          
                                                                        
      WRITE(TITLEZ,220)                                                 
      WRITE(TITLEX,240)                                                 
      WRITE(TITLEY,260)                                                 
                                                                        
                                                                        
                                                                        
      XLEFT= AX(IOP,1)                                                  
      XRIGHT= AX(IOP,2)                                                 
      XLEN= AX(IOP,3)                                                   
       XINC= AX(IOP,4)                                                   
                                                                        
      YUP= AY(IOP,1)                                                    
      YDOWN= AY(IOP,2)                                                  
      YLEN= AY(IOP,3)                                                   
      YINC= AY(IOP,4)                                                   
                                                                        
      ZMIN= AS(1)                                                       
      ZMAX= AS(2)                                                       
      ZLEN= AS(3)                                                       
      ZINC= AS(4)                                                       
                                                                        
      DX= 0.0                                                           
      DY= 0.0                                                           
      DZ= 0.0                                                           
                                                                        
      YOFF= 0.0                                                         
                                                                        
      DLAB(1)=' '                                                       
                                                                        
      OPTION(1:16)='C-SNAP PROFL,RDP'                                   
      NLAB= 0                                                           
      IGRID= 0                                                          
      CALL PLTFIL(TITLE, IGRID, OPTION, NLAB, NSECT, XLEN, YLEN,       
     & XLEFT, XRIGHT, XINC, YDOWN, YUP, YINC, DLAB)
                                                                        
                                                                        
C   INPUT PARAMETERS ARE READ FROM FILE 10                              
                                                                        
      REWIND 10                                                         
C      READ(10) MINMOD, MAXMOD                                           
      READ(10) IDUMMY

      DO 6000   ISECT= 1, NSECT                                         
                                                                        
C      READ(10) RKM                                                      
       READ(10) DUMMY
C      READ(10) R1, R2, H0, H1, ND0, ND1                                 
       READ(10) DUMMY, DUMMY, H0, H1, ND0, ND1
C      READ(10) BETA(1), BETA(2), BETA(3), SCATT(1), SCATT(2), CC0, CC1  
       READ(10) DUMMY

C   SVP IN WATER COLUMN                                                 
      READ(10) (Z0(I), C0(I), I= 1, ND0)                                
      DO 1200   I=1,ND0                                                 
      ZY(I)= Z0(I)                                                      
      CX(I)= C0(I)                                                      
 1200 CONTINUE                                                          

      NDADD= 0
C ********************************************************
C  A FEW OF THE NEXT LINES ( CSKIP ) ARE COMMENTED OUT
C  IN THIS PROGRAM VERSION.
C  THEY WILL BE REINTRODUCED WHEN THE FIPPLOT PACKAGE 
C  IS MODIFIED TO ACCEPT A SEDIMENT LAYER AS WELL
       
C   SVP IN SEDIMENT LAYER                                               
      IF( H1 .GT. 0.0 )   THEN                                              
        READ(10) (Z1(I), C1(I), I= 1, ND1)                              
CSKIP        NDADD= ND1
CSKIP        DO 1300   I=1,ND1                                               
CSKIP        ZY(I+ND0)= Z1(I) + H0                                           
CSKIP        CX(I+ND0)= C1(I)                                                
CSKIP 1300   CONTINUE                                                        
        NDTOT= ND0 + NDADD                                                
      ELSE                                                              
        NDTOT= ND0                                                     
      END IF                                                            
C ********************************************************

      RM= RPROF(ISECT)*1.0E3                                            
                                                                        
      CALL PLTF1(1,NDTOT,1,RM,YOFF)                                     
      CALL PLTF2(ZY,1,NDTOT)                                            
      CALL PLTF2(CX,1,NDTOT)                                            
                                                                        
C   BOTTOM                                                              
C      READ(10) C2, C2S                                                   
      READ(10) DUMMY

 6000 CONTINUE                                                          
                                                                        
      REWIND 10                                                         
                                                                        
      RETURN                                                            
      END                                                               
