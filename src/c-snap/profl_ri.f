C   PROFL_RI.FOR                                                        
                                                                        
      SUBROUTINE PROFRI(IOP, C0, Z0, C1, Z1, AS, AY, NOPT, TITLE,     
     & ZY, CX)                                                          
                                                                        
      CHARACTER*3 XBTYPE, YBTYPE                                        
      CHARACTER*4 TITLE(20)                                             
      CHARACTER*30 DLAB(10)                                             
      CHARACTER*80 TITLEX, TITLEY, TITXUP, OPTION                       
                                                                        
      REAL ZY( * ), CX( * )                                             
C      REAL BETA(-1:3), SCATT(2)                                         
      REAL AS(5), AY(NOPT,7)                                            
                                                                        
C      DOUBLE PRECISION CC0, CC1                                         
      DOUBLE PRECISION H0, H1                                           
      DOUBLE PRECISION C0( * ), Z0( * ), C1( * ), Z1( * )               
                                                                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT         
      COMMON /TITL/ TITXUP, TITLEX, TITLEY, XBTYPE, YBTYPE              
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
                                                                        
  100 FORMAT(1H ,//,'  REVISE Y AXIS FOR OPTION "PROFL".')              
  200 FORMAT(1H ,//,'   ERROR IN PROFL SUBROUTINE ')                    
  820 FORMAT('Sound speed (m/s)$')                                      
  840 FORMAT('Depth (m)$')                                              
                                                                        
      TITXUP= ' '                                                       
      OPTION= ' '                                                       
      XBTYPE= 'LIN'                                                     
      YBTYPE= 'LIN'                                                     
      XDIV= 1.0                                                         
      YDIV= 1.0                                                         
      WRITE(TITLEX,820)                                                 
      WRITE(TITLEY,840)                                                 
C                                                                       
      XLEFT= AS(1)                                                      
      XRIGHT= AS(2)                                                     
      XLEN= AS(3)                                                       
      IF( AS(5) .LT. 1.0 )   XLEN= 2.5 * XLEN                           
      XINC= AS(4)                                                       
      YLEN= AY(IOP,3)                                                   
      YINC= AY(IOP,4)                                                   
      DX=0.0                                                            
      DY=0.0                                                            
      XOFF=0.0                                                          
      YOFF=0.0                                                          
      DLAB(1)=' '                                                       
      OPTION(1:12)='SNAP   PROFL'                                       
C                                                                       
C                                                                       
      REWIND 10                                                         
C      READ(10) MINMOD, MAXMOD                                           
      READ(10) IDUMMY
      NDTOT=0                                                           
C      READ(10) RKM                                                      
      READ(10) DUMMY
C      READ(10) R1, R2, H0, H1, ND0, ND1                                  
      READ(10) DUMMY, DUMMY, H0, H1, ND0, ND1
      H0H1= H0 + H1                                                     
C      READ(10) (BETA(J), J= 1, 3), SCATT, CC0, CC1                      
      READ(10) DUMMY
      READ(10) (Z0(J), C0(J),J= 1, ND0)                                 
      IF(ND1.GT.0)   READ(10) (Z1(J), C1(J),J= 1, ND1)                  
C      READ(10)C2,C2S                                                    
      READ(10) DUMMY
C                                                                       
C   REVISING OF Y AXIS                                                  
C                                                                       
      IF(AY(IOP,1).LT.AY(IOP,2))   GO TO 1100                           
      TEMP=AY(IOP,1)                                                    
      AY(IOP,1)=AY(IOP,2)                                               
      AY(IOP,2)=TEMP                                                    
 1100 CONTINUE                                                          
      ZMIN=AY(IOP,1)                                                    
      ZMAX=AY(IOP,2)                                                    
      IF(AY(IOP,5).LT.1.0)   ZMAX=H0H1                                  
C   CHECK ON MIN DEPTH                                                  
      IF(ZMIN.GE.0.0)   GO TO 1200                                      
      WRITE(LUPRT,100)                                                  
      GO TO 6800                                                        
                                                                        
C   CHECK ON MAX/MIN DEPTH INTERVAL                                     
 1200 CONTINUE                                                          
      ZYAX=ZMAX                                                         
      ZMAX=AMIN1(H0H1,ZYAX)                                             
      IF((ZMAX-ZMIN).GT.0.0)   GO TO 1400                               
      WRITE(LUPRT,100)                                                  
      GO TO 6800                                                        
 1400 CONTINUE                                                          
C                                                                       
C   ARRAYS C0,Z0,C1 AND Z1 ARE NOW SCANNED TO BUILD                     
C   ARRAYS ZY AND CX WHICH WILL CONTAIN ONLY                            
C   THE DATA TO BE PLOTTED.                                             
C                                                                       
      IF(ZMIN.GE.H0)   GO TO 2500                                       
C                                                                       
C  WATER LAYER                                                          
C                                                                       
      DO 1800   I=2,ND0                                                 
      IF(Z0(I).LE.ZMIN)   GO TO 1800                                    
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=ZMIN                                                    
      CX(NDTOT)=(C0(I)-C0(I-1))*((ZMIN-Z0(I-1))/(Z0(I)-                 
     $Z0(I-1)))+C0(I-1)                                                 
      GO TO 2000                                                        
 1800 CONTINUE                                                          
 2000 CONTINUE                                                          
      DO 2200   J=I,ND0                                                 
      IF(Z0(J).GE.ZMAX)   GO TO 2400                                    
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=Z0(J)                                                   
      CX(NDTOT)=C0(J)                                                   
 2200 CONTINUE                                                          
      I=1                                                               
      GO TO 2800                                                        
 2400 CONTINUE                                                          
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=ZMAX                                                    
      CX(NDTOT)=(C0(J)-C0(J-1))*((ZMAX-Z0(J-1))/(Z0(J)-                 
     $Z0(J-1)))+C0(J-1)                                                 
      GO TO 3400                                                        
C                                                                       
C   SEDIMENT LAYER                                                      
C                                                                       
 2500 CONTINUE                                                          
      DO 2600   I=2,ND1                                                 
      IF(H0+Z1(I).LE.ZMIN)   GO TO 2600                                 
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=ZMIN                                                    
      CX(NDTOT)=(C1(I)-C1(I-1))*((ZMIN-H0-Z1(I-1))/(Z1(I)-              
     $Z1(I-1)))+C1(I-1)                                                 
      GO TO 2800                                                        
 2600 CONTINUE                                                          
 2800 CONTINUE                                                          
      DO 3000   J=I,ND1                                                 
      IF(H0+Z1(J).GE.ZMAX)   GO TO 3200                                 
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=H0+Z1(J)                                                
      CX(NDTOT)=C1(J)                                                   
 3000 CONTINUE                                                          
      WRITE(LUPRT,200)                                                  
      STOP 'STOP PROFL.FOR '                                            
 3200 CONTINUE                                                          
      NDTOT=NDTOT+1                                                     
      ZY(NDTOT)=ZMAX                                                    
      CX(NDTOT)=(C1(J)-C1(J-1))*((ZMAX-H0-Z1(J-1))/(Z1(J)-              
     $Z1(J-1)))+C1(J-1)                                                 
 3400 CONTINUE                                                          
      YUP=ZMIN                                                          
      YDOWN=ZYAX                                                        
      CALL PLTFIL(TITLE,          
     $             0,              
     $             OPTION,         
     $             0,              
     $             1,              
     $             XLEN,           
     $             YLEN,           
     $             XLEFT,          
     $             XRIGHT,         
     $             XINC,           
     $             YDOWN,          
     $             YUP,            
     $             YINC,           
     $             DLAB)
C                                                                       
C                                                                       
      CALL PLTF1(1,NDTOT,1,XOFF,YOFF)                                   
C                                                                       
C                                                                       
      CALL PLTF2(CX,1,NDTOT)                                            
      CALL PLTF2(ZY,1,NDTOT)                                            
C                                                                       
C                                                                       
6800  CONTINUE                                                          
      REWIND 10                                                         
      RETURN                                                            
      END                                                               
