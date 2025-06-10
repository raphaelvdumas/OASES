C   PLTFILE.FOR
      SUBROUTINE PLTFIL(TITXUP,IGRIDT,OPTION,NLAB,NC,XLEN,YLEN,        
     &                  XLEFT,XRIGHT,XINC,YDOWN,YUP,YINC,DLAB)
                                                                        
      CHARACTER*80 TITXUP, OPTION                                       
      CHARACTER*80 TITLE, TITLEX, TITLEY, TITLEZ                        
      CHARACTER*3 XBTYPE, YBTYPE, ZBTYPE                                
      CHARACTER*30 DLAB(10)                                             
                                                                        
      DIMENSION PX(1)                                                   
                                                                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /TITL/ TITLE,TITLEX,TITLEY,XBTYPE,YBTYPE                   
      COMMON /XAXN/ DX,XDIV                                             
      COMMON /YAXN/ DY,YDIV                                             
      COMMON /ZAXN/ DZ,ZDIV,ZMIN,ZMAX,ZLEN,ZINC                         
      COMMON /ZAXC/ TITLEZ,ZBTYPE                                       
                                                                        
  300 FORMAT(1X ,A80)                                                   
  320 FORMAT(1H ,A72)                                                   
  330 FORMAT(1X ,A30)
  340 FORMAT(1H ,I8,14X,A30)                                            
  360 FORMAT(1H ,3(3X,I5),13X,' NPT,NSP,CURVE IDENTIFIER ')             
  400 FORMAT(1H ,E15.6,3X,A34)                                          
  420 FORMAT(1H ,A3)                                                    
                                                                        
                                                                        
C       DESCRIPTION OF INPUT PARAMETERS                                 
                                                                        
                                                                        
C    OPTION    STRING UP TO 80 CHRS. IT IS WRITTEN ON                   
C              THE UPPER RIGHT HAND CORNER OF THE GRAPH                 
C    TITXUP    TEXT STRING UP TO 80 CHRS. IT IS WRITTEN ON THE          
C              UPPER PART OF THE GRAPH (CENTERED)                       
C    TITLE     TEXT STRING UP TO 80 CHRS. IT IS WRITTEN                 
C              ON THE UPPER LEFT PART OF THE GRAPH                      
C                                                                       
C    NLAB      N. OF LABELS TO BE WRITTEN INSIDE THE GRAPH              
C    DLAB      CHARACTER ARRAY WITH PLOT LABELS                         
C                                                                       
C    XLEN      X AXIS LENGTH IN CM                                      
C    YLEN      Y AXIS LENGTH IN CM                                      
C                                                                       
C  
C    ZLEN      Z AXIS LENGTH IN CM                                      
C  
C                                                                       
C    IGRIDT    = 1 A GRID IS PLOTTED                                    
C              = 0 NO GRID IS PLOTTED                                   
C                                                                       
C    XLEFT     VALUE OF X AT THE AXIS ORIGIN                            
C    XRIGHT    VALUE OF X AT THE AXIS END                               
C    XINC      STEP INTERVAL IN USER'S UNIT                             
C    XDIV      FACTOR APPLIED ON VALUES APPEARING ON THE AXIS           
C    TITLEX    TEXT ON X AXIS                                           
C    XBTYPE    "LIN" = LINEAR AXIS                                      
C              "LOG" = LOGARITHMIC AXIS                                 
C                                                                       
C    YDOWN     VALUE OF Y AT THE AXIS ORIGIN                            
C    YUP       VALUE OF Y AT THE AXIS END                               
C    YINC      STEP INTERVAL IN USER'S UNIT                             
C    YDIV      FACTOR APPLIED ON VALUES APPEARING ON THE AXIS           
C    TITLEY    TEXT ON Y AXIS                                           
C    YBTYPE    "LIN" = LINEAR AXIS                                      
C              "LOG" = LOGARITHMIC AXIS                                 
C                                                                       
C  
C                                                                       
C    ZMIN      VALUE OF Z AT THE AXIS ORIGIN                            
C    ZMAX      VALUE OF Z AT THE AXIS END                               
C    ZINC      STEP INTERVAL IN USER UNIT                               
C    ZDIV      FACTOR APPLIED ON VALUES APPEARING ON THE AXIS           
C    TITLEZ    TEXT FOR SOUND SPEED AXIS                                
C    ZBTYPE    "LIN" = LINEAR AXIS                                      
C              "LOG" = LOGARITHMIC AXIS                                 
C  
C                                                                       
C    NC        NUMBER OF CURVES ON SAME GRAPH                           
C                                                                       
C    NPT       NUMBER OF DATA POINTS TO BE PLOTTED                      
C    NSP       NO. OF POINTS FOR SMOOTHING (RUNNING AVERAGE)            
C    INDEX     CURVE IDENTIFIER                                         
C                                                                       
C    XOFF      OFFSET ON X AXIS                                         
C    DX        .EQ. 0 :  X COORDINATES ARE EXPLICITLY SUPPLIED          
C              .NE. 0 :  FIXED INCREMENT BETWEEN X COORDINATES          
C    YOFF      OFFSET ON Y AXIS                                         
C    DY        .EQ. 0 :  Y COORDINATES ARE EXPLICITLY SUPPLIED          
C              .NE. 0 :  FIXED INCREMENT BETWEEN Y COORDINATES          
C                                                                       
C    LUPLP     OUTPUT FILE WITH PARAMETERS                              
C    LUPLT     OUTPUT FILE WITH ACTUAL DATA TO BE PLOTTED               
                                                                        
                                                                        
                                                                        
      WRITE(LUPLP,300) OPTION                                           
      WRITE(LUPLP,320) TITXUP(1:72)                                     
      WRITE(LUPLP,320) TITLE(1:72)                                      
      WRITE(LUPLP,340) NLAB,'  NUMBER OF LABELS'                        
                                                                        
      IF(NLAB. GT. 0)   THEN                                            
       DO 1000 I=1,NLAB                                                 
       WRITE(LUPLP,330) DLAB(I)                                           
 1000  CONTINUE                                                         
      END IF                                                            
                                                                        
                                                                        
                                                                        
                                                                        
      WRITE(LUPLP,400) XLEN,'XLEN'                                      
      WRITE(LUPLP,400) YLEN,'YLEN'                                      
      IF( (OPTION(08:12) .EQ. 'PROFL') .AND.                            
     &    (OPTION(14:16) .EQ. 'RDP'  ))  WRITE(LUPLP,400) ZLEN,'ZLEN'   
                                                                        
      WRITE(LUPLP,340) IGRIDT,'  GRID TYPE. 0:  NO GRID'                
                                                                        
      WRITE(LUPLP,400) XLEFT,'XLEFT'                                    
      WRITE(LUPLP,400) XRIGHT,'XRIGHT'                                  
      WRITE(LUPLP,400) XINC,'XINC'                                      
      WRITE(LUPLP,400) XDIV,'XDIV'                                      
      WRITE(LUPLP,320) TITLEX                                           
      WRITE(LUPLP,420) XBTYPE                                           
                                                                        
      WRITE(LUPLP,400) YDOWN,'YDOWN'                                    
      WRITE(LUPLP,400) YUP,'YUP'                                        
      WRITE(LUPLP,400) YINC,'YINC'                                      
      WRITE(LUPLP,400) YDIV,'YDIV'                                      
      WRITE(LUPLP,320) TITLEY                                           
      WRITE(LUPLP,420) YBTYPE                                           
                                                                        
      IF( (OPTION(08:12) .EQ. 'PROFL') .AND.                            
     &    (OPTION(14:16) .EQ. 'RDP'  ))   THEN                          
         WRITE(LUPLP,400) ZMIN,'ZMIN'                                   
         WRITE(LUPLP,400) ZMAX,'ZMAX'                                   
         WRITE(LUPLP,400) ZINC,'ZINC'                                   
         WRITE(LUPLP,400) ZDIV,'ZDIV'                                   
         WRITE(LUPLP,320) TITLEZ                                        
         WRITE(LUPLP,420) ZBTYPE                                        
      END IF                                                            
                                                                        
      WRITE(LUPLP,340) NC,'  NC'                                        
                                                                        
      RETURN                                                            
                                                                        
                                                                        
      ENTRY PLTF1(INDEX,NPT,NSP,XOFF,YOFF)
      WRITE(LUPLP,360) NPT,NSP,INDEX                                    
      WRITE(LUPLP,400) XOFF,'OFFSET ON X AXIS'                          
      WRITE(LUPLP,400) DX,'STEP INC ON X AXIS'                          
      WRITE(LUPLP,400) YOFF,'OFFSET ON Y AXIS'                          
      WRITE(LUPLP,400) DY,'STEP INC ON Y AXIS'                          
      RETURN                                                            
C                                                                       
C                                                                       
C                                                                       
      ENTRY PLTF2(PX,I1,I2)
      WRITE(LUPLT,100)(PX(I),I=I1,I2)                                   
  100 FORMAT(5E15.6)                                                    
      RETURN                                                            
                                                                        
      END                                                               
