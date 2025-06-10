C  OUTSUM                                                               
                                                                        
      SUBROUTINE OUTSUM(BIASUM, IPROF, RCOUPL, SUM2A, YMIN, YMAX)       
                                                                        
      REAL BIASUM(*), RCOUPL(*), SUM2A(*)                               
      COMMON /XAXN/ DX, XDIV                                            
      COMMON /YAXN/ DY, YDIV                                            
C                                                                       
       IF(IPROF .LE. 1)   RETURN                                        
C                                                                       
       DX=0.0                                                           
       DY=0.0                                                           
       XOFF=RCOUPL(1)                                                  
       YOFF=0.0                                                         
C                                                                       
C   MIN, MAX IN SUM2A ARRAY                                             
      SUMMIN=SUM2A(1)                                                   
      SUMMAX=SUMMIN                                                     
      DO 1000  I=2,IPROF                                                
      IF(SUM2A(I) .LT. SUMMIN)   THEN                                   
        SUMMIN=SUM2A(I)                                                 
      ELSE IF(SUM2A(I) .GT. SUMMAX)   THEN                              
        SUMMAX=SUM2A(I)                                                 
      END IF                                                            
 1000 CONTINUE                                                          
C      SUMAVR= (SUMMIN+SUMMAX)/2.0                                       
      SUMDIF= SUMMAX-SUMMIN                                           
                                                                        
C      YAVR=(YMIN+YMAX)/2.0                                              
      YDELTA= ABS(YMAX-YMIN)                                            
                                                                        
C      BIAS= YMAX - YDELTA/3.                                           
      BIAS= YMAX                                                        
      IF( SUMDIF .GT. 0 )   THEN                                      
        RATIO= (YDELTA/4.0)/SUMDIF                                    
        DO 2000   I=1,IPROF                                             
        BIASUM(I)= ( -SUM2A(I) + SUMMIN ) * RATIO + BIAS                 
 2000   CONTINUE                                                        
      ELSE                                                              
        DO 2200   I=1,IPROF                                             
        BIASUM(I)= YMAX                                                  
 2200   CONTINUE                                                        
      END IF                                                            
                                                                        
      NPM1=IPROF-1                                                      
                                                                        
      CALL PLTF1(1,             
     $           NPM1,          
     $           1,             
     $           XOFF,          
     $           YOFF)          
      CALL PLTF2(RCOUPL,2,IPROF)                                       
      CALL PLTF2(BIASUM,2,IPROF)                                         
      RETURN                                                            
      END                                                               
