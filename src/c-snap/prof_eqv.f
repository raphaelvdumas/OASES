C   PROF_EQV.FOR                                                        
C                                                                       
      SUBROUTINE PROFEQ(NDL,ZIL,CIL,NDR,ZIR,CIR,NDLR,ZLR,CL,CR)       
                                                                        
C                                                                       
C     a) Given two SVPs, extending at different depths, new data points 
C        obtained from the deepest SVP are added to the shallowest one  
C        to make it cover the max depth between them..                  
C        The new points maintain the slope they have in the deepest SVP 
C        but are adjusted to match the last point in the shallowest SVP.
C     b) The two SVPs are modified as necessary to contain              
C        SVP data points at exactly the same depth coordinates.         
C        The purpose here is to speed up subsequent interpolations.     
C                                                                       
C     INPUT:                                                            
C            NDL = NUMBER OF DATA POINTS IN FIRST SVP (left)            
C            ZIL = DEPTH IN FIRST SVP (left)                            
C            CIL = SOUND SPEED IN FIRST SVP (left)                      
C            NDR = NUMBER OF DATA POINTS IN SECOND SVP (right)          
C            ZIR = DEPTH IN SECOND SVP (right)                          
C            CIR = SOUND SPEED IN SECOND SVP (right)                    
C                                                                       
C     OUTPUT :                                                          
C           NDLR = NUMBER OF DATA POINTS IN FINAL SVPs                  
C            ZLR = DEPTH IN FINAL SVPs                                  
C             CL = SOUND SPEED IN FINAL SVP (left)                      
C             CR = SOUND SPEED IN FINAL SVP (right)                     
                                                                        
                                                                        
                                                                        
      DOUBLE PRECISION DELTAC, CINT1, CINT2                             
      DOUBLE PRECISION ZIL(1), CIL(1), ZIR(1), CIR(1)                   
      DOUBLE PRECISION ZLR(1), CL(1), CR(1)                             
                                                                        
                                                                        
                                                                        
      NMAX=NDL                                                          
                                                                        
C EXTEND shallowest SVP (STEP a)                                        
                                                                        
      IF(NDL.EQ.0) THEN                                                 
        IF(NDR.EQ.0)   THEN                                             
          NDLR=0                                                        
          RETURN                                                        
        ELSE                                                            
          NDLR=NDR                                                      
          DO 1000 I=1,NDR                                               
          ZLR(I)=ZIR(I)                                                 
          CL(I)=CIR(I)                                                  
          CR(I)=CIR(I)                                                  
 1000     CONTINUE                                                      
          RETURN                                                        
        END IF                                                          
      ELSE                                                              
        IF(NDR.EQ.0) THEN                                               
          NDLR=NDL                                                      
          DO 2000 I=1,NDL                                               
          ZLR(I)=ZIL(I)                                                 
          CR(I)=CIL(I)                                                  
          CL(I)=CIL(I)                                                  
 2000     CONTINUE                                                      
          RETURN                                                        
        ELSE                                                            
                                                                        
          IF(ZIL(NDL) .LT. ZIR(NDR)) THEN                               
            DO 2400 I2=1,NDR                                            
            IF(ZIL(NDL) .LT. ZIR(I2))   THEN                            
              CINT2= CIR(I2-1) +                                        
     &               (CIR(I2)-CIR(I2-1)) * ( (ZIL(NDL)-ZIR(I2-1)) /     
     &               (ZIR(I2)-ZIR(I2-1)) )                              
              DELTAC=CINT2-CIL(NDL)                                     
              DO 2200 I1=I2,NDR                                         
              NMAX= NMAX + 1                                            
              ZIL(NMAX)=ZIR(I1)                                         
 2200         CIL(NMAX)=CIR(I1)-DELTAC                                  
              GO TO 3000                                                
            END IF                                                      
 2400       CONTINUE                                                    
          ELSE IF(ZIR(NDR) .LT. ZIL(NDL))   THEN                        
            DO 2800 I1=1,NDL                                            
            IF(ZIR(NDR) .LT. ZIL(I1))   THEN                            
              CINT1= CIL(I1-1) +                                        
     &               (CIL(I1)-CIL(I1-1)) * ( (ZIR(NDR)-ZIL(I1-1)) /     
     &               (ZIL(I1)-ZIL(I1-1)) )                              
              DELTAC=CINT1-CIR(NDR)                                     
              DO 2600 I2=I1,NDL                                         
              INDEX=NDR+I2-I1+1                                         
              ZIR(INDEX)=ZIL(I2)                                        
 2600         CIR(INDEX)=CIL(I2)-DELTAC                                 
              GO TO 3000                                                
            END IF                                                      
 2800       CONTINUE                                                    
          END IF                                                        
                                                                        
 3000     CONTINUE                                                      
        END IF                                                          
      END IF                                                            
                                                                        
C   SUBTABULATE FINAL SVPs AT SAME DEPTH COORDINATES                    
C   THE ALGORYTHM USES ZIL AS REFERENCE                                 
                                                                        
      ZLR(1)=0.0                                                        
      CL(1)=CIL(1)                                                      
      CR(1)=CIR(1)                                                      
                                                                        
      NDLR=1                                                            
      I2=2                                                              
      DO 4400 I1=2,NMAX                                                 
 4200 NDLR=NDLR+1                                                       
      IF(ZIL(I1) .LT. ZIR(I2)) THEN                                     
        ZLR(NDLR)=ZIL(I1)                                               
        CL(NDLR)=CIL(I1)                                                
        CR(NDLR)=CIR(I2-1)+(CIR(I2)-CIR(I2-1))*(ZIL(I1)-ZIR(I2-1))/     
     &         (ZIR(I2)-ZIR(I2-1))                                      
      ELSE IF(ZIL(I1) .EQ. ZIR(I2)) THEN                                
        ZLR(NDLR)=ZIL(I1)                                               
        CL(NDLR)=CIL(I1)                                                
        CR(NDLR)=CIR(I2)                                                
        I2=I2+1                                                         
      ELSE                                                              
        ZLR(NDLR)=ZIR(I2)                                               
        CL(NDLR)=CIL(I1-1)+(CIL(I1)-CIL(I1-1))*(ZIR(I2)-ZIL(I1-1))/     
     &         (ZIL(I1)-ZIL(I1-1))                                      
        CR(NDLR)=CIR(I2)                                                
        I2=I2+1                                                         
        GO TO 4200                                                      
      END IF                                                            
 4400 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
                                                                        
