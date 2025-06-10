C   PROF_INT.FOR

      SUBROUTINE PROFIN(NPOUT,ZOUT,COUT,NPIN,ZIN,CL,CR,               
     & RATIOX,ZX,CCOUT)                                                 
                                                                        
C     THIS SUBROUTINE PERFORMS LINEAR INTERPOLATION IN DEPTH AND        
C     RANGE BETWEEN TWO SVPs DEFINED AT THE SAME DEPTH COORDINATES.     
C     "RATIOX" IS THE INPERPOLATION FACTOR IN RANGE.                    
                                                                        
C     INPUT:                                                            
C           NPIN = NUMBER OF DATA POINTS IN SVPs                        
C            ZIN = ARRAY WITH DEPTH IN SVPs                             
C             CL = ARRAY WITH SOUND SPEED IN LEFT SVP                   
C             CR = ARRAY WITH SOUND SPEED IN RIGHT SVP                  
C         RATIOX = INTERPOLATION CONSTANT IN RANGE                      
C             ZX = DEPTH AT INTERPOLATED SVP                            
C                                                                       
C     OUTPUT :                                                          
C          NPOUT = NUMBER OF DATA POINTS IN SVP                         
C           ZOUT = ARRAY WITH DEPTH IN INTERPOLATED SVP                 
C           COUT = ARRAY WITH SOUND SPEED IN INTERPOLATED SVP           
C          CCOUT = AVERAGE SOUND SPEED IN INTERP SVP                    
                                                                        
                                                                        
                                                                        
      DOUBLE PRECISION ZX, RATIOX, RATIOY, CA, CB, CCOUT                
      DOUBLE PRECISION ZOUT( * ), COUT( * ),                            
     &                 ZIN( * ), CL( * ), CR( * )                       
                                                                        
                                                                        
                                                                        
      DO 1000   I=1,NPIN                                                
      IF(ZIN(I) .LT. ZX)   THEN                                         
       ZOUT(I)=ZIN(I)                                                   
       COUT(I)= CL(I) + RATIOX*(CR(I)-CL(I))                            
      ELSE                                                              
       NPOUT=I                                                          
       ZOUT(NPOUT)=ZX                                                   
       RATIOY= (ZX-ZIN(I-1))/(ZIN(NPOUT)-ZIN(I-1))                      
       CA= CL(I-1) + RATIOY*(CL(NPOUT)-CL(I-1))                         
       CB= CR(I-1) + RATIOY*(CR(NPOUT)-CR(I-1))                         
       COUT(NPOUT)= CA + RATIOX*(CB-CA)                                 
       GO TO 2000                                                       
      END IF                                                            
 1000 CONTINUE                                                          
 2000 CONTINUE                                                          
                                                                        
C   AVERAGE SOUND SPEED                                                 
      CCOUT=0.0                                                         
      IF(NPOUT .GT. 0)   THEN                                           
       DO 3000   J=1,NPOUT-1                                            
 3000  CCOUT=CCOUT+((COUT(J)+COUT(J+1))*(ZOUT(J+1)-ZOUT(J)))/2.0        
       CCOUT=CCOUT/ZX                                                   
      END IF                                                            
                                                                        
      RETURN                                                            
      END                                                               
