C   CONVRG.FOR                                                          
                                                                        
      SUBROUTINE CONVRG(ERR1A,ERR1R,ERR2A,ERR2R,MODQTY,NMES,            
     & MY,MAXMSH,MODEN,DH0SQ,H0,EK,EXTPOL,WNMIN)
                                                                        
      INTEGER EXTPOL, REMOVE                                            
                                                                        
      DOUBLE PRECISION ERROR                                            
      DOUBLE PRECISION H0                                               
      DOUBLE PRECISION ERR1R, ERR1A, ERR2R, ERR2A, EXTA,                
     &                 ZL, WNMIN                                        
      DOUBLE PRECISION MY(MAXMSH,MODEN), DH0SQ(8), EK(MODEN)            
                                                                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT

  200 FORMAT(1X,//,' *** WARNING :  MODE CUTOFF AFTER EXTRAPOLATION',/, 
     & ' DETECTED IN SUB CONVRG. ',/,                                   
     & ' EXECUTION TERMINATED FOR THIS SOURCE FREQUENCY.')              
                                                                        
C  ERR1A   : absolute error on extrapolated wavenumber                  
C  ERR1R   : relative error on extrapolated wavenumber                  
C  ERR2A   : absolute error on NON extrapolated wavenumber              
C  ERR2R   : relative error on NON extrapolated wavenumber              
C  WNMIN   : minimum wavenumber ( cutoff )                              
                                                                        
      IF(NMES .EQ. 1)   THEN                                            
       DO 1000   M=1,MODQTY                                             
       EK(M)=SQRT(MY(NMES,M))/H0                                        
 1000  CONTINUE                                                         
       RETURN                                                           
      END IF                                                            
                                                                        
C *******************************************************************   
                                                                        
C  REMOVE= 1  : THE LAST ORDER MODE IS DISCARDED FROM THE CHECK         
C  REMOVE= 0  : THE LAST ORDER MODE IS "NOT" DISCARDED FROM THE CHECK   
      REMOVE= 1                                                         
                                                                        
C *******************************************************************   
                                                                        
                                                                        
C Computing extrapolated wavenumber and error on same                   
      IF(EXTPOL .GT. 0)   THEN                                          
        ERR1A=0.0                                                       
        ERR1R=0.0                                                       
        DO 4000   M=1, MODQTY                                           
                                                                        
        EK(M)=0.0                                                       
        DO 2200  J=1,NMES                                               
        ZL=1.0                                                          
        DO 2000  K=1,NMES                                               
        IF(K.NE.J)  ZL= ZL*DH0SQ(K)/(DH0SQ(K)-DH0SQ(J))                 
 2000   CONTINUE                                                        
        EK(M)=EK(M)+ZL*MY(J,M)                                          
 2200   CONTINUE                                                        
                                                                        
        IF( EK(M) .LE. WNMIN )  THEN                                    
          MODQTY=M-1                                                    
          IF(MODQTY .GT. 0)   GO TO 5000                                
          WRITE(LUPRT,200)                                                  
          RETURN                                                        
        END IF                                                          
        EK(M)=SQRT(EK(M))/H0          
                                                                        
        IF( (M+REMOVE) .LE. MODQTY )   THEN                             
         EXTA=0.0                                                       
         DO 3200  J=1,NMES-1                                            
         ZL=1.0                                                         
         DO 3000  K=1,NMES-1                                            
         IF(K.NE.J)  ZL= ZL*DH0SQ(K)/(DH0SQ(K)-DH0SQ(J))                
 3000    CONTINUE                                                       
         EXTA=EXTA+ZL*MY(J,M)                                           
 3200    CONTINUE                                                       
                                                                        
C        IF( EXTA .LE. WNMIN )  THEN                                    
C          MODQTY=M-1                                                   
C          IF(MODQTY .GT. 0)   GO TO 5000                               
C          WRITE(LUPRT,200)                                                 
C          RETURN                                                       
C        END IF                                                         
         EXTA=SQRT(EXTA)/H0          
                                                                        
         ERROR= ABS(EXTA-EK(M))                                         
         IF(ERR1A .LT. ERROR)    THEN                                   
C           MSAVEA= M                                                    
           ERR1A= ERROR                                                 
         END IF                                                         
         ERROR= ERROR/EK(M)                                             
         IF(ERR1R .LT. ERROR)    THEN                                   
C           MSAVER= M                                                    
           ERR1R= ERROR                                                 
         END IF                                                         
        END IF                                                          
                                                                        
 4000   CONTINUE                                                        
                                                                        
 5000   CONTINUE                                                        
                                                                        
      END IF                                                            
                                                                        
C *******************************************************************   
C Computing error on NON extrapolated wavenumber                        
      ERR2A=0.0                                                         
      ERR2R=0.0                                                         
      M2SAVE= 1                                                         
      DO 6000   M=1, MODQTY - REMOVE                                    
      IF(ERR2A .LT. ABS(MY(NMES,M)-MY(NMES-1,M)))   THEN                
        ERR2A= ABS(MY(NMES,M)-MY(NMES-1,M))                             
        M2SAVE=M                                                        
      END IF                                                            
 6000 CONTINUE                                                          
      ERR2A=ABS( SQRT(MY(NMES,M2SAVE)) - SQRT(MY(NMES-1,M2SAVE)) )      
      ERR2R= ERR2A/SQRT(MY(NMES,M2SAVE))                                
      ERR2A=ERR2A/H0                                                    
                                                                        
      RETURN                                                            
      END                                                               
