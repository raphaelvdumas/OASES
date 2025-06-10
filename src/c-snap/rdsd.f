      SUBROUTINE RDSD(I,WORD,REREAD,LUIN,ALPHA1,HTOT,HMIN,           
     & SRD,*    )                                       
C                                7000                                   
                                                                        
      INTEGER REREAD, ERROR                                             
                                                                        
      DIMENSION SRD(NOPT,KSRD,2)                                        
      DIMENSION BUFFIN(4)                                               
                                                                        
      CHARACTER*80 WORD, BLANK                                          
      CHARACTER*5 ALPHA1(NOPT)
                                                                        
      COMMON /CFIELD/ FLDRD(3),PULRD(3)                                 
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MODSMP/ MSPH0, MODSED, MSPH1                              
      COMMON /PARA1/ NFF,MSP,NDEP,NOPT,ICF,KSRD

      DATA BLANK /' '/
C                                                                       
  125 FORMAT(A80)                                                       
  480 FORMAT(1H ,//,' THE SOURCE DEPTH SPECIFIED FOR OPTION "',A5,'" IS 
     & NOT ACCEPTABLE.',/,' SOURCE DEPTH =',F10.3,' M',/,               
     & ' MAX ALLOWED DEPTH  =',F10.3,' M',/,' EXECUTION IS TERMINATED') 
  481 FORMAT(1H ,//,' THE RECEIVER DEPTH SPECIFIED FOR OPTION "',A5,    
     & '" IS NOT ACCEPTABLE.',/,' RECEIVER DEPTH  =',F10.3,' M',/,      
     & ' MAX ALLOWED DEPTH =',F10.3,' M',/,' EXECUTION IS TERMINATED')  
  482 FORMAT(1H ,//,' THE RECEIVER DEPTH SPECIFICATION FOR OPTION "',A5,
     &'" IS NOT ACCEPTABLE. ',/,                                        
     & ' MIN RECEIVER DETH  = ',F10.3,/,                                
     & ' MAX RECEIVER DEPTH = ',F10.3,/,                                
     & ' DELZ               = ',F10.3,/,                                
     & ' MAX ALLOWED DEPTH  = ',F10.3,/, ' EXECUTION IS TERMINATED')    
  483 FORMAT(1H ,//,'  MISSING SOURCE SPECIFICATION FOR OPTION "',A5,   
     & '"',/,'  EXECUTION IS TERMINATED')                               
  484 FORMAT(1H ,//,' THE SOURCE DEPTH SPECIFICATION FOR OPTION "',A5,  
     & ' IS NOT ACCEPTABLE. ',/,                                        
     & ' SOURCE DEPTH       = ',F10.3,/,                                
     & ' MAX ALLOWED DEPTH  = ',F10.3,/, ' EXECUTION IS TERMINATED')    
  500 FORMAT(1H ,'MAXIMUM NUMBER OF SOURCE/RECEIVER DEPTH',
     & ' COMBINATIONS',/,' FOR OPTION "',A5,'" HAS BEEN REACHED. ',/,
     & ' EXECUTION IS TERMINATED.')          
  820 FORMAT(1H ,//,'  MISSING DEPTH SPECIFICATION FOR OPTION "',A5,    
     & '"',/,'  EXECUTION IS TERMINATED')                               
                                                                        
                                                                        
C                                                                       
C     READING OF SOURCE/RECEIVER DEPTHS                                 
C                                                                       
      FLAGSD=0.0                                                        
      FLAGRD=0.0                                                        
      DO   6300   K=1,KSRD + 1                                          
C           TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR PROFL 
C           MODES CONDA HORWN PARAM FIELD PULSE                         
      WORD= BLANK
      GO TO(5900 ,7000 ,6000 ,6000 ,6000 ,5900 ,6000 ,6000 ,5900 ,7000 ,
     &      7000 ,6000 ,7000 ,7000 ,5850),I                             
 5850 CONTINUE                                                          
      READ(LUIN, 125, END= 6800) WORD
      IF( WORD .EQ. BLANK )   GO TO 5850
      IF(WORD(1:4) .EQ. '@EOF')   GO TO 6800                              
      IF(K .EQ. 1)  THEN                                                
       CALL RFFORM(WORD,80,BUFFIN,1,3,ERROR)                            
       IF(ERROR .GT. 0)   GOTO 6500                                     
C      MIN AND MAX RECEIVER DEPTH, AND DELZ FOR OPTION "FIELD"          
       FLDRD(1)=BUFFIN(1)                                               
       FLDRD(2)=BUFFIN(2)                                               
       FLDRD(3)=BUFFIN(3)                                               
       IF( (FLDRD(1) .LT. 0.)                      .OR.                 
     &     (FLDRD(1) .GT. HMIN)                   .OR.                  
     &     (FLDRD(2) .LE. 0.)                      .OR.                 
     &     (FLDRD(2) .GT. HMIN)                   .OR.                  
     &     (FLDRD(3) .LT. 0.)                      .OR.                 
     &     (FLDRD(3) .GT. (FLDRD(2)-FLDRD(1)))     .OR.                 
     &     (FLDRD(1) .GT. FLDRD(2)))                    THEN            
        WRITE(LUPRT,482) ALPHA1(I),(BUFFIN(J),J=1,3),HMIN                   
        RETURN                                                          
       END IF
       WORD= BLANK
 5860  READ(LUIN, 125, END= 6700) WORD
       IF( WORD .EQ. BLANK )   GO TO 5860
       IF( WORD(1:4) .EQ. '@EOF' )   GO TO 6700
      END IF                                                            
      IF(K.GT.KSRD)   GO TO 6400                                        
      CALL RFFORM(WORD,80,BUFFIN,1,1,ERROR)                             
      IF(ERROR.GT.0)   GO TO 6500                                       
C  SOURCE DEPTH FOR OPTION "FIELD"                                      
      SRD(I,K,1)=BUFFIN(1)                                              
      IF( (SRD(I,K,1) .LE. 0.) .OR.                                     
     &    (SRD(I,K,1) .GT. HTOT) )   THEN                            
       WRITE(LUPRT,484) ALPHA1(I),BUFFIN(1),HTOT                         
       RETURN                                                           
      END IF                                                            
      GO TO 6300                                                        
C                                                                       
 5900 CONTINUE                                                          
      READ(LUIN, 125, END= 6800) WORD
      IF( WORD .EQ. BLANK )   GO TO 5900
      IF(WORD(1:4) .EQ. '@EOF')   GO TO 6800                              
      CALL RFFORM(WORD,80,BUFFIN,1,2,ERROR)                             
      IF(ERROR.GT.0)   GO TO 6500                                       
      TEMP1=BUFFIN(1)                                                   
      TEMP2=BUFFIN(2)                                                   
      IF(K.GT.KSRD)   GO TO 6400                                        
C                                                                       
C   RECEIVER DEPTH IS CHECKED HERE:                                     
C                                                                       
      IF(TEMP2.LE.0.0.OR.TEMP2.GT.HMIN)   FLAGRD=1.0                    
      SRD(I,K,2)=TEMP2                                                  
      GO TO 6100                                                        
 6000 CONTINUE
      READ(LUIN, 125, END= 6800)   WORD
      IF( WORD .EQ. BLANK )   GO TO 6000
      IF(WORD(1:4) .EQ. '@EOF')   GO TO 6800                              
      CALL RFFORM(WORD,80,BUFFIN,1,2,ERROR)                             
      TEMP1=BUFFIN(1)                                                   
      IF(ERROR.NE.0)   GO TO 6500                                       
      IF(K.GT.KSRD)   GO TO 6400                                        
C                                                                       
C   SOURCE DEPTH IS CHECKED HERE:                                       
C                                                                       
 6100 IF(TEMP1.LE.0.0.OR.TEMP1.GT.HTOT)   FLAGSD=1.0                 
      IF(FLAGSD+FLAGRD.LT.1.0)   GO TO 6200                             
      IF(FLAGSD.GT.0.0)   WRITE(LUPRT,480) ALPHA1(I),TEMP1,HTOT          
      IF(FLAGRD.GT.0.0)   WRITE(LUPRT,481) ALPHA1(I),TEMP2,HMIN             
      RETURN                                                            
 6200 CONTINUE                                                          
      SRD(I,K,1)=TEMP1                                                  
                                                                        
C   "CONDR", NUMBER OF RECEIVERS IS SAVED HERE:                         
C   This is a hidden option: by default the number of receivers is      
C   defined by the variable MSP. The default action is overridden       
C   whenever srd(i,k,1) > 0.                                            
      IF( I .EQ. 8 )   THEN                                             
        IF( BUFFIN(2) .GT. 0. )  THEN                                   
          SRD(I,K,2)= BUFFIN(2)                                         
        ELSE                                                            
          SRD(I,K,2)= MSPH0                                             
        END IF                                                          
      END IF                                                            
                                                                        
 6300 CONTINUE                                                          
 6400 WRITE(LUPRT,500) ALPHA1(I)
      RETURN                                                            
 6500 CONTINUE                                                          
      IF(K .EQ. 1)   GO TO 7100                                           
      REREAD=1                                                          
      RETURN 1                                                          
 6700 CONTINUE                                                          
      WORD(1:4)= '@EOF'
      WRITE(LUPRT,483) ALPHA1(I)                                          
      RETURN                                                          
 6800 CONTINUE                                                          
      WORD(1:4)= '@EOF'
      REREAD=1                                                          
      IF(K.GT.1)   RETURN 1                                             
 7000 CONTINUE                                                          
      REREAD=0                                                          
      RETURN 1                                                          
 7100 WRITE(LUPRT,820) ALPHA1(I)                                            
      RETURN                                                            
      END                                                               
