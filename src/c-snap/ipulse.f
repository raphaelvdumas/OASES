      SUBROUTINE IPULSE( WORD, ALPHA1, SECD, SRD, NOPT,                 
     & I, KSRD, H0BEG, H1BEG, LUIN)                                       
C                                                                       
      CHARACTER*5 ALPHA1(NOPT)                                          
      CHARACTER*80 WORD, BLANK                                                 

      DIMENSION BUFFIN(4)                                               
      DIMENSION SRD(NOPT,KSRD,2), SECD(NOPT,3)          

      COMMON /CFIELD/ FLDRD(3), PULRD(3)
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /PULSED/ NFFT, FMIN, FMAX, DELTAT                             

      DATA BLANK/' '/

  200 FORMAT(A80)                                                       
  300 FORMAT(1H ,' REVISE THE FOLLOWING INPUT LINE:',/,2X,A80,/,        
     & '  SPECIFIED FOR OPTION "',A5,'".',/,                            
     & ' EXECUTION IS TERMINATED.')                                     
  400 FORMAT(1H ,//,' THE RECEIVER DEPTH SPECIFICATION FOR OPTION "',   
     & A5,'" IS NOT CORRECT. ',/,                                       
     & ' MIN RECEIVER DETH  = ',F10.3,/,                                
     & ' MAX RECEIVER DEPTH = ',F10.3,/,                                
     & ' DELZ               = ',F10.3,/,                                
     & ' MAX ALLOWED DEPTH  = ',F10.3,/, ' EXECUTION IS TERMINATED.')    
  500 FORMAT(1H ,//,' THE SOURCE DEPTH SPECIFICATION FOR OPTION "',A5,  
     & ' IS NOT CORRECT. ',/,'  INPUT LINE : ',1X,A80,/,                
     & ' DELZ               = ',F10.3,/,                                
     & ' MAX ALLOWED DEPTH  = ',F10.3,/, ' EXECUTION IS TERMINATED.')    
  600 FORMAT(1H ,
     & ' INCOMPLETE COMPUTATIONAL INPUT FOR OPTION "PULSE".',
     & /,' EXECUTION IS TERMINATED.')                                     
C                                                                       

      CALL RFFORM(WORD,80,BUFFIN,1,4,ISTAT)                             
      IF(ISTAT .GT. 0)   THEN                                           
        WRITE(LUPRT,300) WORD,ALPHA1(I)                                      
        STOP                                                             
      ELSE
        BACKSPACE LUIN
        READ(LUIN,*) NFFT, FMIN, FMAX, DELTAT
      END IF                                                            
      IF( DELTAT.LE.0.0)   THEN
        DELTAT= -1.0/DELTAT                                         
        WRITE(LUPRT,*)
     &  ' *** WARNING: Negative DT interpreted as SAMPLING FREQUENCY '      
      END IF                                                            
C   READING OF RMIN,RMAX AND STEP INCREMENT IN KILOMETERS.              
      WORD= BLANK
 1000 READ(LUIN, 200, END= 2000)   WORD
      IF( WORD .EQ. BLANK )   GO TO 1000
      CALL RFFORM(WORD,80,BUFFIN,1,3,ISTAT)                             
      IF(ISTAT .GT. 0)   THEN                                           
        WRITE(LUPRT,300) WORD, ALPHA1(I)                                     
        STOP                                                             
      END IF                                                            
      SECD(I,1)=BUFFIN(1)*1000.                                         
      SECD(I,2)=BUFFIN(2)*1000.                                         
      SECD(I,3)=BUFFIN(3)*1000.                                         
C                                                                       
C   READING OF MIN, MAX RECEIVER DEPTH AND STEP INCREMENT               
C                                                                       
      WORD= BLANK
 1200 READ(LUIN, 200, END= 2000)   WORD
      IF( WORD .EQ. BLANK )   GO TO 1200
      CALL RFFORM(WORD,80,BUFFIN,1,3,ISTAT)                             
      IF(ISTAT .GT. 0)   THEN                                           
       WRITE(LUPRT,300) WORD, ALPHA1(I)                                     
       STOP                                                             
      END IF                                                            
      PULRD(1)=BUFFIN(1)                                                
      PULRD(2)=BUFFIN(2)                                                
      PULRD(3)=BUFFIN(3)                                                
      IF( (PULRD(1) .LT. 0.)                      .OR.                  
     &    (PULRD(1) .GT. H0BEG+H1BEG)                .OR.                  
     &    (PULRD(2) .LE. 0.)                      .OR.                  
     &    (PULRD(2) .GT. H0BEG+H1BEG)                .OR.                  
     &    (PULRD(3) .LT. 0.)                      .OR.                  
     &    (PULRD(3) .GT. (PULRD(2)-PULRD(1)))     .OR.                  
     &    (PULRD(1) .GT. PULRD(2)))                    THEN             
       WRITE(LUPRT,400) ALPHA1(I),(BUFFIN(J),J=1,3),H0BEG+H1BEG               
       write(luprt,*) 'h0beg,h1=',h0beg,h1beg
       write(luprt,*) 'pulrd=',(pulrd(jj),jj=1,3)
       STOP                                                             
      END IF                                                            
C                                                                       
C   READING OF SOURCE DEPTH                                             
C                                                                       
      WORD= BLANK
 1400 READ(LUIN, 200, END= 2000) WORD
      IF( WORD .EQ. BLANK )   GO TO 1400
      CALL RFFORM(WORD,80,BUFFIN,1,1,ISTAT)                             
      IF(ISTAT.GT.0)   THEN                                             
      WRITE(LUPRT,300) WORD, ALPHA1(I)                                      
      END IF                                                            
      K=1                                                               
      SRD(I,K,1)=BUFFIN(1)                                              
      IF( (SRD(I,K,1) .LE. 0.) .OR.                                     
     &    (SRD(I,K,1) .GT. H0BEG+H1BEG) )   THEN                          
       WRITE(LUPRT,500) ALPHA1(I),BUFFIN(1),H0BEG+H1                       
       STOP                                                             
      END IF                                                            
C                                                                       
      RETURN                                                            
 2000 CONTINUE
      WRITE(LUPRT,600)
      STOP
      END                                                               
