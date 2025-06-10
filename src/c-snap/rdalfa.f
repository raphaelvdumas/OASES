      SUBROUTINE RDALFA(I,REREAD,WORD,ALPHA1,                        
     & AX,AY,AS,STORE,*   ,*    )                              
C                     7000 3500                                
C                                                                       
      DIMENSION AX(NOPT,6), AY(NOPT,7), AS(5)                           
      DIMENSION BUFFIN(4)                                      
      DIMENSION STORE(NOPT)                                             
                                                                        
      INTEGER REREAD, ERROR                                             
      CHARACTER*80 WORD                                                 
      CHARACTER*5 ALPHA1(NOPT)                                          
      COMMON /CONTUR/ CNTR1(4,4)
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /PARA1/ NFF,MSP,NDEP,NOPT,ICF,KSRD
C                                                                       
  200 FORMAT(1H ,//,'  REVISE PLOT AXIS DEFINITION FOR OPTION ',A6,     
     & /,2X,A80,/,'  THE EXECUTION IS TERMINATED ')                     
  300 FORMAT(1H ,' REVISE THE FOLLOWING INPUT LINE:',/,2X,A80,/         
     & ,' EXECUTION IS TERMINATED')                                     
C  400 FORMAT(1X ,' WARNING: FREQ VALUES ON LOG AXIS MUST BE .GT. 0 ')  
  600 FORMAT(1H ,//,'  REVISE DEPTH SPECIFICATION FOR OPTION "',A5,     
     & '"',/,1X,' DEPTH',A75,/,'  EXECUTION IS TERMINATED')             
C                                                                       
C                                                                       
       DO 1000   J= 1, 4
       BUFFIN(J)= 0.0
 1000  CONTINUE
       IF(WORD(1:6) .EQ. 'XAXIS ')   THEN                               
        WORD(1:6)='      '                                              
        CALL RFFORM(WORD,80,AX(I,1),NOPT,5,ERROR)                       
        IF( (ERROR .GT. 0) .OR. (AX(I,1).EQ.AX(I,2)))   THEN            
         WORD(1:5)='XAXIS'                                             
         WRITE(LUPRT,200) ALPHA1(I),WORD                                    
         RETURN                                                         
        ELSE                                                            
         AX(I,6)= AX(I,5)                                               
         AX(I,5)=1.0                                                    
C              TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR    
C              PROFL MODES CONDA HORWN PARAM FIELD PULSE CONSV                
         GO TO(1001 ,1999 ,1001 ,1999 ,1999 ,1999 ,1999 ,1001 ,1001 ,   
     &         1999 ,1999 ,1001 ,1999 ,1999 ,1999 ,1999 ,1001),I             
C                                                                       
 1001    CONTINUE                                                       
         AX(I,1)=AX(I,1)*1.0E3                                          
         AX(I,2)=AX(I,2)*1.0E3                                          
         AX(I,4)=AX(I,4)*1.0E3                                          
 1999    CONTINUE                                                       
        END IF                                                          
                                                                        
       ELSE IF(WORD(1:6).EQ.'YAXIS ')   THEN                            
                                                                        
        WORD(1:6)='      '                                              
        CALL RFFORM(WORD,80,AY(I,1),NOPT,4,ERROR)                       
        IF( (ERROR .GT. 0) .OR. (AY(I,1).EQ.AY(I,2)))   THEN            
         WORD(1:5)='YAXIS'                                             
         WRITE(LUPRT,200) ALPHA1(I),WORD                                    
         RETURN                                                         
        ELSE                                                            
         AY(I,5)=1.0                                                    
        END IF                                                          
                                                                        
       ELSE IF(WORD(1:6).EQ.'SAXIS ')   THEN                            
                                                                        
        IF( ALPHA1(I) .EQ. 'PROFL' )   THEN
          WORD(1:6)='      '                                            
          CALL RFFORM(WORD,80,AS(1),1,4,ERROR)                          
          IF( (ERROR .GT. 0) .OR. (AS(1).EQ.AS(2)))   THEN              
           WORD(1:5)='SAXIS'                                           
           WRITE(LUPRT,200) ALPHA1(I),WORD                                  
           RETURN                                                       
          ELSE                                                          
           AS(5)=1.0                                                    
          END IF                                                        
        ELSE                                                            
          WRITE(LUPRT,*) ' WARNING : "SAXIS" NOT IN LEGAL CONTEXT '     
          WRITE(LUPRT,*) ' EXECUTION IS TERMINATED '                    
          RETURN                                                        
        END IF                                                          

       ELSE IF(WORD(1:6).EQ.'ZAXIS ')   THEN                            
                                                                        
        IF( (ALPHA1(I) .EQ. 'CONDR') .OR.
     &      (ALPHA1(I) .EQ. 'CONFR') .OR.
     &      (ALPHA1(I) .EQ. 'CONSV')      )   THEN
          WORD(1:6)='      '
          CALL RFFORM(WORD,80,BUFFIN,1,4,ERROR)                          
          IF( (ERROR .GT. 0) )   THEN
           WORD(1:5)='ZAXIS'                                           
           WRITE(LUPRT,200) ALPHA1(I),WORD                                  
           RETURN                                                       
          ELSE
           IF(I.EQ.8)   ICON=1
           IF(I.EQ.9)   ICON=2
           IF(I.EQ.12)  ICON=3
           IF(I.EQ.17)  ICON=4
C   ZMIN, ZMAX, DELZ, and Number of smoothings
           CNTR1(ICON,1)= BUFFIN(1)
           CNTR1(ICON,2)= BUFFIN(2)
           CNTR1(ICON,3)= BUFFIN(3)
           CNTR1(ICON,4)= BUFFIN(4)
          END IF                                                        
        ELSE                                                            
          WRITE(LUPRT,*) ' WARNING : "ZAXIS" NOT IN LEGAL CONTEXT '     
          WRITE(LUPRT,*) ' EXECUTION IS TERMINATED '                    
          RETURN                                                        
        END IF                                                          
                                                                        
       ELSE IF(WORD(1:5) .EQ. 'DEPTH')   THEN                           
        WORD(1:5)='     '                                               
        CALL RFFORM(WORD,80,BUFFIN,1,2,ERROR)                           
        IF((ERROR .EQ. 0) .AND. (BUFFIN(2) .GT. BUFFIN(1)) .AND.        
     &      (BUFFIN(1) .GE. 0.))   THEN                                 
         AY(I,6)=BUFFIN(1)                                              
         AY(I,7)=BUFFIN(2)                                              
        ELSE                                                            
         WRITE(LUPRT,600) ALPHA1(I),WORD(6:80)                              
         RETURN                                                         
        END IF                                                          
                                                                        
       ELSE IF(WORD(1:6).EQ.'FILE  ')    THEN                           
        STORE(I)=1.0                                                    
                                                                        
C      AT THIS POINT WORD(1:6) CAN BE A VALID OPTION NAME ONLY IF IT    
C      OCCURS AFTER ONE OF THE FOLLOWING OPTIONS:                       
       ELSE IF( (ALPHA1(I).EQ.'PROFL').OR.                              
     &          (ALPHA1(I).EQ.'MODES').OR.                              
     &          (ALPHA1(I).EQ.'GROUP').OR.                              
     &          (ALPHA1(I).EQ.'PARAM').OR.                              
     &          (ALPHA1(I).EQ.'CONSV')     )   THEN                     
        REREAD=1                                                        
        RETURN 1                                                        
       ELSE                                                             
        PRINT *,' MESSAGE FROM RDALFA '
        WRITE(LUPRT,300) WORD                                               
        RETURN                                                          
       END IF                                                           
       RETURN 2                                                         
       END                                                              
