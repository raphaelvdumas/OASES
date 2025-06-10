C  RDNUMB.FOR                                                           
      SUBROUTINE RDNUMB(I, WORD, REREAD, ALPHA1, H1BEG,                
     & H0BEG, SECD, SRD, LUIN, *, *)                        
                                                                        
      INTEGER REREAD, ERROR                                             
                                                                        
      CHARACTER*80 WORD                                                 
      CHARACTER*5 ALPHA1(NOPT)                                          
                                                                        
      DIMENSION SRD(NOPT,KSRD,2)                                        
      DIMENSION SECD(NOPT,3)                                            
C      DIMENSION BUFFIN(4)                                               

      DOUBLE PRECISION H1                                          
                                                                        
      COMMON /CFIELD/ FLDRD(3),PULRD(3)                                 
      COMMON /CONTUR/ CNTR1(4,4)                                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /PARA1/ NFF,MSP,NDEP,NOPT,ICF,KSRD
                                                                        
  125 FORMAT(A80)                                                       
  200 FORMAT(1X,/,' REVISE THE FOLLOWING INPUT LINE FOR OPTION',/,      
     & ' "',A5,'" :',/,1X,A80,/,                                        
     & ' EXECUTION IS TERMINATED.')                                     
                                                                        
C   CNTR1 CONTAINS THE VARIOUS ZMIN, ZMAX, AND ZLEV            
C   COMBINATIONS FOR CONTOURED REPRESENTATIONS.                
                                                                        
                                                                        
C     THE CURRENT RECORD (WORD) BEGINS WITH A NUMERICAL VALUE.          
C     THIS IS AN ERROR CONDITION FOR THE FOLLOWING OPTIONS:             
      IF( (ALPHA1(I).EQ.'PROFL').OR.                                    
     &    (ALPHA1(I).EQ.'MODES').OR.                                    
     &    (ALPHA1(I).EQ.'GROUP').OR.                                    
     &    (ALPHA1(I).EQ.'PARAM')     )   THEN                           
        REREAD=1                                                        
        RETURN 1                                                        
      ELSE                                                              
                                                                        
C             TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR        
C             PROFL MODES CONDA HORWN PARAM FIELD PULSE                    
        GO TO(4800 ,6000 ,4800 ,4800 ,4800 ,4800 ,4800 ,4800 ,4800 ,    
     &        6000 ,6000 ,4800 ,6000 ,6000 ,4800 ,4700 ),I              
                                                                        
 4700   CONTINUE                                                        
        CALL IPULSE( WORD, ALPHA1, SECD, SRD, NOPT,                     
     &  I, KSRD, H0BEG, H1BEG, LUIN)                                      
        REREAD=0                                                        
        RETURN 1                                                        
C                                                                       
C   READING OF INPUT DATA FOR TLRAN,TLAVR,TLDEP,TLAVF,ANGLE             
C   PHASE, CONDR, CONDA, AND CONFR OPTIONS.                             
C                                                                       
 4800   CONTINUE                                                        
C       READING OF RMIN,RMAX,DELR IN KILOMETERS.                        
C       VALUES ARE THEN CONVERTED TO METERS.                            
        CALL RFFORM(WORD,80,SECD(I,1),NOPT,3,ERROR)                     
        IF(ERROR .GT. 0)   THEN                                         
          WRITE(LUPRT,200) ALPHA1(I),WORD                                   
          RETURN                                                        
        END IF                                                          
        SECD(I,1)=SECD(I,1)*1000.                                       
        SECD(I,2)=SECD(I,2)*1000.                                       
        SECD(I,3)=SECD(I,3)*1000.
        IF(SECD(I,1) .GT. SECD(I,2))   THEN
          WRITE(LUPRT,200) ALPHA1(I),WORD                                   
          RETURN
        END IF
        IF( (SECD(I,1).EQ.0.0) .AND. (SECD(I,2).EQ.0.0) )   THEN
          WRITE(LUPRT,200) ALPHA1(I),WORD                                   
          RETURN
        END IF
        SECD(I,3)= MIN(SECD(I,3),SECD(I,2)-SECD(I,1))                                       
        IF((SECD(I,3) .EQ. 0.) .AND.                                    
     &     (SECD(I,1) .NE. SECD(I,2)))   THEN                           
          WRITE(LUPRT,200) ALPHA1(I),WORD                                   
          RETURN                                                        
        END IF                                                         
                                                                        
CC         IF((ALPHA1(I).EQ.'CONDA').OR.(ALPHA1(I).EQ.'CONDR').OR.        
CC     &      (ALPHA1(I).EQ.'CONFR').OR.(ALPHA1(I).EQ.'CONSV'))   THEN    
CC           IF(I.EQ.8)   ICON=1                                          
CC           IF(I.EQ.9)   ICON=2                                          
CC           IF(I.EQ.12)  ICON=3                                          
CC           IF(I.EQ.17)  ICON=4                                          
C   ZMIN, ZMAX, DELZ, Number of smoothings                              
C          READ(LUIN,*)(CNTR1(ICON,L),L=1,3)                            
CC           DO 5000   L= 1, 4                                            
CC           BUFFIN(L)= 0.0                                               
CC 5000      CONTINUE                                                     
CC           READ(LUIN,125) WORD                                          
CC           CALL RFFORM(WORD,80,BUFFIN,1,4,ERROR)                        
CC           IF(ERROR .GT. 0)   THEN                                      
CC             WRITE(LUPRT,200) ALPHA1(I),WORD                                
CC             RETURN                                                     
CC           END IF                                                       
CC           DO 5200   L= 1, 4                                            
CC           CNTR1(ICON,L)= BUFFIN(L)                                     
CC 5200      CONTINUE                                                     
CC         END IF                                                          
      END IF                                                            
C                                                                       
      RETURN 2                                                          
 6000 RETURN 1                                                          
      END                                                               
