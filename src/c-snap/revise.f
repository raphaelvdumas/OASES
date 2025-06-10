C   REVISE.FOR                                                          
      SUBROUTINE REVISE(I,FF,NFREQ,ALPHA1,
     & H0,H0MAX,H1,H0BEG,          
     & NINC,NXVAL,                                                      
     & FLAG,AX,AY,SECD,SRD,HMAX,RMAX,NSECT,*   )               
C                                          9999                
                                                                        

      CHARACTER*5 ALPHA1(NOPT)                                          
                                                                        
      REAL FLAG(NOPT,ICF), AX(NOPT,6), AY(NOPT,7)
      REAL SRD(NOPT,KSRD,2)                                             
      REAL FF(*)
      REAL SECD(NOPT,3)                                                 
                                                                        
      DOUBLE PRECISION H0, H1, CC0, CC1                                 
                                                                        
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /CFIELD/ FLDRD(3), PULRD(3)                                
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MODSMP/ MSPH0, MODSED, MSPH1                              
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /PARA1/ NFF,MSP,NDEP,NOPT,ICF,KSRD
                                                                        
  200 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  DEPTH INTERVAL SPECIFICATION NON IMPLEMENTED YET.')           
  220 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  UNCORRECT DEPTH INTERVAL SPECIFICATION.',/,                   
     & '  EXECUTION IS TERMINATED.')                                    
  240 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  THE INPUT SPECIFICATION OF THE DEPTH INTERVAL WILL BE',       
     & ' IGNORED ',/,'  FOR THIS OPTION. ALL SOURCE/RECEIVER DEPTHS',   
     & ' NOT EXCEEDING ',/,'  THE SEDIMENT DEPTH ARE ACCEPTABLE.')      
  260 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  MAX DEPTH IS REDUCED TO NOT EXCEED SEDIMENT DEPTH.')          
  280 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  DEFAULT RANGE AXIS SPECIFICATION IS NOT APPLICABLE',          
     & ' FOR ONE RANGE POINT ONLY.',/,'  EXECUTION IS TERMINATED.')       
  300 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  DEFAULT FREQUENCY AXIS SPECIFICATION IS NOT APPLICABLE',      
     & ' FOR ONE FREQUENCY ONLY.',/,'  EXECUTION IS TERMINATED.')       
  320 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  THE INPUT SPECIFICATION OF THE DEPTH INTERVAL IS',            
     & ' MEANINGLESS ',/,'  FOR THIS OPTION. ')                         
  410 FORMAT(1H ,/,'  EXECUTION TERMINATED BECAUSE OF ARRAY SIZE ',     
     & 'LIMITATIONS.')                                                 
  415 FORMAT(1H ,//,'  REVISE THE "X" AXIS DEFINITION FOR OPTION "',A5, 
     & '" :',/,'  THE NUMBER OF SUBDIVISIONS CANNOT EXCEED', I3,' .')     
  420 FORMAT(1H ,//,'  REVISE THE "Y" AXIS DEFINITION FOR OPTION "',A5, 
     & '" :',/,'  THE NUMBER OF SUBDIVISIONS CANNOT EXCEED', I3,' .')     
  440 FORMAT(1H ,//,'  REVISE THE "X" AXIS DEFINITION FOR OPTION "',A5, 
     & '" :',/,'  EXECUTION IS TERMINATED. ')                             
  710 FORMAT(1H ,///,' REVISE RMIN,RMAX AND/OR "X" AXIS INPUT ',       
     & 'VALUES FOR OPTION ',A6,'.',//,                                  
     & ' CALCULATION:',3X,'MINIMUM RANGE',2X,F11.2,2X,'MAXIMUM RANGE',  
     & 3X,F11.2,/,                                                      
     & ' PLOT AXIS  :',3X,'LEFT HAND VALUE',1X,F11.2,2X,                 
     & 'RIGHT HAND VALUE',1X,F11.2,/,' EXECUTION IS TERMINATED.',//)     
  720 FORMAT(1H ,///,'  REVISE FREQUENCY AND/OR "X" AXIS INPUT VALUES', 
     & ' FOR OPTION ',A6,'.',//,                                        
     & ' CALCULATION:',3X,'MINIMUM FREQ ',2X,F11.2,2X,'MAXIMUM FREQ ',  
     & 3X,F11.2,/,                                                      
     & '  "X" AXIS  :',3X,'LEFT HAND VALUE',1X,F11.2,2X,
     & 'RIGHT HAND VALUE',
     & 1X,F11.2,/,'  EXECUTION IS TERMINATED.',//)                      
  730 FORMAT(1H ,///,'  REVISE FREQUENCY AND/OR YAXIS INPUT VALUES FOR',
     & ' OPTION ',A6,'.',//,                                            
     & ' CALCULATION:',3X,'MINIMUM FREQ ',2X,F11.2,2X,'MAXIMUM FREQ ',  
     & 3X,F11.2,/,                                                      
     & '  "Y" AXIS  :',3X,'MIN VALUE',6X,F11.2,3X,'MAX VALUE',7X,F11.2, 
     & /,'  EXECUTION IS TERMINATED.',//)                                
  740 FORMAT(1X ,/,'  WARNING FOR OPTION "',A5,'" :',/,                   
     & '  FREQUENCY VALUES ON LOG AXIS MUST BE GREATER THAN ZERO.',/,    
     & '  EXECUTION IS TERMINATED.')                                    
                                                                        
C
C    FLAG(IOP,1)   =>  Y/N FOR OPTION ALPHA1(IOP)
C    FLAG(IOP,2)   =>  PRT
C    FLAG(IOP,3)   =>  PLT
C    FLAG(IOP,4)   =>  COH
C    FLAG(IOP,5)   =>  INC
C    FLAG(IOP,6)   =>  COL
C

                                                                        
      MODSED=0                                                          
                                                                        
      IF(FLAG(I,3) .GT. 0.)   THEN                                      
                                                                        
C     REVISING "X" AXIS                                                 
                                                                        
C            TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR      
C            PROFL MODES CONDA HORWN PARAM FIELD PULSE CONSV                  
       GO TO(2001 ,2002 ,2001 ,2004 ,2005 ,2004 ,2004 ,2001 ,2001 ,     
     &       2010 ,2011 ,2001 ,2004 ,3000 ,3000 ,3000 ,2017),I               
                                                                        
 2001  CONTINUE                                                         
       IF(AX(I,5) .LT. 1.0)   THEN                                      
        IF(SECD(I,1) .NE. SECD(I,2))   THEN                             
C        TAKE DEFAULT ACTION
         AX(I,1)=SECD(I,1)                                              
         AX(I,2)=SECD(I,2)                                              
         AX(I,3)=ABS( (AX(I,2)-AX(I,1)) / AX(I,3) )                     
         AX(I,4)=ABS(AX(I,2)-AX(I,1))/5.                                
        ELSE                                                            
         WRITE(LUPRT,280) ALPHA1(I)                                         
         RETURN                                                         
        END IF                                                          
       ELSE                                                             
        AX(I,3)=ABS((AX(I,2)-AX(I,1))/AX(I,3))                          
        IF(MAX(SECD(I,1),AX(I,1)) .LT. MIN(AX(I,2),SECD(I,2)))  THEN    
         IF(FLAG(I,2) .LE. 0.0)   THEN                                  
          SECD(I,1)=MAX(AX(I,1),SECD(I,1))                              
          SECD(I,2)=MIN(AX(I,2),SECD(I,2))                              
         END IF                                                         
        ELSE                                                            
         WRITE(LUPRT,710) ALPHA1(I),SECD(I,1),SECD(I,2),AX(I,1),AX(I,2)     
         RETURN                                                         
        END IF                                                          
       END IF                                                           
       GO TO 3000                                                       
                                                                        
 2002  CONTINUE                                                         
       IF(AX(I,5) .LT. 1.0)   THEN                                      
        IF(NFREQ .GT. 1)   THEN                                         
         AX(I,1)=FF(1)                                                  
         AX(I,2)=FF(NFREQ)                                              
         AX(I,3)=ABS( (AX(I,2)-AX(I,1)) / AX(I,3) )                     
         AX(I,4)=ABS( AX(I,2)-AX(I,1) )/5.0                             
        ELSE                                                            
         WRITE(LUPRT,300) ALPHA1(I)                                         
         RETURN                                                         
        END IF                                                          
       ELSE                                                             
        AX(I,3)=ABS((AX(I,2)-AX(I,1))/AX(I,3))                          
        IF(MAX(FF(1),AX(I,1)) .GE. MIN(AX(I,2),FF(NFREQ)))   THEN       
         WRITE(LUPRT,720) ALPHA1(I),FF(1),FF(NFREQ),AX(I,1),AX(I,2)         
         RETURN                                                         
        END IF                                                          
       END IF                                                           
       GO TO 3000                                                       
                                                                        
 2004  CONTINUE                                                         
       AX(I,3)=ABS( (AX(I,2)-AX(I,1)) / AX(I,3) )                       
       GO TO 3000                                                       
                                                                        
 2005  CONTINUE                                                         
       IF(AX(I,5).LT. 1.0)   THEN                                       
        IF(NFREQ .GT. 1)   THEN                                         
         AX(I,1)=FF(1)                                                  
         AX(I,2)=FF(NFREQ)                                              
         AX(I,3)=AX(I,3)/(LOG(AX(I,2)/AX(I,1))/LOG(2.0))                
         AX(I,4)=1.0                                                    
        ELSE                                                            
         WRITE(LUPRT,300) ALPHA1(I)                                         
         RETURN                                                         
        END IF                                                          
       ELSE                                                             
        IF(AX(I,1) .LE. 0.0)   THEN                                     
         WRITE(LUPRT,730) ALPHA1(I)                                         
         RETURN                                                         
        ELSE                                                            
         AX(I,3)= AX(I,3)/(LOG(AX(I,2)/AX(I,1))/LOG(2.0))               
         NXVAL=ALOG(AX(I,2)/AX(I,1))/ALOG(2.0)*AX(I,4)+1                
         IF(NXVAL .GT. NINC)   THEN                                     
          WRITE(LUPRT,415) ALPHA1(I), NINC                                  
          WRITE(LUPRT,410)                                                  
          RETURN                                                        
         END IF                                                         
        END IF                                                          
       END IF                                                           
       GO TO 3000                                                       
                                                                        
 2010  CONTINUE                                                         
       IF(NSECT .GT. 1)   THEN                                          
         IF(AX(I,5) .LT. 1.0)   THEN                                    
           AX(I,1)= 0.                                                  
           AX(I,2)= RMAX                                                
           AX(I,4)=ABS(AX(I,2)-AX(I,1))/5.                              
         ELSE                                                           
          IF( (AX(I,1) .GE. AX(I,2)) .OR. (AX(I,2) .LE. 0.0) )   THEN   
           WRITE(LUPRT,440) ALPHA1(I)                                       
           RETURN
          END IF
         END IF                                                         
       END IF                                                           
       GO TO 3000                                                       
                                                                        
 2011  CONTINUE                                                         
C      MODES ARE PLOTTED IN GROUPS OF 6                                 
       AX(I,3)=ABS(6.0*(AX(I,2)-AX(I,1))/AX(I,3))                       
       GO TO 3000                                                       
                                                                        
 2017  CONTINUE                                                         
       IF(NSECT .GT. 1)   THEN                                          
         IF(AX(I,5) .LT. 1.0)   THEN                                    
           AX(I,1)= 0.                                                  
           AX(I,2)= RMAX                                                
           AX(I,4)=ABS(AX(I,2)-AX(I,1))/5.                              
         ELSE                                                           
          IF( (AX(I,1) .GE. AX(I,2)) .OR. (AX(I,2) .LE. 0.0) )   THEN   
           WRITE(LUPRT,440) ALPHA1(I)                                       
           RETURN
          ELSE
           AX(I,3)=ABS( (AX(I,2)-AX(I,1)) / AX(I,3) )
          END IF
         END IF                                                         
       END IF                                                           
       GO TO 3000                                                       

      END IF                                                            
                                                                         
 3000  CONTINUE                                                         
                                                                        
                                                                        
C     REVISING "Y" AXIS                                                 
                                                                        
      IF(FLAG(I,3) .LT. 1.)   THEN
C            TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR      
C            PROFL MODES CONDA HORWN PARAM FIELD PULSE CONSV                 
       GO TO(4000 ,4000 ,4000 ,1000 ,4000 ,4000 ,1000 ,1000 ,4000 ,     
     &       4000 ,1000 ,4000 ,4000 ,4000 ,4000 ,4000 ,1000),I               
      END IF                                                            
                                                                        
 1000 CONTINUE                                                          
                                                                        
C           TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR       
C           PROFL MODES CONDA HORWN PARAM FIELD PULSE CONSV                  
      GO TO(3001 ,3001 ,3001 ,3004 ,3001 ,3001 ,3004 ,3004 ,3009 ,      
     &      3010 ,3011 ,3012 ,3013 ,4000 ,4000 ,4000 ,3004),I                
                                                                        
 3001 CONTINUE                                                          
      AY(I,3)=ABS((AY(I,2)-AY(I,1))/AY(I,3))                            
      GO TO 4000                                                        
                                                                        
 3004 CONTINUE                                                          
      IF(AY(I,5) .LE. 0.0)   THEN                                       
C       AY(I,6), AY(I,7) are used for controlling min-max               
C       calculation interval over depth.                                
        IF(AY(I,7) .LE. 0.0)   THEN                                     
          AY(I,1)=0.                                                    
          AY(I,2)=HMAX                                                  
          AY(I,3)=ABS( (AY(I,2)-AY(I,1))/AY(I,3))                       
          AY(I,4)=ABS(AY(I,2)-AY(I,1))/5.0                              
          AY(I,5)= 1.                                                   
CCC          AY(I,6)= AY(I,1)                                           
CCC          AY(I,7)= AY(I,2)                                           
        ELSE                                                            
CCC          AY(I,6)= MAX(0.0, AY(I,6))                                 
CCC          AY(I,7)= MIN(HMAX, AY(I,7))                                
          AY(I,1)= AY(I,6)                                              
          AY(I,2)= AY(I,7)                                              
          AY(I,3)=ABS( (AY(I,2)-AY(I,1))/AY(I,3))                       
          AY(I,4)=ABS(AY(I,2)-AY(I,1))/5.0                              
          AY(I,5)= 1.0                                                  
        END IF                                                          
      ELSE                                                              
        AY(I,3)=ABS((AY(I,2)-AY(I,1))/AY(I,3))                          
        IF(AY(I,2) .GT. H0BEG)   MODSED=1                              
CCC        IF(AY(I,7) .LE. 0.0)   THEN                                  
CCC          AY(I,6)= MAX(0.0, AY(I,1))                                 
CCC          AY(I,7)= MIN(HMAX, AY(I,2))                                
CCC        END IF                                                       
      END IF                                                            
      GO TO 4000                                                        
                                                                        
 3009 CONTINUE                                                          
      IF(AY(I,5) .LE. 0.0)   THEN                                       
       AY(I,1)=FF(1)                                                    
       AY(I,2)=FF(NFREQ)                                                
      END IF                                                            
      GO TO 4000                                                        
 3010 CONTINUE                                                          
      IF( NSECT .EQ. 1 )   THEN
        IF(AY(I,5) .LE. 0.0)   THEN                                       
         IF(AY(I,7) .LE. 0.0)   THEN                                      
          AY(I,1)=0.                                                      
          AY(I,2)=H0                                                      
          AY(I,4)=ABS(AY(I,1)-AY(I,2))/5.                                 
         END IF                                                           
        ELSE                                                              
        END IF                                                            
        GO TO 4000                                                        
      ELSE
        IF(AY(I,5) .LE. 0.0)   THEN                                       
         IF(AY(I,7) .LE. 0.0)   THEN                                      
          AY(I,1)=0.                                                      
          AY(I,2)=H0MAX                                                      
          AY(I,4)=ABS(AY(I,1)-AY(I,2))/5.                                 
         END IF                                                           
        ELSE                                                              
        END IF                                                            
        GO TO 4000                                                        
      END IF   
                                                                     
 3011 CONTINUE                                                          
      IF(AY(I,5) .LE. 0.)    THEN                                       
       IF(AY(I,7) .LE. 0.0)   THEN                                      
        AY(I,1)=0.                                                      
        AY(I,2)=H0BEG                                                      
        AY(I,3)=ABS(AY(I,1)-AY(I,2))/12.0                               
        AY(I,4)=ABS(AY(I,1)-AY(I,2))/5.                                 
       END IF                                                           
      ELSE                                                              
       AY(I,3)=ABS((AY(I,2)-AY(I,1))/AY(I,3))                           
       IF(AY(I,2) .GT. H0)   MODSED=1                                   
      END IF                                                            
      GO TO 4000                                                        
                                                                        
 3012 CONTINUE                                                          
      IF(AY(I,5) .LE. 0.0)    THEN                                      
       IF(NFREQ .GT. 1)   THEN                                          
        AY(I,1)=FF(1)                                                   
        AY(I,2)=FF(NFREQ)                                               
        AY(I,3)=ABS((AY(I,1)-AY(I,2))/AY(I,3))                          
        AY(I,4)=ABS(AY(I,2)-AY(I,1))/5.0                                
       ELSE                                                             
        WRITE(LUPRT,300) ALPHA1(I)                                          
        RETURN                                                          
       END IF                                                           
      ELSE                                                              
       YMIN=MIN(AY(I,1),AY(I,2))                                        
       YMAX=MAX(AY(I,1),AY(I,2))                                        
       IF(MAX(FF(1),YMIN) .GE. MIN(YMAX,FF(NFREQ)))   THEN              
        WRITE(LUPRT,740) ALPHA1(I),FF(1),FF(NFREQ),AY(I,1),AY(I,2)          
        RETURN                                                          
       ELSE                                                             
        AY(I,3)=ABS((AY(I,1)-AY(I,2))/AY(I,3))                          
       END IF                                                           
      END IF                                                            
      GO TO 4000                                                        
 3013 CONTINUE                                                          
      GO TO 4000                                                        
                                                                        
 4000 CONTINUE                                                          
                                                                        
C     REVISING "DEPTH OR FREQUENCY INTERVAL"
C           TLRAN GROUP TLAVR TLDEP TLAVF ANGLE PHASE CONDR CONFR       
C           PROFL MODES CONDA HORWN PARAM FIELD PULSE CONSV                  
      GO TO(5001 ,5002 ,5003 ,5004 ,5003 ,5001 ,5004 ,5004 ,5009 ,      
     &      5004 ,5004 ,5003 ,9999 ,9999 ,5001 ,5001 ,5004),I                
C                                                                       
                                                                        
 5001 CONTINUE                                                          
      IF(AY(I,7) .GT. 0.0)   WRITE(LUPRT,240) ALPHA1(I)                     
      AY(I,6)=0.                                                        
      AY(I,7)=0.                                                        
      GO TO 6000                                                        
                                                                        
 5002 CONTINUE                                                          
      IF(AY(I,7) .GT. 0.0)   WRITE(LUPRT,320) ALPHA1(I)                     
      AY(I,6)=0.                                                        
      AY(I,7)=0.                                                        
      GO TO 6000                                                        
                                                                        
 5003 CONTINUE                                                          
      IF(AY(I,7) .GT. 0.0)   WRITE(LUPRT,200) ALPHA1(I)                     
      AY(I,6)=0.                                                        
      AY(I,7)=H0                                                        
      GO TO 6000                                                        
                                                                        
 5004 CONTINUE                                                          
                                                                        
      IF(AY(I,7) .LE. 0.0)   THEN                                       
                                                                        
CCC       IF(AY(I,5) .GT. 0.0)   THEN                                   
        AY(I,6)= MAX(0.0, AY(I,1))                                      
        AY(I,7)= MIN(HMAX, AY(I,2))                                     
CCC       ELSE                                                          
CCC        AY(I,7)= HMAX                                                
CCC       END IF                                                        
                                                                        
      ELSE                                                              
                                                                        
        IF(AY(I,7) .GT. HMAX)   THEN                                    
          WRITE(LUPRT,260) ALPHA1(I)                                        
          AY(I,7)=HMAX                                                  
          MODSED=1                                                      
          IF(AY(I,7) .LT. AY(I,6))   THEN                               
            WRITE(LUPRT,220) ALPHA1(I)                                      
            RETURN                                                      
          END IF                                                        
        ELSE                                                            
          IF(AY(I,7) .GT. H0)   MODSED=1                                
        END IF                                                          
                                                                        
      END IF                                                            
      GO TO 6000

 5009 CONTINUE


 6000 CONTINUE                                                          
                                                                        
      IF( H1 .EQ. 0.0 )   THEN                                          
       MODSED=0                                                         
       RETURN 1                                                         
      END IF                                                            
      IF(MODSED .LE. 0)   THEN                                          
       DO 8200   K=1,KSRD                                               
       IF(SRD(I,K,1) .LE. 0)   GO TO 8300                               
       DO 8100   J=1,2                                                  
       IF(SRD(I,K,J) .GT. H0BEG)   THEN                                
        MODSED=1                                                        
        GO TO 9999                                                      
       END IF                                                           
 8100  CONTINUE                                                         
 8200  CONTINUE                                                         
 8300  CONTINUE                                                         
       IF(FLDRD(2) .GT. H0)   MODSED=1                                  
       IF(PULRD(2) .GT. H0)   MODSED=1                                  
      END IF                                                            
                                                                        
 9999 CONTINUE                                                          
                                                                        
      RETURN 1                                                          
      END                                                               
