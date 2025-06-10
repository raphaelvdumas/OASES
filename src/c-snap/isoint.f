C   ISOINT.FOR                                                          
      SUBROUTINE ISOINT(ISO,MODEN,
     & DH0I,MH0I,MSEDI,DSEDI,        
     & FRQ,SPEED,ADA,C0,Z0,C1,Z1, *)
C                                                                       
C___________________________________________________________            
C                                                          |            
C     This routine isolates the eigenvalues by counting    |            
C     sign changes in the Sturm sequence. The counting     |            
C     is done by the routine STURM                         |            
C__________________________________________________________|            
C                                                                       
                                                                        
      INTEGER EXTPOL                                                    
                                                                 
      DOUBLE PRECISION CMIN, H0, H1, STEP                                     
      DOUBLE PRECISION TWOPI, PI, OMEGA, FRQ                            
      DOUBLE PRECISION ADA( * ), SPEED( * ), ISO( * )
      DOUBLE PRECISION EIGREF, EIGMIN, EIGMAX, EIGVAL, MINEIG           
      DOUBLE PRECISION DH0I, DSEDI, STIFF                               
      DOUBLE PRECISION C0(*), Z0(*), C1(*), Z1(*)           
                                                                        
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /G/ H0, H1                                                 
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF                        
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL
      COMMON /NA/ ND0, ND1, CMIN                                        
      COMMON /TRIGON/ TWOPI, PI, OMEGA                                  
C                                                                       
                                                                        
  100 FORMAT(1X,/,' *** WARNING : ',/,' MODE CUTOFF FOR SOURCE',        
     & ' FREQUENCY: ',F9.2,'Hz  DETECTED IN SUB ISOINT.',/,
     & ' EXECUTION IS TERMINATED FOR THIS SOURCE FREQUENCY. ')          
  200 FORMAT(1X,/,' *** WARNING : ',/,' THE MAXIMUM ORDER MODE',        
     & ' FOR WHICH COMPUTATION IS WANTED IS',/,                         
     & ' LARGER THAN THE MAXIMUM ORDER EXISTING MODE. ',/,              
     & ' EXECUTION IS TERMINATED FOR THIS SOURCE FREQUENCY. ')          
  300 FORMAT(1H ,/    /,' SOURCE FREQUENCY     =',F9.2,' HZ',/,         
     & ' ESTIMATED MAXIMUM NO. OF MODES =',I5,/)                        
  400 FORMAT(1X,/,' *** WARNING: ',/,
     & ' THE MAXIMUM NUMBER OF MODES IN THE SOURCE FIELD WILL BE',
     & ' ADJUSTED TO',/,' MATCH THE',F8.2,' deg',
     & ' HALF_BEAMWIDTH LIMITATION SPECIFIED ON INPUT.')
  500 FORMAT(1X,'THE RESULTING HIGHEST ORDER MODE IS:',I5,' .',//)
C                                                                       
      DO 1000 I= 1, MODEN                                                 
 1000 ISO(I)= 0.D0                                                       
                                                                        
      CALL MDIAG(MH0I,MSEDI,DH0I,DSEDI,                                 
     & SPEED,ADA,C0,Z0,C1,Z1)                                           
                                                                        
      MINEIG=EIGMIN*1.00001D0                                           
                                                                        
C     FIND THE ESTIMATED NUMBER OF PROPAGATING MODES: NCROSS.           
                                                                        
      CALL STURM(MINEIG,MH0I,MSEDI,DH0I,DSEDI,NCROSS,ADA)               
                                                                        
      IF( (FLAGPU.LT.1.0) .AND. (FIRST.EQ.0.0) )   THEN
        WRITE(LUPRT,300) FRQ, NCROSS 
        IF( BPHVEL .GT. 0.0 )   THEN
          MINEIG= ( (OMEGA*H0)/(DBLE(BPHVEL)*MH0I) )**2
          WRITE(LUPRT,400) HBEAM
C         FIND THE ESTIMATED NUMBER OF MODES IN THE HALF BEAM SOURCE.
          CALL STURM(MINEIG,MH0I,MSEDI,DH0I,DSEDI,NCROSS,ADA)               
          WRITE(LUPRT,500) NCROSS
        END IF
        FIRST=1.0                                                         
      END IF

      MAXMOD=MIN(MAXMOD,NCROSS)                                         


      IF(MAXMOD .LT. 1)   THEN                                          
       WRITE(LUPRT,100) FRQ                                                 
       RETURN 1                                                           
      END IF                                                            
      IF(MAXMOD .LT. MINMOD)   THEN                                       
       WRITE(LUPRT,200)                                                     
       RETURN 1                                                           
      END IF                                                            
                                                                        
                                                                        
C                                                                       
      MAXIND=MIN(MODEN,NCROSS-MINMOD+2)                                 
      ISO(MAXIND)=MINEIG                                                
C                                                                       
C     ISOLATE THE EIGENVALUES                                           
C                                                                       

      IF(MINMOD.EQ.1) THEN
        ISO(1)=EIGMAX                                                   
        M1=1                                                            
      ELSE                                                              
        M1=MINMOD-1                                                     
      ENDIF                                                             
 2000 CONTINUE                                                          
      EIGVAL=(EIGMAX+MINEIG)*.5D0                                       
      STEP=(EIGMAX-MINEIG)*.5D0
 3000 CONTINUE                                                          
      CALL STURM(EIGVAL,MH0I,MSEDI,DH0I,DSEDI,M,ADA)
      STEP=STEP*0.5D0                                                    
      INDEX=MIN(MODEN,M-MINMOD+2)                                       
      INDEX=MAX(1,INDEX)                                                
      IF(INDEX .GT. 1)   THEN                                           
        IF(ISO(INDEX).EQ.0.D0.AND.M+1.GE.MINMOD.AND.M.LE.MAXMOD)        
     &         ISO(INDEX)=EIGVAL                                        
      ELSE                                                              
        ISO(1)=EIGVAL                                                   
      END IF                                                            
      IF(M.GT.M1) THEN                                                  
        EIGVAL=EIGVAL+STEP                                              
        GO TO 3000                                                      
      ELSEIF(M.LT.M1) THEN                                              
        EIGVAL=EIGVAL-STEP                                              
        GO TO 3000                                                      
      ENDIF                                                             
 4000 CONTINUE                                                          
      M1=M1+1                                                           
      IF(M1.EQ.MAXMOD+1)    RETURN             
      IF(ISO(M1-MINMOD+2).EQ.0.D0) THEN                                 
        EIGMAX=ISO(M1-MINMOD+1)                                         
        GO TO 2000                                                      
      ELSE                                                              
        GO TO 4000                                                      
      ENDIF                                                             
      END                                                               
