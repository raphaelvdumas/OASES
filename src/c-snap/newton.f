C  NEWTON.FOR                                                           
                                                                        
      SUBROUTINE NEWTON( *, *, MY, MAXMSH, MODEN, MODQTY,
     & DH0, MH0I, I, DSED, MSEDI, ADA)
                                                                        
C____________________________________________________________           
C                                                            |          
C     This routine finds the zeroes in the characteristiC    |          
C     equation for the second and subsequent meshes.         |          
C____________________________________________________________|          
                                                                        
                                                                        
      DOUBLE PRECISION CMIN, H0, H1                                     
      DOUBLE PRECISION TWOPI, PI, OMEGA                                 
      DOUBLE PRECISION MY(MAXMSH,MODEN)                                 
      DOUBLE PRECISION EPS, EPS10, X0, X1, X2                        
      DOUBLE PRECISION F0, F1, DH0(8), DSED(8), STIFF                   
      DOUBLE PRECISION EIGVAL, EIGREF, EIGMIN, EIGMAX, MINEIG           
      DOUBLE PRECISION F00, F11                                         
      DOUBLE PRECISION CON1, CON2, CON3, CON4, CON5, SEDK               
      DOUBLE PRECISION ADA( * )
                                                                        
      COMMON /CONST/ CON1, CON2, CON3, CON4, CON5, SEDK                 
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /G/ H0, H1                                                 
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF                        
      COMMON /LUIN/ LUIN, LUWRN, LUCHK
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /N/ MINMOD, MDUMMY, MODCUT, HBEAM, BPHVEL                 
      COMMON /NA/ ND0, ND1, CMIN
      COMMON /TRIGON/ TWOPI, PI, OMEGA                                  
                                                                        
      DATA MAXIT/ 20 /                                                  
      DATA EPS / 1.0E-11 /                                              
      DATA EPS10 / 1.0E-10 /                                            
                                                                        
  300 FORMAT(1X,/,' *** MAX ITERATIONS EXCEEDED FOR MODE ',             
     & I4,' ***',/,                                                     
     &  ' *** SEARCH WILL BE REPEATED WITH BRENT *** ',/)               
  320 FORMAT(1X,' *** WARNING: MAXIMUM ORDER MODE=',I4,' AT MESH',I4)   
  400 FORMAT(1X,' *** WARNING: MODE ',I4,' NOT FOUND.',                 
     & ' SEARCH WILL BE REPEATED WITH SUB BRENT. ')                     
                                                                        
                                                                        
C     NCROSS : MAXIMUM ORDER MODE FOR THE ACTUAL MESH                   
      MINEIG=EIGMIN*1.00001D0                                           
      CALL STURM(MINEIG,MH0I,MSEDI,DH0(I),DSED(I),NCROSS,ADA)           
C      NMMAX= NCROSS                                                     
                                                                        
C   CHECKING TRIAL EIGENVALUES BEFORE SEARCH                            
                                                                        
      MAXMOD=MINMOD+MODQTY-1                                            
      IF(NCROSS .LT. MAXMOD)   THEN                                     
       WRITE(LUPRT,320) NCROSS, I                                           
CWRN       WRITE(LUWRN,320) NCROSS, I                                           
       MAXMOD=NCROSS                                                    
       MODQTY=MAXMOD-MINMOD+1                                           
       IF(MODQTY .LE. 0)   RETURN                                       
      END IF                                                            
                                                                        
      IF(MODQTY .GE. 2)   THEN                                          
       DO 2000   M=2,MODQTY                                             
       IF(  MY(I,M-1)  .LE. MY(I,M) )   THEN                            
CWRN        WRITE(LUWRN,*)
CWRN     &    ' TRIAL EIGENVALUES NOT PROPERLY ORDERED ',          
CWRN     &    ' SEARCH WILL BE DONE WITH SUB BRENT '                          
        RETURN 2                                                        
       END IF                                                           
 2000  CONTINUE                                                         
      END IF                                                            
C                                                                       
                                                                        
      DO 2200   M=MODQTY,1,-1                                           
      IF(MY(I,M) .GE. MINEIG)   THEN                                    
       MODQTY=M                                                         
       MAXMOD=MINMOD+MODQTY-1                                           
       GO TO 2400                                                       
      END IF                                                            
CWRN        WRITE(LUWRN,*)
CWRN     &    ' TRIAL EIGENVALUE BEYOND CUTOFF FOR MODE ',         
CWRN     &    MINMOD+M-1                                         
 2200 CONTINUE                                                          
      MODQTY=0                                                          
      MAXMOD=0                                                          
      RETURN                                                            
 2400 CONTINUE                                                          
                                                                        
C     FIND THE EIGENVALUES                                              
                                                                        
      DO 4000 M=1,MODQTY                                                
      X0=MY(I,M)                                                        
      CALL INIT(X0,MH0I,MSEDI,DH0(I),DSED(I),F0,NOVFL0,MM,ADA)          
       IF(MM .EQ. MINMOD+M-1)   THEN                                    
        X1=X0+2.0D0*EPS10*X0                                            
       ELSE IF (MM .EQ. MINMOD+M-2)   THEN                              
        X1=X0-2.0D0*EPS10*X0                                            
       ELSE                                                             
CWRN        WRITE(LUWRN,*)
CWRN     &    ' WARNING FOR MODE ',MINMOD+M-1,MM,' AT MESH ',I     
CWRN        WRITE(LUWRN,*)
CWRN     &    ' POORLY ESTIMATED TRIAL EIGENVALUE. BRENT IS USED'  
        RETURN 2                                                        
       END IF                                                           
      NITER=0                                                           
 3000 CONTINUE                                                          
      CALL CHARAC(X1,MH0I,MSEDI,DH0(I),DSED(I),F1,NOVFL1,ADA)
      NODIF=NOVFL1-NOVFL0                                               
      F00=F0                                                            
      F11=F1                                                            
      IF (NODIF.NE.0) THEN                                              
       IF (NODIF.GT.0) THEN                                             
        F00=F0*(1E-20)**NODIF                                           
       ELSE                                                             
        F11=F1*(1E-20)**(-NODIF)                                        
       END IF                                                           
      END IF                                                            
      IF(F11-F00.EQ.0.D0) THEN                                          
       X2=(X0+X1)/2.D0                                                  
      ELSE                                                              
       X2=(X0*F11-X1*F00)/(F11-F00)                                     
      ENDIF                                                             
      NITER=NITER+1                                                     
      IF (NITER.GT.MAXIT) THEN                                          
CWRN       WRITE(LUWRN,300) MINMOD+M-1                                          
       RETURN 2                                                         
      END IF                                                            
      IF(ABS(X1-X2).GT.ABS(EPS*X2)) THEN                                
       X0=X1                                                            
       F0=F1                                                            
       NOVFL0=NOVFL1                                                    
       X1=X2                                                            
       GO TO 3000                                                       
      ENDIF                                                             
      IF( X2 .LT. MINEIG )   THEN                                       
CWRN        WRITE(LUWRN,400) MINMOD+M-1                                         
       RETURN 2                                                         
      END IF                                                            
      MY(I,M)=X2                                                        
 4000 CONTINUE                                                          
                                                                        
C   CHECKING OF EXTREME EIGENVALUES AND MONOTONICITY                    
                                                                        
      MAXMOD=MINMOD+MODQTY-1                                            
      IF( MY(I,1) .GT. EIGMAX )   THEN                                  
CWRN       WRITE(LUWRN,*)
CWRN     &   ' ***  WARNING : POOR ACCURACY ON FIRST MODE.',       
CWRN     &   ' SEARCH IS REPEATED WITH SUB BRENT '                            
       RETURN 2                                                         
      END IF                                                            
      EIGVAL=MY(I,1) + EPS10*MY(I,1)                                    
      CALL STURM(EIGVAL,MH0I,MSEDI,DH0(I),DSED(I),NCROSS,ADA)           
                                                                        
      IF( NCROSS+1 .NE. MINMOD )   THEN                                 
CWRN       WRITE(LUWRN,*) ' FIRST MODE NOT FOUND.',                             
CWRN     & ' SEARCH IS NOW REPEATED WITH SUB BRENT '                        
       RETURN 2                                                         
      END IF                                                            
      EIGVAL=MY(I,MODQTY) - EPS10*MY(I,MODQTY)                          
      CALL STURM(EIGVAL,MH0I,MSEDI,DH0(I),DSED(I),NCROSS,ADA)           
      IF( NCROSS .NE. MAXMOD )   THEN                                   
CWRN       WRITE(LUWRN,400) MINMOD+MODQTY-1                                     
       RETURN 2                                                         
      END IF                                                            
      IF(MODQTY.GT.2)   THEN                                            
       DO 6000   M=2,MODQTY-1                                           
       IF(  MY(I,M)-MY(I,M+1) .LT. 2.0D0*EPS )   THEN                   
CWRN        WRITE(LUWRN,*) ' WN NOT PROPERLY ORDERED ',                         
CWRN     &  ' SEARCH IS NOW REPEATED WITH SUB BRENT '                       
        RETURN 2                                                        
       END IF                                                           
 6000  CONTINUE                                                         
      END IF                                                            
      RETURN                                                            
      END                                                               
