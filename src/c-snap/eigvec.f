C    EIGVEC.FOR                                                         
      SUBROUTINE EIGVEC( MEIG, MH0I, MSEDI, DH0I, DSEDI,                
     &  FRQ, ALFA,                                                      
     &  MINMOD, MODAVR,                                                 
     &  ADA, SPEED, EIGF,                                               
     &  A3, B3, C3, EE, ZZ, SSOLD, EXCH,                                
     &  EKM, EIGVAL, WNEXTP)
C__________________________________________________________             
C                                                         |             
C     This routine calculates the eigenvectors by         |             
C     means of inverse iteration. The eigenfunctions      |             
C     are normalized, and the attenuation coefficients    |             
C     are determined.                                     |             
C_________________________________________________________|             
C                                                                       
      INTEGER EXTPOL                                                    
      REAL ETIME, CPSEC(2)
      REAL CPUBEG, CPEIGV, CPEIGF, CPNEWP, CPFILE

      LOGICAL EXCH( * )                                                 
                                                                        
      REAL MODAVR( * )                                                  
      REAL K0, K1, K2, KH0                                              
                                                                        
      DOUBLE PRECISION CC0, CC1, H0, H1                                 
      DOUBLE PRECISION EKM, SQKM, EIGVAL, WNEXTP
      DOUBLE PRECISION ZZMAX, ZZINV, ABS1, ABS2                                
      DOUBLE PRECISION NORMT                                            
      DOUBLE PRECISION ATTW, ATTS, ATTB                                 
      DOUBLE PRECISION ADA( * )                                         
C                                                                       
C     LOCAL ARRAYS USED FOR INVERSE ITERATION                           
C                                                                       
      DOUBLE PRECISION Q, DD, DIFF                                      
      DOUBLE PRECISION A3( * ), B3( * ), C3( * ),                       
     &                 EE( * ), ZZ( * )                                 
      DOUBLE PRECISION SSOLD( * ), EIGF( * )                            
C                                                                       
      DOUBLE PRECISION SPEED( * )                                       
      DOUBLE PRECISION ROB, ROS                                         
      DOUBLE PRECISION KM, CIN, COST, DH0I, DSEDI                       
      DOUBLE PRECISION FRQ, TWOPI, PI, OMEGA                            
      DOUBLE PRECISION EIGREF, EIGMIN, EIGMAX, STIFF                    
      DOUBLE PRECISION CON1, CON2, CON3, CON4, CON5, SEDK               
                                                                        
      COMMON /ATTEN/ ALF0, ALF1, ALF2, ALF2S, ALFOS, ALFOB              
      COMMON /AB/ BETA(-1:3), SCATT(2), C2S, CC0, CC1, C2               
      COMMON /CONST/ CON1, CON2, CON3, CON4, CON5, SEDK                 
      COMMON /DENS/ R0, R1, R2                                          
      COMMON /DENS8/ ROB, ROS                                           
      COMMON /FLAGG/ PLANE, NOVOL, NOLOSS, NOCYL, LARGE, SUMPL
      COMMON /FLAGPL/ FIRST, FLAGP, FLAGPU, EXTPOL, CORREC
      COMMON /FRQDEP/ FRQREF, FPOW, FRQDEP
      COMMON /G/ H0, H1                                                 
      COMMON /GEN/ EIGREF, EIGMIN, EIGMAX, STIFF                        
      COMMON /LUIN/ LUIN, LUWRN, LUCHK
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /NORM/ N12221, N14241                                      
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE                    
      COMMON /TRIGON/ TWOPI, PI, OMEGA                                  
      COMMON /XREFL/ CINTFC, RINTFC                                     
                                                                        
  200 FORMAT(1X,'MODE=',I4, 1X,D17.10,1X,E10.4,6(1X,E9.3),              
     & 2X,2(F5.2,1X),F5.2)                                              
  300 FORMAT(1X,' *** WARNING FOR MODE NO.',I4,' :',/,                  
     & '      INVERSE ITERATION RESTARTED WITH OFFSET EIGENVALUE.')     

                                                                        
      CPUBEG= ETIME(CPSEC)                                       
                                                                        
      EKM= SQRT(EIGVAL)/(DH0I*H0)                                       
      KM= SQRT(EIGVAL)/DH0I                                             
      SQKM= KM*KM                                                       
      NTOT= MH0I+MSEDI                                                  
      MN= MEIG+MINMOD-1                                                 
                                                                        
C     SET UP OF COEFFICIENT MATRIX                                      
                                                                        
C     WATER LAYERS                                                      
                                                                        
 1000 CONTINUE                                                          
                                                                        
      A3(1)= ADA(1)-EIGVAL                                              
      B3(1)= 1.0D0
      DO 1200   N= 2, MH0I-1                                            
      A3(N)= ADA(N)-EIGVAL                                              
      B3(N)= 1.0D0                                                      
      C3(N-1)= 1.0D0
 1200 CONTINUE                                                          
                                                                        
C     SEDIMENT LAYERS                                                   
                                                                        
      IF (MSEDI .GT. 0) THEN                                            
       A3(MH0I)= ADA(MH0I)-EIGVAL + (DH0I/(DSEDI*ROS))*                 
     &  ( (((EIGREF*DSEDI)/SPEED(MH0I+2))**2-2.0D0) - EIGVAL*CON3 )    
       B3(MH0I)= 2.0D0*(DH0I/DSEDI)                                     
       C3(MH0I-1)= 2.0D0
       A3(MH0I+1)= ADA(MH0I+1)-EIGVAL*CON3                              
       B3(MH0I+1)= 1.0D0
       C3(MH0I)= 1.0D0/ROS                                              
       DO 1400   N= MH0I+2, NTOT-1                                      
       A3(N)= ADA(N)-EIGVAL*CON3                                        
       B3(N)= 1.0D0
       C3(N-1)= 1.0D0
 1400  CONTINUE                                                         
      END IF                                                            
      IF (EIGMIN .GT. EIGVAL) THEN                                      
       A3(NTOT)= ADA(NTOT)-EIGVAL*CON3                                  
      ELSE                                                              
       A3(NTOT)= ADA(NTOT)-EIGVAL*CON3-2.0D0*SQRT(EIGVAL-EIGMIN)*CON5   
      END IF                                                            
                                                                        
      C3(NTOT-1)= 2.0D0
      B3(NTOT)= 0.0D0                                                   
      C3(NTOT)= 0.0D0                                                   
                                                                        
C     ELIMINATION IN COEFFICIENT MATRIX                                 
                                                                        
      DO 1600   I=1,NTOT-1
        I1=I+1
        IF (ABS(C3(I)).GT.ABS(A3(I))) THEN
          EXCH(I)=.TRUE.
          Q= C3(I)
          C3(I)= A3(I)
          A3(I)= Q
          Q= A3(I1)
          A3(I1)= B3(I)
          B3(I)= Q
          DD= B3(I1)
          A3(I)= 1.0D0/A3(I)
          Q= -C3(I) * A3(I)
          EE(I)= Q
          A3(I1)= A3(I1) + Q*B3(I)
          B3(I1)= Q*DD
          C3(I)= DD
        ELSE
          EXCH(I)= .FALSE.
          A3(I)= 1.0D0/A3(I)
          Q= -C3(I)*A3(I)
          EE(I)= Q
          A3(I1)= A3(I1)+Q*B3(I)
          C3(I)= 0.0D0
         END IF
 1600 CONTINUE
      IF(A3(NTOT) .EQ. 0.0D0)   THEN                                    
CWRN       WRITE(LUWRN,300) MEIG                                                
       EIGVAL= EIGVAL*(1.0D0+1.0D-12)                                   
       GO TO 1000                                                       
      END IF                                                            
      A3(NTOT)= 1.0D0/A3(NTOT)                                          
                                                                        
C     ELIMINATION OF RIGHT HAND SIDE                                    
                                                                        
      DO 2000   I= 1, NTOT                                              
      ZZ(I)= 1.0D0
      SSOLD(I)= 1.0D0
 2000 CONTINUE                                                          
      NIT= 0                                                            
 2200 IF (NIT .GT. 0) THEN                                              
       DO 2400   I= 1, NTOT-1                                           
       IF (EXCH(I)) THEN                                                
        Q= ZZ(I+1)                                                      
        ZZ(I+1)= ZZ(I)                                                  
        ZZ(I)= Q                                                        
       END IF                                                           
       ZZ(I+1)= ZZ(I+1)+EE(I)*ZZ(I)                                     
 2400  CONTINUE                                                         
      END IF                                                            
                                                                        
C     BACK SUBSTITUTION                                                 
                                                                        
      ZZMAX= 0.0D0                                                      
      EIGF(NTOT)= ZZ(NTOT)*A3(NTOT)                                     
      EIGF(NTOT-1)= (ZZ(NTOT-1)-B3(NTOT-1)*EIGF(NTOT))*A3(NTOT-1)       
      ABS1= ABS(EIGF(NTOT))                                             
      ABS2= ABS(EIGF(NTOT-1))                                           
      ZZMAX= MAX(ABS1,ABS2)                                             
C     ZZMAX= AMAX1(ABS(EIGF(NTOT)),ABS(EIGF(NTOT-1)))                   
      DO 3000   I= NTOT-2, 1, -1                                        
      EIGF(I)= (ZZ(I)-B3(I)*EIGF(I+1)-C3(I)*EIGF(I+2))*A3(I)            
      ABS2= ABS(EIGF(I))                                                
      ZZMAX= MAX(ZZMAX,ABS2)                                            
C     ZZMAX= AMAX1(ZZMAX,ABS(EIGF(I)))                                  
 3000 CONTINUE                                                          
                                                                        
C     SCALE AND CHECK ACCURACY                                          
                                                                        
C     ZZMAX= SIGN(ZZMAX,EIGF(NTOT))                                     
      NIT= NIT+1                                                        
      Q= 0.0D0                                                          
      ZZINV= 1.0D0/ZZMAX
      DO 4000   I= 1, NTOT                                              
      EIGF(I)= EIGF(I)*ZZINV
      DIFF= ABS(ABS(EIGF(I))-ABS(SSOLD(I)))                             
      Q= MAX(Q,DIFF)                                                    
 4000 CONTINUE                                                          
      IF (Q .LT. 1.0D-10) GO TO 4400                                    
      IF (NIT .GT. 10) THEN                                             
CWRN       WRITE(LUWRN,*)
CWRN     &   '*** MAX NUMBER OF ITERATIONS REACHED IN EIGVEC ***' 
       WRITE(LUPRT,*) '*** MAX DIFFERENCE IN MODE AMPL: ',Q                              
       GO TO 4400                                                       
      END IF                                                            
      DO 4200   I= 1, NTOT                                              
      ZZ(I)= EIGF(I)                                                    
      SSOLD(I)= EIGF(I)                                                 
 4200 CONTINUE                                                          
      GO TO 2200                                                        
 4400 CONTINUE                                                          
                                                                        
                                                                        
C     NORMALIZATION                                                     
                                                                        
      IF( N12221 .EQ. 1)   THEN                                         
        CALL NRM122(MEIG,NTOT,MH0I,MSEDI,SPEED,DH0I,DSEDI,            
     &  MINMOD,EIGF,MODAVR,EKM,EIGVAL,                              
     &  ATTW, ATTS, ATTB, NORMT)                                        
      ELSE                                                              
        CALL NRM142(MEIG,NTOT,MH0I,MSEDI,SPEED,DH0I,DSEDI,            
     &  MINMOD,EIGF,MODAVR,EKM,EIGVAL,                              
     &  ATTW, ATTS, ATTB, NORMT)                                        
      END IF                                                            
                                                                        
                                                                        
C     MODE ATTENUATION                                                  
                                                                        
      ATTW= ATTW/NORMT*EIGREF/KM                                        
      ATTS= ATTS/NORMT*EIGREF/KM                                        
      ATTB= ATTB/NORMT*EIGREF/KM                                        
                                                                        
      IF(NOVOL .EQ. 0)   THEN                                           
       ALF0= BETA(0) * FRQ * ATTW / (CC0 * 8.68589)                     
      ELSE                                                              
       ALF0= 0.0                                                        
      END IF                                                            
      IF(MSEDI .GT. 0)   THEN                                           
       ALF1= FRQDEP *  BETA(1) * FRQ * ATTS / (CC1 * 8.68589)
      ELSE                                                              
       ALF1= 0.0                                                        
      END IF                                                            
      ALF2= FRQDEP * BETA(2) * FRQ * ATTB / (C2 * 8.68589)

C     BOTTOM LOSSES                                                     
                                                                        
      IF(C2S .GT. 0.) THEN                                              
                                                                        
C     REFLECTION COEFFICIENT                                            
                                                                        
       IF (MSEDI .GT. 0) THEN                                           
        CIN= SPEED(NTOT+2)                                              
       ELSE                                                             
        CIN= SPEED(MH0I+1)                                              
       END IF                                                           
       COST= KM/EIGREF*CIN                                              
       CALL REFL2(CINTFC,RINTFC,R2,COST,Q)                              
                                                                        
C     THE FOLLOWING IS TAKEN FROM SNAP, BUT I DOUBT                     
C     IT'S CORRECTNESS FOR (K/CINTFC)<KM, I.E. THE                      
C     CASE WHERE THE MODE IS EVANESCENT AT THE                          
C     SUBBOTTOM INTERFACE                                               
C     H. SCHMIDT, 841002                                                
                                                                        
       K1= ABS((EIGREF/CIN)**2-SQKM)                                    
       SQRK1= SQRT(K1)                                                  
       K2= SQKM-((OMEGA*H0)/DBLE(C2))**2                                
       ALF2S= RINTFC*EIGF(NTOT)**2*SQRK1/(8.0*KM)                       
       ALF2S= ALF2S*(1.0+(RINTFC/R2)**2*K2/K1)*Q                        
       ALFA= ALF0+ALF1+ALF2S                                            
      ELSE                                                              
       ALF2S= 0.0                                                       
       ALFA= ALF0+ALF1+ALF2                                             
      END IF                                                            
                                                                        
C     SCATTER LOSSES                                                    
                                                                        
      DER0= EIGF(1)/(H0*DH0I)                                           
      IF (MSEDI .GT. 0) THEN                                            
       DER1= (CON1*EIGF(MH0I)-(CON1+CON4)*EIGF(MH0I-1)+                 
     & CON4*ROS*EIGF(MH0I+1))/(2.0D0*DH0I*H0)                             
      ELSE                                                              
       K1= SQKM-((OMEGA*H0)/DBLE(C2))**2                                
       SQK1= SQRT(K1)                                                   
       DER1= -EIGF(MH0I)*SQK1/(ROB*H0)                                  
      END IF                                                            
                                                                        
      DER1= SIGN(  MAX(ABS(DER1), 1.0E-19), DER1 )
                                                                        
      RATS= (EIGREF/SPEED(1))**2                                        
      RATB= (EIGREF/SPEED(MH0I+1))**2                                   
      IF (SQKM .LT. RATS) THEN                                          
       K0= SQRT(RATS-SQKM)                                              
      ELSE                                                              
       K0= 0.0                                                          
      END IF                                                            
      IF (SQKM .LT. RATB) THEN                                          
       KH0= SQRT(RATB-SQKM)                                             
      ELSE                                                              
       KH0= 0.0                                                         
      END IF                                                            
      ALFOS= SCATT(1)**2*K0*DER0**2/(2.0*KM)                             
      ALFOB= SCATT(2)**2*KH0/(2.0*KM)*(DER1**2+(EIGF(MH0I)*KH0/H0)**2)   
      ALFA= ALFA+ALFOB+ALFOS                                            
C                                                                       
      CHECKS= 2.0*((RATS-SQKM)/(H0*H0))*SCATT(1)**2                     
      CHECKB= 2.0*((RATB-SQKM)/(H0*H0))*SCATT(2)**2                     
C                                                                       
      IF(FLAGPU .LT. 1.0)   WRITE(LUPRT,200) MN,WNEXTP,ALFA,
     & ALF0,ALF1,ALF2,ALF2S,ALFOS,ALFOB,Q,CHECKS,CHECKB                      
                                                                        
                                                                        
      DO 5000   IZ=  MH0I + 1, NTOT                                     
      EIGF(IZ)=  EIGF(IZ )  * R1                                        
 5000 CONTINUE                                                          
                              
      CPEIGF= CPEIGF + (ETIME(CPSEC) - CPUBEG)                                
                                                                        
      RETURN                                                            
      END                                                               
