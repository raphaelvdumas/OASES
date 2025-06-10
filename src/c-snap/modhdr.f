      SUBROUTINE MODHDR( FREQ, NMEDIA, NTOT, NMAT,
     &   MATER, DEPTH, RHO,                                          
     &   BCTOP, CST, RHOT, DEPTHT,                                           
     &   BCBOT, CSB, RHOB, DEPTHB,                                           
     &   M, Z, KTOP2, KBOT2 )                                  
                                                                        
C     Note T suffix means top                                           
C          B suffix means bottom                                        
                                                                        
      PARAMETER ( PI = 3.141592 )                                       
      
      REAL ETIME, CPSEC(2)
      REAL CPUBEG, CPUEND
      REAL CPEIGV, CPEIGF, CPNEWP, CPFILE
      
      CHARACTER MATER( * )*8,                
     &          BCTOP*1, BCBOT*1, MODEL*8                                
                                                                        
                                                                        
      REAL      DEPTH( * ), Z( * ), RHO( * )                            
                                                                        
      DOUBLE PRECISION H0, H1                                           
                                                                        
      COMPLEX   CPT, CST, CPB, CSB, KTOP2, KBOT2                        
                                                                        
      COMMON /G/ H0, H1                                                 
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /MODEL/ MODEL                                               
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /TIMING/ CPEIGV, CPEIGF, CPNEWP, CPFILE                    
                                                                        
      INCLUDE 'param.inc'                                               
      INCLUDE 'acommon.inc'                                             
                                                                        
                                                                        
                                                                        
  100 FORMAT(1X,' MAX SIZE EXCEEDED FOR MODAL INFO IN SUB MODHDR.',/,   
     & '  MAX ALLOWED NUMBER OF MODES            : ',I5,/,              
     & '  NUMBER OF EXISTING MODES AT THIS RANGE : ',I5,/,              
     & '  CALCULATION RESUMED WITH MAX ALLOWED NUMBER OF MODES.' )      
                                                                        
                                                                        
                                                                        
      CPUBEG= ETIME(CPSEC)                                     
                                                                        
C     *** Read header info ***                                          
                                                                        
      H0= AH0(CURRNT)                                                 
      H1= AH1(CURRNT)                                                 
                                                                        
      MODEL(1:7)= 'C-SNAP '                                             
      NMEDIA= ANMED(CURRNT)                                            
C     PRINT *,' SUB MODHDR CURRNT, ALTOT:',CURRNT, ALTOT(CURRNT)        
      NTOT= ALTOT(CURRNT)                                              
      NMAT= NTOT                                                           
C      MINMOD= AMINM(CURRNT)                                           
C     FREQ= ?????????????  FORSE NON SERVE                                 
C ************************************************************************
                                                                        
                                                                        
C      N( 1 )= AMH0(CURRNT)                                              
      MATER( 1 )= 'ACOUSTIC'                                              
      IF( NMEDIA .GT. 1 )   THEN                                          
C        N( 2 )= AMSED(CURRNT)                                           
        MATER( 2 )= 'ACOUSTIC'                                            
      END IF                                                              
C ************************************************************************
                                                                        
                                                                        
      BCTOP(1:1)= 'V'                                                   
      CPT= CMPLX(0.0, 0.0)                                              
      CST= CMPLX(0.0, 0.0)                                              
      RHOT= 0.0                                                         
      DEPTHT= 0.0                                                       
      BCBOT(1:1)= 'A'                                                   
      CPB= CMPLX( AC2(CURRNT), 0.0 )                                 
      CSB= CMPLX( AC2S(CURRNT), 0.0 )                               
      RHOB= AR2(CURRNT)                                               
      DEPTHB= AH0H1(CURRNT)                                         
C ************************************************************************
                                                                        
                                                                        
      DEPTH( 1 )= 0.0                                                     
      RHO( 1 )= 1.0                                                       
      IF( NMEDIA .GT. 1)   THEN                                           
        DEPTH( 2 )= AH0(CURRNT)                                       
        RHO( 2 )= AR1(CURRNT)                                         
      END IF                                                              
C ************************************************************************
                                                                        
                                                                        
      M= AMQTY(CURRNT)                                              
                                                                        
C ************************************************************************
                                                                        
                                                                        
      IF ( M .EQ. 0 ) RETURN                                            
                                                                        

      Z(1)= 0.0
      DO 1600   IZ= 1, AMH0(CURRNT) - 1
      Z(IZ+1)= ADH0(CURRNT)*IZ * AH0(CURRNT)
 1600 CONTINUE
      IF( AH1(CURRNT) .GT. 0.0 )   THEN
       ISTART= AMH0(CURRNT)
       DO 1800   IZ= 1, AMSED(CURRNT)
       Z(ISTART+IZ)= ADSED(CURRNT)*IZ*AH0(CURRNT) + AH0(CURRNT)
 1800  CONTINUE
      END IF

                                                                        
C ************************************************************************
                                                                        
      IF( M .GT. NMAM2 )   THEN                                         
        WRITE(LUPRT,100) NMAM2, M                                           
        AMQTY(CURRNT)= NMAM2                                        
        M= NMAM2                                                        
      END IF                                                            
                                                                        
      M=MIN(MODCUT,M)                                                    
                                                                        
      IF ( BCTOP(1:1) .EQ. 'A' ) KTOP2 = ( 2.0 * PI * FREQ / CPT ) **2  
      IF ( BCBOT(1:1) .EQ. 'A' ) KBOT2 = ( 2.0 * PI * FREQ / CPB ) **2  
                                                                        
      CPUEND= ETIME(CPSEC)
                                     
      CPFILE= CPFILE + CPUEND - CPUBEG                                  
      RETURN                                                            
      END                                                               
