C   FILE.FOR                                                            
      SUBROUTINE FILE                                                   
C                                                                       
C   BASIC FILES TO BE USED THROUGHOUT THE EXECUTION                     
C   ARE ASSIGNED HERE.                                                  
C                                                                       
      CHARACTER*4 EOF
      CHARACTER*7 STATUS
                                                                        
      COMMON /LUCONT/ LUCDR, LUBDR, LUCFR, LUBFR
      COMMON /LUIN/ LUIN, LUWRN, LUCHK
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /LUPUL/ LUTRF
      COMMON /TLUNIT/ LUTLC, LUTLI                                      
                                                                        
      DATA EOF/'@EOF'/                                                  
                                                                        
C                                                                       
  120 FORMAT(A4)                                                        
  160 FORMAT(1H ,I8,10X,'  MODULO',A22)

C                                                                       
C   USE OF LOGIC UNIT NUMBER(S) :                                       
C   SUB ACCRCY       LUWRN                                                  
C   SUB CONDRC       LUCDR and LUBDR
C   SUB EIGVEC       LUWRN                                                  
C   SUB EVALCM       LUPRT, LUTLC and LUTLI
C   SUB FNDVEC       LUPRT, LUWRN
C   SUB GET_HDR      LUPRT
C   SUB INPUT        LUIN and 10
C   SUB MAIN         LUPRT, LUWRN, LUCHK, LUIN, LUTLC, LUTLI, LUTRF, and 10
C   SUB MODE         LUPRT
C   SUB MODES        LUPRT, LUPLT
C   SUB MODPRT       LUCHK
C   SUB NEWTON       LUPRT, LUWRN
C   SUB PLTFIL       LUPLP and LUPLT
C   SUB PORTER       LUPRT and LUWRN
C   SUB SORTP        LUTRF
C   SUB TLDEP_CP     LUPRT
                                                                        

C   PC VERSION  AND
C   OSF (unix) VERSION :
      STATUS= 'UNKNOWN'
C   VAX VERSION
C      STATUS='NEW    '



      LUPRT= 66
      OPEN(UNIT= LUPRT, FILE= 'c-snap.prt',
     & STATUS= STATUS,                               
     & FORM= 'FORMATTED')                                              

      OPEN(UNIT= 10,
     & STATUS= 'SCRATCH',                                                
     & FORM= 'UNFORMATTED')                                              
                                                                        
                                                                        
C   TRANSMISSION LOSS THROUGH "COHERENT" ADDITION OF MODES.
      LUTLC= 81                                                        
      OPEN(UNIT= LUTLC,                                                 
     & STATUS= 'SCRATCH',                                                
     & FORM= 'UNFORMATTED')                                              
                                                                        
C   TRANSMISSION LOSS THROUGH "INCOHERENT" ADDITION OF MODES.
      LUTLI= 82
      OPEN(UNIT= LUTLI,                                                 
     & STATUS= 'SCRATCH',                                                
     & FORM= 'UNFORMATTED')                                              
                                                                        
C                                                                       
C   THE INPUT RUN STREAM IS *** ALWAYS *** EXPECTED FROM FILE c-snap.dat
      LUIN= 20
      OPEN(UNIT= LUIN, FILE= 'c-snap.dat', STATUS='OLD')

                                                                       
C  FILES FOR TWO-DIMENSIONAL PLOTS
      LUPLP=9                                                           
      OPEN(UNIT= LUPLP, FILE= 'c-snap.plp',
     & STATUS= STATUS,                                            
     & FORM= 'FORMATTED')                                                
      IBUFF= 1024                                                       
      WRITE(LUPLP,160) IBUFF                                

      LUPLT=14                                                          
      OPEN(UNIT= LUPLT,FILE= 'c-snap.plt',
     & STATUS= STATUS,                                                   
     & FORM= 'FORMATTED')                                                
      WRITE(LUPLT,120) EOF                                               
      REWIND LUPLT
C ***************************************


C  FILE FOR CONTOUR PLOTS
      LUCDR= 26                                                          
      OPEN(UNIT= LUCDR,FILE= 'c-snap.cdr',
     & STATUS= STATUS,                                                   
     & FORM= 'FORMATTED')                                                

      LUBDR= 27                                                          
      OPEN(UNIT= LUBDR, FILE= 'c-snap.bdr',
     & STATUS= STATUS,                                                   
     & FORM= 'FORMATTED')

      LUCFR= 28                                                          
      OPEN(UNIT= LUCFR,FILE= 'c-snapfr.cdr',
     & STATUS= STATUS,                                                   
     & FORM= 'FORMATTED')                                                

      LUBFR= 29                                                          
      OPEN(UNIT= LUBFR, FILE= 'c-snapfr.bdr',
     & STATUS= STATUS,                                                   
     & FORM= 'FORMATTED')
C ***************************************


C  FILE FOR THE "PULSE" OPTION
      LUTRF= 83
      CLOSE(UNIT= LUTRF)
      OPEN(UNIT= LUTRF,FILE= 'c-snap.trf',
     & STATUS= STATUS,
     & FORM= 'UNFORMATTED')

C ****************************************

CWRN       LUWRN= 8
CWRN       OPEN(UNIT= LUWRN, FILE= 'c-snap.wrn',
CWRN     & STATUS= STATUS,                                                   
CWRN     & FORM= 'FORMATTED')

CCHK       LUCHK= 85                                                        
CCHK       OPEN(UNIT= LUCHK, FILE= 'c-snap.chk',
CCHK     & STATUS= STATUS,                                                   
CCHK     & FORM= 'FORMATTED')                                                
                                                                        

      RETURN
      END
