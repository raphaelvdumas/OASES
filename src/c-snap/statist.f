                                                                        
C  STATIST.FOR                                                          
      
C      SUBROUTINE STATST( A, MODAVR, MR, SUM2 )
      SUBROUTINE STATST( A, MR, SUM2 )                         
                                                                        
      INTEGER OPTMZ                                                     
                                                                        
C      REAL MODAVR( * )                                                  
                                                                        
      COMPLEX A( * )                                                    
      COMMON /LUNIT/ LUPLP, LUPLT, LUPRT
      COMMON /N/ MINMOD, MAXMOD, MODCUT, HBEAM, BPHVEL                   
      COMMON /PARAM3/ IMESH, NMESH, MSHRAT, OPTMZ
                                                                        
C *****************************************************************     
                                                                        
                                                                        
      IF( OPTMZ .EQ. 0 )   THEN                                         
        MAXMOD= MODCUT                                                  
        RETURN                                                          
      END IF                                                            
                                                                        
C   Acceptable tolerance on total excitation                            
      EPS= 1.0E-6 * SUM2                                                
                                                                        
C      VMAX= 0.0                                                        
C   Search for MAX excitation                                           
C      DO 2020   IMODE= 1, MR                                           
C      VMAX=  MAX( VMAX, ABS(A(IMODE))*modavr(imode) )                  
C 2020 CONTINUE                                                         
                                                                        
                                                                        
C   Search for modes with little contribution to the whole excitation   
      SUM= 0.0                                                          
      DO 2040   IMODE= MR, 1, -1                                        
       SUM= SUM + ABS(A(IMODE))**2                                      
       IF( SUM .GT. EPS )   THEN                                        
         MAXMOD= MIN( MODCUT, imode + 4 )                               
C         WRITE(LUPRT,*) ' MR, CUT, NEW MAXMOD :',MR, MR-IMODE, MAXMOD       
         RETURN                                                         
      END IF                                                            
 2040 CONTINUE                                                          
                                                                        
C *****************************************************************     
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
