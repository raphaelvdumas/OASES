C RETRV.FOR                                                             
      SUBROUTINE RETRV(LUSORT, PRSS, NRCVRS, NR)                          
                                                                        
      REAL PRSS(NRCVRS,NR)                                              
                                                                        
      REWIND(LUSORT)
                                                                        
      DO 1200   J= 1, NR                                                
      READ(LUSORT) (PRSS(K,J), K= 1, NRCVRS)                              
 1200 CONTINUE                                                          
                                                                        
      RETURN                                                            
      END                                                               
