C     PARAM.INC

C  DEFINITION  OF :
C                    MAXMSH
C                    MAXNRD
C                    MAXNSD
C                    MODEN
C                    MSPMAX
C                    NPHALF
C                    NPOINT
C                    NRANGE
C                    SMPLS

C     SMPLS: Number of sample_points/mode 
      INTEGER SMPLS
      PARAMETER ( MODEN=5002, MSPMAX= 501, NRANGE= 2049 )
      PARAMETER ( MAXMSH= 8 )
      PARAMETER ( SMPLS= 8, NPOINT=  MAXMSH * SMPLS * MODEN,
     &            NPHALF= NPOINT/2 )
      PARAMETER ( MAXNRD= MSPMAX )

      PARAMETER ( MAXNSD= 1 )
