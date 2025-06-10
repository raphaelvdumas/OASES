      SUBROUTINE SORTP(NRD, NRNG, NFREQ, LUTRF, LUTRF1, 
     & WORK, MAXSIZ)

      COMPLEX WORK( MAXSIZ )
      COMPLEX fact

      COMMON /DUT/ fact


      fact= -cexp( cmplx(0.,acos(0.0)/2.0) )

      NGROUP= MAXSIZ/NFREQ
      IF( NGROUP .GE. NRNG)   THEN
        MODRNG= NRNG
        NGROUP= NGROUP/NRNG
        IF( NGROUP .GE. NRD)   THEN
          MODDEP= NRD
        ELSE
          MODDEP= MAX(1,NGROUP)
        END IF
      ELSE
        MODRNG= NGROUP
        MODDEP= 1
      END IF

      DO 1000 IRD= 1, NRD, MODDEP
        ID1= (IRD-1)*MODDEP + 1
        ID2= MIN(NRD, ID1+MODDEP-1)

      DO 1000 IRNG= 1, NRNG, MODRNG
        IR1= (IRNG-1)*MODRNG + 1
        IR2= MIN(NRNG, IR1+MODRNG-1)

        REWIND LUTRF1
        CALL READM(WORK, NFREQ, MODRNG, MODDEP,
     &           ID1, ID2,
     &           IR1, IR2,
     &           NRD, NRNG, LUTRF, LUTRF1 )

 1000 CONTINUE

      CLOSE(LUTRF1, STATUS= 'DELETE')
      RETURN
      END

C   READM.FOR
      SUBROUTINE READM(WORK, NFREQ, MODRNG, MODDEP,
     &                 ID1, ID2,
     &                 IR1, IR2,
     &                 NRD, NRNG, LUTRF, LUTRF1 )

      COMPLEX WORK(NFREQ, MODRNG, MODDEP)
      COMPLEX DUMMY, fact

      COMMON /DUT/ fact


      DO 3000   JF= 1, NFREQ
      DO 2000   JR1= 1, IR1-1
      READ(LUTRF1) (DUMMY, JD= 1, NRD)
 2000 CONTINUE
      DO 2200   JR= IR1, IR2
      READ(LUTRF1) (DUMMY, JD1= 1, ID1-1),
     &             (WORK(JF, JR-IR1+1, JD), JD= ID1, ID2),
     &             (DUMMY, JD2= ID2+1, NRD)
 2200 CONTINUE
      DO 2400   JR2= IR2+1, NRNG
      READ(LUTRF1) (DUMMY, JD= 1, NRD)
 2400 CONTINUE
 3000 CONTINUE

c >>> write trf file

      do jf=1,nfreq
       DO JR= IR1, IR2
        DO JD= ID1, ID2
         WRITE(LUTRF)  
     &   fact*WORK(JF, JR-IR1+1, JD-ID1+1)
        end do
       end do
      end do
      
      RETURN
      END
