CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE USER
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This is the user part of the program. In this part, the users need
C  to specify all of the aspects related with the individual physical 
C  problems, including:
C  (1) the governing equations to be solved;
C  (2) the calculation domain, and the corresponding mesh generation;
C  (3) the physical properties of the materials;
C  (4) the boundary and/or initial conditions (if any);
C  (5) the source terms;
C  (6) the time interval if any, and the other parameters for the
C      iterations, i.e., the relaxation factors, the criteria for
C      ending the internal/external iterations.
***********************************************************************
      PARAMETER (NX=600,NY=600,NXY=600)
      CHARACTER (LEN=20) TITLE
      LOGICAL LSOLVE,LPRINT,LBLK,LSTOP,LEND
      COMMON F(NX,NY,13),FOLD(NX,NY,13),P(NX,NY),RHO(NX,NY),GAM(NX,NY),
     & CON(NX,NY),AIP(NX,NY),AIM(NX,NY),AJP(NX,NY),AJM(NX,NY),AP(NX,NY),
     & X(NXY),XU(NXY),XDIF(NXY),XCV(NXY),XCVS(NXY),
     & Y(NXY),YV(NXY),YDIF(NXY),YCV(NXY),YCVS(NXY),
     & YCVR(NXY),YCVRS(NXY),ARX(NXY),ARXJ(NXY),ARXJP(NXY),
     & R(NXY),RMN(NXY),SX(NXY),SXMN(NXY),XCVI(NXY),XCVIP(NXY)
      COMMON DU(NX,NY),DV(NX,NY),FV(NXY),FVP(NXY),
     & FX(NXY),FXM(NXY),FY(NXY),FYM(NXY),PT(NXY),QT(NXY)
      COMMON/INDX/NF,NFMAX,NP,NRHO,NGAM,L1,L2,L3,M1,M2,M3,
     &  IST,JST,ITER,LAST,TITLE(13),RELAX(13),TIME,DT,XL,YL,
     &  IPREF,JPREF,LSOLVE(10),LPRINT(13),LBLK(10),MODE,NTIMES(10),
     &  RHOCON
      COMMON/CNTL/LSTOP,LEND
      COMMON ITIME
      COMMON/SORC/SMAX,SSUM,RSMAX
      COMMON/COEF/FLOW,DIFF,ACOF
      DIMENSION U(NX,NY),V(NX,NY),PC(NX,NY)
      EQUIVALENCE (F(1,1,1),U(1,1)),(F(1,1,2),V(1,1)),(F(1,1,3),PC(1,1))
      DIMENSION TH(NXY),THU(NXY),THDIF(NXY),THCV(NXY),THCVS(NXY)
      EQUIVALENCE(X,TH),(XU,THU),(XDIF,THDIF),(XCV,THCV),
     &  (XCVS,THCVS),(XL,THL)
***********************************************************************
*                                                                     *
*            Example-2: Lid-Driven Cavity Flow                        *
*                                                                     *
***********************************************************************
      DIMENSION TX(15),TU(15),TV(15)
      ENTRY GRID
        XL=2.E-2
        YL=2.E-2
        L1=600
        M1=600
	  CALL UGRID
      RETURN
C----------------------------------------------------------------------
      ENTRY START
        MODE=1

        LAST=6000
        RSMAX=1E-7
 
      TSTART=0.
	  DT=1.E10
	  MTIME=1
	  ITIME=1
	  TIME=TSTART
	  LEND=.FALSE.

      DO I=1,3
      LSOLVE(I)=.TRUE.
      LPRINT(I)=.TRUE.
      END DO
	  RELAX(1)=0.5
	  RELAX(2)=0.5
	  TITLE(1)='.U EQ.'
	  TITLE(2)='.V EQ.'

      DO I=1,L1
      DO J=1,M1
	      U(I,J)=0.
	      V(I,J)=0.
      END DO
      END DO
      

      DO K=1,NFMAX
        DO I=1,L1
	      DO J=1,M1
	        FOLD(I,J,K)=F(I,J,K)
	      END DO
        END DO
      END DO
        TIME=TSTART+DT
      RETURN
C----------------------------------------------------------------------
      ENTRY DENSE
      DO J=1,M1
      DO I=1,L1
	      RHO(I,J)=1.225
      END DO
      END DO
      RETURN
C----------------------------------------------------------------------
      ENTRY BOUND
      OPEN(1,FILE='ARGON.DAT')
      DO I=1,15
      READ(1,*) TX(I),TU(I),TV(I)
C      WRITE(*,*) TX(I),TU(I),TV(I) 
      END DO
      CLOSE(1)
c-------inlet------
      
      DO I=1,L1
      U(I,1)=0.
      V(I,2)=0.
      U(I,2)=0.
      V(I,3)=0.
      ENDDO
      DO J=1,M1
      U(L1,J)=0.
      V(L1,J)=0.
      U(L2,J)=0.
      V(L2,J)=0.
      ENDDO
      
      DO I=1,L1/2
      DO J=M1/2,M1
      U(I,J)=0.
      V(I,J)=0.
      ENDDO
      ENDDO
      UMAX=3.0
      DO J=1,(M1/2)
      U(2,J)=UMAX
      V(1,J)=0
      ENDDO
c-------outlet----
      DO I=L1/2+1,L1
      U(I,M1)=PROP(XU(I),TX,TU)
      V(I,M1)=PROP(XU(I),TX,TV)
c      WRITE(*,*) "---------------------"
C      write(*,*) XU(I),U(I,M1),V(I,M1)
      ENDDO
      IF (.FALSE.)THEN
      OPEN(2,FILE='TESTUV.DAT')
      DO I=1,L1
      DO J=1,M1
      WRITE(2,*) I,J,U(I,J),V(I,J)
      ENDDO
      ENDDO
      CLOSE(2)
      OPEN(2,FILE='TESTxy.DAT')
      DO I=1,L1
      WRITE(2,*) X(I),Y(I)
      ENDDO
      CLOSE(2)
      ENDIF
      
      RETURN
C----------------------------------------------------------------------
      ENTRY OUTPUT
C----------------------------------------------------------------------
      IF (.FALSE.) THEN
      OPEN(2,FILE='TESTUV.DAT')
      DO I=1,L1
      DO J=1,M1
      WRITE(2,*) I,J,U(I,J),V(I,J)
      ENDDO
      ENDDO
      CLOSE(2)
      ENDIF
C----------------------------------------------------------------------
      IF(ITER.GE.2000) THEN
      FLOWIN=0.
      FLOWOUT=0.
      DO J=1,M1/2
      FLOWIN=FLOWIN+U(2,J)*ARX(J)
      END DO          
      DO I=L1/2+1,L1
          FLOWOUT=FLOWOUT+V(I,M1)*XCV(I)*R(M1)
      END DO
      FACTOR=FLOWOUT/FLOWIN
      DO I=L1/2+1,L1
      V(I,M1)=V(I,M1)/FACTOR
      END DO
      END IF
       
        
        IF(ITER.EQ.1) WRITE(8,400)
        IF(ITER.GE.1) THEN
          WRITE(8,403) ITER,X(L1/2),Y(M1/4),U(L1/2,M1/4),SMAX
          WRITE(*,403) ITER,X(L1/2),Y(M1/4),U(L1/2,M1/4),SMAX         
        END IF

  400   FORMAT(13X,'ITER',14X,'U(L1/2,M1/4)',12X,'SMAX')
  403   FORMAT(1X,I8,4F20.10)  
  
        IF(LSTOP.OR.(ITER.EQ.LAST)) CALL PRINT_RESULT        
      
        IF(LSTOP.OR.(ITER.EQ.LAST)) THEN
        FLOWIN=0.
        FLOWOUT=0.
        DO J=2,M1/2-1
            FLOWIN=FLOWIN+RHO(1,J)*U(2,J)*ARX(J)
                   
        END DO 
        DO I=L1/2+2,L2
        FLOWOUT=FLOWOUT+RHO(I,L1)*V(I,L1)*XCV(I)*R(M1)   
        ENDDO
      WRITE(*,*) 'FLOWIN=',FLOWIN
      WRITE(*,*) 'FLOWOUT=',FLOWOUT
          
      
      OPEN(2,FILE='TESTUV.DAT')
      DO I=1,L1
      DO J=1,M1
      WRITE(2,*) I,J,U(I,J),V(I,J)
      ENDDO
      ENDDO
      CLOSE(2)
      OPEN(2,FILE='TESTxy.DAT')
      DO I=1,L1
      WRITE(2,*) X(I),Y(I)
      ENDDO
      CLOSE(2)
        END IF          
      RETURN
C----------------------------------------------------------------------
      ENTRY GAMSOR
        DO I=1,L1
          DO J=1,M1
            GAM(I,J)=1.225/(1.789E-5)
          END DO
        END DO
        
      RETURN
C----------------------------------------------------------------------
      ENTRY TSTEP
        ITER=0
        LSTOP=.FALSE.
        IF (ITIME.GE.MTIME) THEN
        LEND=.TRUE.
        ENDIF
        TIME=TIME+DT
        ITIME=ITIME+1
      RETURN
C----------------------------------------------------------------------
      ENTRY STEPUP
        DO K=1,NFMAX
          DO J=1,M1
            DO I=1,L1
              FOLD(I,J,K)=F(I,J,K)
            END DO
          END DO
        END DO
      RETURN
C----------------------------------------------------------------------
      ENTRY ISSTOP
        IF(LSOLVE(1)) THEN
          IF (SMAX<RSMAX) THEN
          LSTOP=.TRUE.
          ENDIF
        END IF
      RETURN
C----------------------------------------------------------------------
C      ENTRY OUTPUT11
      
      END
      
      FUNCTION PROP(TEMP,TX,TABLE) 
      REAL TEMP,TEMP2
      DIMENSION TABLE(15),TX(15)
      INTEGER :: I

      DO I=1,14
      IF (TX(I).LE.TEMP.AND.TEMP.LT.TX(I+1))THEN
      EXIT
      ENDIF
      ENDDO

      TEMP2=(TEMP-TX(I))/(TX(I+1)-TX(I))*(TABLE(I+1)-TABLE(I))+TABLE(I)
      IF(TEMP .LE. TX(1) .OR. TEMP .GE. TX(15))THEN
      PROP=0.
      ELSE
      PROP=TEMP2
      ENDIF
      RETURN
      END