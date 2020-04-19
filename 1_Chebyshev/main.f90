! ****************************************************
!   FRANK MANU
!   SPRING 2020
!	REF: DR. THOMPSON
!   EECE.5200 - COMPUTER AIDED ENGINEERING ANAYLSIS
!   PROBLEM SET 6 - PART 1
! ****************************************************

	PARAMETER (N=8)
	REAL THETA(0:N),X(0:N),Z(0:N)
	REAL T(0:N,0:N),COEF(0:N)

	PI = 4.0*ATAN(1.0)
	DO I=0,N
		THETA(I) = I*PI/N
		X(I) = COS(THETA(I))
		Z(I) = (X(I) +1)/2.0
	ENDDO

	DO I=0,N
		COEF(I) = EXP(Z(I))
		DO J=0,N
			T(I,J) = COS(J*THETA(I))
		ENDDO
	ENDDO

	CALL GESS (N+1,T,N+1,COEF,N+1,1,COND)
	WRITE(*,*) 'COND =',COND
	WRITE(*,*)' '
	WRITE(*,*) 'INDEX              COEF'
	
	DO I=0,N
		WRITE(*,*)I,COEF(I)
	ENDDO

	WRITE(*,*)' '
	WRITE(*,*) '   Z               EXP               APPROX          DAPPROX          ERROR'
	DO I=0,10
		ZTMP = I* 1/10.0
		CALL CHEBY(ZTMP,COEF,N,SUM,DSUM)
 		WRITE(*,*) ZTMP, EXP(ZTMP),SUM,DSUM, EXP(ZTMP)-SUM
	ENDDO
	END




