! ****************************************************
!   FRANK MANU
!   SPRING 2020
!	REF: DR. THOMPSON
!   EECE.5200 - COMPUTER AIDED ENGINEERING ANAYLSIS
!   PROBLEM SET 6 - PART 1
! ****************************************************

    SUBROUTINE CHEBY(Z,COEF,N,SUM,DSUM)
	REAL COEF(0:0)
	REAL,ALLOCATABLE ::T(:),DT(:)

	ALLOCATE ( T(0:N),DT(0:N) )

	X = 2*Z-1
	DXDZ=2

	T(0) =1.
	T(1) =X

	DO J=2,N
		T(J) =2.0*X*T(J-1)-T(J-2)
	ENDDO

	DT(0)=0
	DT(1)=1
	DT(2)=4*X
	DO I=3,N
		DT(I)=2*I*T(I-1)+DT(I-2)*I/(I-2)
	ENDDO

	SUM=0.0
	DSUM=0.0
	DO J=0,N
		SUM=SUM+COEF(J)*T(J)
		DSUM=DSUM+COEF(J)*DT(J)*DXDZ
	ENDDO	

	DEALLOCATE(T,DT)	
	RETURN
	END