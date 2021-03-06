	SUBROUTINE DECOMP(NDIM,N,COND,IPVT,WORK,A)
	
	IMPLICIT NONE

	INTEGER :: NDIM,N
	
	REAL(KIND=8) :: WORK(NDIM),A(NDIM,NDIM)
	REAL(KIND=8) :: ANORM,T,EK,YNORM,ZNORM,COND

	INTEGER :: IPVT(NDIM),NM1,J,J2,I,I2,I3,I4,I5,I6,I7,I8,K,K2,K3,KP1,M
	INTEGER :: KM1,KB
	
	IPVT(N)=1
	IF(N .EQ. 1) GO TO 80
	NM1=N-1
!
! COMPUTE 1-NORM OF MATRIX A
!
	ANORM=0.0D0
	DO J=1,N
		T=0.0D0
		DO I=1,N
			T=T+DABS(A(I,J))
5		END DO
		IF(T .GT. ANORM) ANORM=T
10 	END DO

!
! GAUSSIAN ELIMINATION WITH PIVOTING
!

	DO K=1,NM1
		KP1=K+1
!
! FIND PIVOT
!
		M=K
		DO I2=KP1,N
			IF(DABS(A(I2,K)) .GT. DABS(A(M,K))) M=I2
15 		END DO
		IPVT(K)=M
		IF( M .NE. K ) IPVT(N)=-IPVT(N)
		T=A(M,K)
		A(M,K)=A(K,K)
		A(K,K)=T
!
! SKIP STEP IF PIVOT IS ZERO
!
		IF(T .EQ. 0.0D0) GO TO 35
!
! COMPUTE MULTIPLIERS
!
		DO I3=KP1,N
			A(I3,K)=-A(I3,K)/T
20		END DO
!
! INTERCHANGE AND ELIMINATE BY COLUMNS
!
		DO J2=KP1,N
			T=A(M,J2)
			A(M,J2)=A(K,J2)
			A(K,J2)=T
			IF( T .EQ. 0.0D0 ) GO TO 30
			DO I4=KP1,N
				A(I4,J2)=A(I4,J2)+A(I4,K)*T
25			END DO
30		END DO
35	END DO
!
! CONDITION NUMBER IS CALCULATED BY SOLVING TWO SYSTEMS,
! (A-TRANSPOSE)*Y = E AND A*Z = Y. E IS A VECTOR OF +1 OR -1
! CHOSEN TO CAUSE GROWTH IN Y. 
! COND = (1-NORM OF Z)/(1-NORM OF Y)
!
! SOLVING (A-TRANSPOSE)*Y = E
!
	DO K2=1,N
		T=0.0D0
		IF(K2 .EQ. 1) GO TO 45
		KM1=K2-1
		DO I5=1,KM1
			T=T+A(I5,K2)*WORK(I5)
40		END DO
45		EK=1.0D0
		IF(T .LT. 0.0) EK=-1.0D0
		IF(A(K2,K2) .EQ. 0.0D0) GO TO 90
		WORK(K2)=-(EK+T)/A(K2,K2)
50	END DO
	DO KB=1,NM1
		K3=N-KB
		T=0.0D0
		KP1=K3+1
		DO I6=KP1,N
			T=T+A(I6,K3)*WORK(K3)
55		END DO
		WORK(K3)=T
		M=IPVT(K3)
		IF(M .EQ. K3) GO TO 60
		T=WORK(M)
		WORK(M)=WORK(K3)
		WORK(K3)=T
60	END DO
	YNORM=0.0D0
	DO I7=1,N
		YNORM=YNORM+DABS(WORK(I7))
65	END DO
!
! SOLVING A*Z = Y
!
	CALL SOLVE(NDIM,N,WORK,IPVT,A)
	ZNORM=0.0D0
	DO I8=1,N
		ZNORM=ZNORM+DABS(WORK(I8))
70	END DO
!
! ESTIMATE COND
!
	COND=ANORM*ZNORM/YNORM
	IF(COND .LT. 1.0D0 ) COND=1.0D0
	RETURN
!
! 1-BY-1 MATRIX
!
80	COND=1.0D0
	IF(A(1,1) .NE. 0.0D0) RETURN
!
! EXACT SINGULARITY
!
90	COND=1.0D+32
	RETURN
	END