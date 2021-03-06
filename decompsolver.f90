	SUBROUTINE SOLVE(NDIM,N,B,IPVT,A)
	
	IMPLICIT NONE
	
	INTEGER :: NDIM,N,NM1,K,K2,KP1,M,I,I2,KB,KM1
	INTEGER :: IPVT(NDIM)

	REAL*8 :: B(NDIM),A(NDIM,NDIM),T
!
! FORWARD ELIMINATION
!
	IF( N .EQ. 1) GO TO 50
	NM1=N-1
	DO K=1,NM1
		KP1=K+1
		M=IPVT(K)
		T=B(M)
		B(M)=B(K)
		B(K)=T
		DO I=KP1,N
			B(I)=B(I)+A(I,K)*T
    END DO
    END DO
!
! BACK SUBSTITUTION
!
	DO KB=1,NM1
		KM1=N-KB
		K2=KM1+1
		B(K2)=B(K2)/A(K2,K2)
		T=-B(K2)
		DO I2=1,KM1
			B(I2)=B(I2)+A(I2,K2)*T
		END DO
	END DO
50	B(1)=B(1)/A(1,1)
	RETURN
	END