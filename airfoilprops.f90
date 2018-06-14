SUBROUTINE FOILPROPS(VINF,PI,ONEO2PI,N,X,Y,ALPHA,ALPHACOUNT,NALPHA,L,SINTH,COSTH, &
BVEC,A,SHARP,NCON,XBAR,YBAR)

INTEGER :: N,SHARP,I,J,K,B,E,ALPHACOUNT,NALPHA
INTEGER :: NCON(N,2)
REAL*8 :: VINF,CHORD,PI,ONEO2PI,ALPHA(NALPHA),DX,DY,DX1,DY1,DX2,DY2,RIJ, &
RIJPI,RKJ,RKJP1,BETAIJ,BETAKJ,SINTHIJ,COSTHIJ,SINTHKJ,COSTHKJ
REAL*8 :: X(N),Y(N),L(N),SINTH(N),COSTH(N),BVEC(N+SHARP),A(N+SHARP,N+SHARP),XBAR(N),YBAR(N)

! CONVERT ALPHA TO RADIANS

ALPHA(ALPHACOUNT)=ALPHA(ALPHACOUNT)*PI/180.0D0

! BUILD CONNECTIVITY MATRIX

DO I=1,N-1
	NCON(I,1)=I
	NCON(I,2)=I+1
END DO
NCON(N,1)=N
NCON(N,2)=1

! COMPUTE SOME MEMBER PROPERTIES

IF (SHARP==1) THEN

	! INITIALIZE ADDITIONAL ELEMENTS OF MATRIX A

	DO I=1,N+1
		A(I,N+1)=0.0D0
	END DO
	
	DO J=1,N
		A(N+1,J)=0.0D0
	END DO
	
END IF

DO I=1,N

	B=NCON(I,1)
	E=NCON(I,2)
	
	DX=X(E)-X(B)
	DY=Y(E)-Y(B)

	XBAR(I)=0.5D0*(X(B)+X(E))
	YBAR(I)=0.5D0*(Y(B)+Y(E))
	
	L(I)=DSQRT(DX*DX+DY*DY)
	
	SINTH(I)=DY/L(I)
	COSTH(I)=DX/L(I)
	
	! ASSEMBLE VECTOR B
	
	BVEC(I)=VINF*(SINTH(I)*DCOS(ALPHA(ALPHACOUNT))-COSTH(I)*DSIN(ALPHA(ALPHACOUNT)))
	
END DO

! COMPUTE MORE PROPERTIES

DO I=1,N
	DO J=1,N
	
		B=NCON(J,1)
		E=NCON(J,2)
		
		DX1=XBAR(I)-X(B)
		DY1=YBAR(I)-Y(B)
		DX2=XBAR(I)-X(E)
		DY2=YBAR(I)-Y(E)
		
		RIJ=DSQRT(DX1*DX1+DY1*DY1)
		RIJP1=DSQRT(DX2*DX2+DY2*DY2)
		
		BETAIJ=DATAN2(DY2*DX1-DX2*DY1,DY1*DY2+DX1*DX2)
		
		IF (I==J) THEN
			BETAIJ=PI
		END IF
		
		! UTILIZE TRIG IDENTITIES
		
		SINTHIJ=SINTH(I)*COSTH(J)-COSTH(I)*SINTH(J)
		COSTHIJ=COSTH(I)*COSTH(J)+SINTH(I)*SINTH(J)
		
		! BUILD MATRIX A
		
		A(I,J)=ONEO2PI*(SINTHIJ*DLOG(RIJP1/RIJ)+BETAIJ*COSTHIJ)
		
		IF (SHARP==1) THEN
		
			A(I,N+1)=A(I,N+1)-ONEO2PI*(BETAIJ*SINTHIJ-COSTHIJ*DLOG(RIJP1/RIJ))
			
		END IF
		
	END DO
END DO

! ADDITIONAL CONDITIONS FOR SHARP AIRFOIL

IF (SHARP==1) THEN
	
	! COMPUTE ADDITIONAL ELEMENT OF VECTOR B

	BVEC(N+1)=-VINF*(COSTH(1)*DCOS(ALPHA(ALPHACOUNT))+SINTH(1)*DSIN(ALPHA(ALPHACOUNT)) &
	+COSTH(N)*DCOS(ALPHA(ALPHACOUNT))+SINTH(N)*DSIN(ALPHA(ALPHACOUNT)))
	
	DO K=1,N,N-1
		DO J=1,N
		
			B=NCON(J,1)
			E=NCON(J,2)
			
			DX1=XBAR(K)-X(B)
			DY1=YBAR(K)-Y(B)
			DX2=XBAR(K)-X(E)
			DY2=YBAR(K)-Y(E)
			
			RKJ=DSQRT(DX1*DX1+DY1*DY1)
			RKJP1=DSQRT(DX2*DX2+DY2*DY2)
			
			BETAKJ=DATAN2(DY2*DX1-DX2*DY1,DY1*DY2+DX1*DX2)
		
			IF (K==J) THEN
				BETAKJ=PI
			END IF
		
			SINTHKJ=SINTH(K)*COSTH(J)-COSTH(K)*SINTH(J)
			COSTHKJ=COSTH(K)*COSTH(J)+SINTH(K)*SINTH(J)
			
			! COMPUTE ADDITIONAL ELEMENTS OF MATRIX A
			
			A(N+1,J)=A(N+1,J)+ONEO2PI*(BETAKJ*SINTHKJ-COSTHKJ*DLOG(RKJP1/RKJ))
			
			! COMPUTE ADDITIONAL DIAGONAL ELEMENT
			
			A(N+1,N+1)=A(N+1,N+1)+ONEO2PI*(BETAKJ*COSTHKJ+SINTHKJ*DLOG(RKJP1/RKJ))
			
		END DO
	END DO
	
END IF

! OUTPUT RESULTS

WRITE(*,*) 'ALPHA',ALPHACOUNT

WRITE(*,*) 'A ='
WRITE(*,*) A

WRITE(*,*) 'B ='
WRITE(*,*) BVEC

RETURN
END