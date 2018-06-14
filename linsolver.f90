! *****************************************************
!
! TEST PROGRAM FOR DECOMP AND SOLVE ROUTINES
! CREATED 1/30/03
!
! *****************************************************

	PROGRAM LINEAR_SOLVER

	REAL(KIND=8), ALLOCATABLE :: A(:,:),WORK(:),B(:)
	REAL(KIND=8) :: COND
	INTEGER :: NDIM,N,I,J,K,L
	INTEGER, ALLOCATABLE :: IPVT(:)
!
! DIMENSION OF A,WORK,B,IPVT ARE ALL ALLOCATABLE
!
	WRITE(*,*) 'What is the dimension of the matrix?'
	READ(*,*) NDIM
	
	ALLOCATE(A(NDIM,NDIM),WORK(NDIM),B(NDIM),IPVT(NDIM))
!
! N IS THE NUMBER OF EQUATIONS
!
	N=NDIM
!
! ENTERING ELEMENTS OF MATRIX A
!
	open(unit=1,file='lineardata.txt');
	
	DO J=1,NDIM
		DO K=1,NDIM
			READ(1,*) A(J,K)
		END DO
	END DO
! 
! ENTERING ELEMENTS OF VECTOR B
!
	DO L=1,NDIM
		READ(1,*) B(L)
	END DO
	
	CALL DECOMP(NDIM,N,COND,IPVT,WORK,A)
	CALL SOLVE (NDIM,N,B,IPVT,A)
!
! WRITING CONDITION NUMBER
!

	WRITE(*,*) "Condition Number = ",COND
	
!
! WRITING SOLUTION STORED IN B
!	
	DO I=1,N
		WRITE(*,*) 'x',I,' = ',B(I)
	END DO


	END PROGRAM LINEAR_SOLVER