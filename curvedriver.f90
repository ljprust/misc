PROGRAM CURVEDRIVE

INTEGER :: NPOINTS,NBASIS,I,J
INTEGER, ALLOCATABLE :: IPVT(:)
REAL*8 :: COND
REAL*8, ALLOCATABLE :: X(:),Y(:),Z(:,:),F(:),A(:,:),B(:),WORK(:)

! OPEN DATA FILE

OPEN(UNIT=1,FILE='curvedata3.txt')

! READ IN DATA

READ(1,*) NPOINTS
READ(1,*) NBASIS

! ALLOCATE MATRICES

ALLOCATE(X(NPOINTS),Y(NPOINTS),Z(NBASIS,NPOINTS),F(NBASIS),A(NBASIS,NBASIS),B(NBASIS),WORK(NBASIS),IPVT(NBASIS))

! READ IN DATA POINTS

DO I=1,NPOINTS
	READ(1,*) X(I)
	READ(1,*) Y(I)
END DO

! CALL A BUNCH OF SUBROUTINES

CALL CURVEZMAT(X,NBASIS,NPOINTS,Z)

CALL CURVEMULT(NPOINTS,NBASIS,Y,Z,A,B)

CALL DECOMP(NBASIS,NBASIS,COND,IPVT,WORK,A)
CALL SOLVE(NBASIS,NBASIS,B,IPVT,A)

! OUTPUT THE COEFFICIENTS

DO I=1,NBASIS
	WRITE(*,*) 'Basis Function',I
	WRITE(*,*) B(I)
END DO

END PROGRAM