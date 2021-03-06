SUBROUTINE FOILNACA(X,Y,N)

INTEGER :: N,K
REAL*8 :: M,P,T,DELTAX,XLOCATION,YC,DYCDX,THETA,YT,XL,YL,X(N),Y(N)

! GET NACA DIGITS

WRITE(*,*) 'ENTER MAXIMUM CAMBER (FIRST DIGIT)'
READ(*,*) M

WRITE(*,*) 'ENTER LOCATION OF MAX CAMBER (SECOND DIGIT)'
READ(*,*) P

WRITE(*,*) 'ENTER THICKNESS (LAST TWO DIGITS)'
READ(*,*) T

! CONVERT NACA DIGITS

M=M/100.0D0
P=P/10.0D0
T=T/100.0D0

DELTAX=2.0D0/N

! SET FIRST DATA POINT (ASSUMING UNIT CHORD)

X(1)=1.0D0
Y(1)=0.0D0

! CREATE LOWER SURFACE

DO K=1,N/2-1

	! FIND X LOCATION ALONG CHORD

	XLOCATION=1.0D0-DELTAX*K
	
	IF (XLOCATION<=P) THEN
	
		! COMPUTE PARAMETERS FOR LEADING PART OF AIRFOIL
	
		YC=M*XLOCATION/P/P*(2.0D0*P-XLOCATION)
		DYCDX=2.0D0*M/P/P*(P-XLOCATION)
		THETA=DATAN2(2.0D0*M*(P-XLOCATION),P*P)
		
	ELSE
	
		! COMPUTE PARAMETERS FOR TRAILING PART OF AIRFOIL
	
		YC=M*(1.0D0-XLOCATION)/(1.0D0-P)/(1.0D0-P)*(1.0D0+XLOCATION-2.0D0*P)
		DYCDX=2.0D0*M/(1.0D0-P)/(1.0D0-P)*(P-XLOCATION)
		THETA=DATAN2(2.0D0*M*(P-XLOCATION),(1.0D0-P)*(1.0D0-P))
		
	END IF
	
	! COMPUTE X AND Y COORDINATES
	
	YT=5.0D0*T*(0.296375D0*DSQRT(XLOCATION)-0.12635D0*XLOCATION-0.35195D0*XLOCATION*XLOCATION &
	+0.283775D0*XLOCATION*XLOCATION*XLOCATION-0.10185*XLOCATION**4)
	
	XL=XLOCATION+YT*DSIN(THETA)
	YL=YC-YT*DCOS(THETA)
	
	X(K+1)=XL
	Y(K+1)=YL
	
END DO

! CREATE UPPER SURFACE

DO K=N/2,N-1

	! FIND X LOCATION ALONG CHORD
	
	XLOCATION=DELTAX*(K-N/2)
	
	IF (XLOCATION<=P) THEN
	
		! COMPUTE PARAMETERS FOR LEADING PART OF AIRFOIL
			
		YC=M*XLOCATION/P/P*(2.0D0*P-XLOCATION)
		DYCDX=2.0D0*M/P/P*(P-XLOCATION)
		THETA=DATAN2(2.0D0*M*(P-XLOCATION),P*P)
		
	ELSE
	
		! COMPUTE PARAMETERS FOR TRAILING PART OF AIRFOIL
		
		YC=M*(1.0D0-XLOCATION)/(1.0D0-P)/(1.0D0-P)*(1.0D0+XLOCATION-2.0D0*P)
		DYCDX=2.0D0*M/(1.0D0-P)/(1.0D0-P)*(P-XLOCATION)
		THETA=DATAN2(2.0D0*M*(P-XLOCATION),(1.0D0-P)*(1.0D0-P))
		
	END IF
	
	! COMPUTE X AND Y COORDINATES
	
	YT=5.0D0*T*(0.296375D0*DSQRT(XLOCATION)-0.12635D0*XLOCATION-0.35195D0*XLOCATION*XLOCATION &
	+0.283775D0*XLOCATION*XLOCATION*XLOCATION-0.10185*XLOCATION**4)
	
	XL=XLOCATION-YT*DSIN(THETA)
	YL=YC+YT*DCOS(THETA)
	
	X(K+1)=XL
	Y(K+1)=YL
	
END DO

! OUTPUT RESULTS

WRITE(*,*) 'X ='
WRITE(*,*) X

WRITE(*,*) 'Y ='
WRITE(*,*) Y
	
RETURN
END