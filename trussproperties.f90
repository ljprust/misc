SUBROUTINE trussprops(NPINS,NMEM,NCON,X,Y,AREA,YOUNG,STIFF)

INTEGER :: I,J,NMEM,NPINS,NCON(NMEM,2),B,E,TBM1,TB,TEM1,TE
REAL*8 :: X(NPINS),Y(NPINS),AREA(NMEM),YOUNG(NMEM),DX,DY,L,ANGCOS,ANGSIN,SPRING,KCS,KC2,KS2
REAL*8 :: STIFF(2*NPINS,2*NPINS)
!
! INITIALIZE STIFFNESS MATRIX
!
DO I=1,2*NPINS
	DO J=1,2*NPINS
		STIFF(I,J)=0.0
	END DO
END DO
!
! CALCULATE MEMBER PROPERTIES
!
DO I=1,NMEM
	DX=X(NCON(I,2))-X(NCON(I,1))
	DY=Y(NCON(I,2))-Y(NCON(I,1))
	L=SQRT(DX*DX+DY*DY)
	ANGCOS=DX/L
	ANGSIN=DY/L
	SPRING=AREA(I)*YOUNG(I)/L
	KCS=SPRING*ANGCOS*ANGSIN
	KC2=SPRING*ANGCOS*ANGCOS
	KS2=SPRING*ANGSIN*ANGSIN
	B=NCON(I,1)
	E=NCON(I,2)
	TBM1=2*B-1
	TB=TBM1+1
	TEM1=2*E-1
	TE=TEM1+1
	STIFF(TBM1,TBM1)=STIFF(TBM1,TBM1)+KC2
	STIFF(TBM1,TB)=STIFF(TBM1,TB)+KCS
	STIFF(TBM1,TEM1)=STIFF(TBM1,TEM1)-KC2
	STIFF(TBM1,TE)=STIFF(TBM1,TE)-KCS
	STIFF(TB,TBM1)=STIFF(TB,TBM1)+KCS
	STIFF(TB,TB)=STIFF(TB,TB)+KS2
	STIFF(TB,TEM1)=STIFF(TB,TEM1)-KCS
	STIFF(TB,TE)=STIFF(TB,TE)-KS2
	STIFF(TEM1,TBM1)=STIFF(TEM1,TBM1)-KC2
	STIFF(TEM1,TB)=STIFF(TEM1,TB)-KCS
	STIFF(TEM1,TEM1)=STIFF(TEM1,TEM1)+KC2
	STIFF(TEM1,TE)=STIFF(TEM1,TE)+KCS
	STIFF(TE,TBM1)=STIFF(TE,TBM1)-KCS
	STIFF(TE,TB)=STIFF(TE,TB)-KS2
	STIFF(TE,TEM1)=STIFF(TE,TEM1)+KCS
	STIFF(TE,TE)=STIFF(TE,TE)+KS2
END DO

RETURN
END