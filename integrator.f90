subroutine integrate(a,b,npoints,integral)
real(kind=8)::a,b,integral,c,m,f,x,t(10,10),w(10,10)
integer::npoints,j

! do a variable transformation
c=0.5*(b+a)
m=0.5*(b-a)

! initialize the sum at zero
integral=0.0

! create a do-loop do sum the contributions from the Gauss points
do j=1,npoints

! call the subroutine containing the Gauss points and weights
call tw_vals(t,w)

! apply the variable transformation
x=c+m*t(npoints,j)

! call the subroutine containing the function to be integrated
call intfunc(f,x)

! sum the contributions from the Gauss points
integral=integral+m*w(npoints,j)*f

end do

! end the subroutine
return
end