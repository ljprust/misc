program gauss1d
implicit none
integer::npoints,ndigits,i,j,converged
real(kind=8)::c,m,a,b,integral,integralprev=0,x,f

! get input variables
write(*,*) 'Enter lower bound (a):'
read(*,*) a
write(*,*) 'Enter upper bound (b):'
read(*,*) b
write(*,*) 'Enter number of digits of accuracy:'
read(*,*) ndigits

! find the integral for different numbers of Gauss points
do npoints=2,10

! call the integration subroutine
call integrate(a,b,npoints,integral)

! call the error checker subroutine
call errchkr(integral,integralprev,ndigits,converged)

! stop if the desired accuracy has been reached
if (converged==1 .AND. npoints>2) then
exit
end if

! store the value of the previous integral (for error-checking purposes)
integralprev=integral

end do

! output the value of the integral to the user
write(*,*) 'The value of the integral is:',integral
end