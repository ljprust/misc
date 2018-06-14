subroutine intfunc(f,x)
real(kind=8)::f,x

! define the function to be integrated
f=x*exp(x)/(1+x)**2

! end the subroutine
return
end
