subroutine errchkr(integral,integralprev,ndigits,converged)
real(kind=8)::integral,integralprev,eps,relerror
integer::converged

! calculate epsilon and the relative error
eps=1.0/(10.0**ndigits)
relerror=abs((integral-integralprev)/integral)

! indicate whether or not the desired accuracy has been reached
if (relerror<=eps) then
converged=1
else
converged=0
end if

! end the subroutine
return
end