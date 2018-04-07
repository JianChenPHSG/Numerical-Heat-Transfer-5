program main
integer,parameter :: dimen = 1,nt = 6
real (kind = 8) :: u(dimen,nt),x(dimen,nt),rho(nt),G(nt),phi(nt)
integer :: i,ind(nt),j
real(kind = 8) :: A(nt,nt),AA(nt-2,nt-2),b(nt-2)
phi=0.
phi(1)=1.0
u = 2.5
x(1,:) = (/((1.0/(nt-1)*(i-1)),i=1,nt)/)
rho = 1.0
ind = 0
ind(1)=1
ind(nt)=1
G = 0.1
call Cvt_MIX(dimen,u,x,ind,nt,rho,G,A)
AA = A(2:nt-1,2:nt-1)
b=-phi(1)*A(2:nt-1,1)-phi(nt)*A(2:nt-1,nt)
write(*,*) 'A:'
do i=1,nt
write(*,*) A(i,:)
end do
write(*,*) 'AA:'
do i=1,nt-2
write(*,*) AA(i,:)
end do


end program