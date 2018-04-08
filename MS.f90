!MS£¬Mix scheme
!code by Ren Zhuo

program CDS(n,u,rho,k,A)
!variable define
!L:length
!u:velocity
!F:convection item
!D:diffusion item
!k:diffusion coefficient
!n:the numbers of nodes  
!rho:density
!fai:unknown quantity
!output coefficient matrix

implicit none

integer i
integer,intent(in)::n 
real,intent(in)::u(n),rho(n),k(n)
real,intent(out)::A(n,n)
real(4) De(n),Dw(n),Fe(n),Fw(n),aw(n),ae(n),ap(n)

do i=1,n

    De(i)=(k(i)+k(i+1))/0.2
    Dw(i)=(k(i-1)+k(i))/0.2

    Fe(i)=0.5*(rho(i)*u(i)+rho(i+1)*u(i+1))
    Fw(i)=0.5*(rho(i-1)*u(i-1)+rho(i)*u(i))

!coefficient
    aw(i)=max(Fw(i),Dw(i)+Fw(i)/2.0,0.0)
    ae(i)=max(-Fe(i),De(i)-Fe(i)/2.0,0.0)
    ap(i)=ae(i)+aw(i)+Fe(i)-Fw(i)
    
    A(i,i)=ap(i)
    A(i,i+1)=-ae(i)
    A(i,i-1)=-aw(i)
    
enddo
end
