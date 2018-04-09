!UWS,upwind scheme

program UWS
!L:length of computed area
!dx:length of single mesh(uniform grid) 
!u:velocity
!rho:density
!k:diffusion coefficient
!g:variable
!F:convection item
!D:diffusion item

implicit none 

integer(8) i,j
real(8) g(1:7)
real(8) dx,u,rho,k,F,D
real(8) ap(1:5),aw(1:5),ae(1:5),b(1:5),A(1:5,1:5)

    dx=0.2   !mesh length
    rho=1.0  !density
    k=0.1    !diffusion coefficient
    g(1)=1   !boundary 
    g(7)=0

print*,'u='
read*,u
             !constant property
    F=rho*u  !convection item
    D=k/dx   !diffusion item
    
!初始化矩阵A和向量b,y
do i=1,5
    b(i)=0.0
    do j=1,5
        A(i,j)=0.0
    enddo
enddo
    
do i=1,5
    if(i==1)          ap(1)=3.0*D+F; aw(1)=2.0*D+F; ae(1)=D
    if(1<i .AND. i<5) ap(i)=D*2.0+F; aw(i)=D+F; ae(i)=D
    if(i==5)          ap(5)=3.0*D+F; aw(5)=D+F; ae(5)=2.0*D
enddo
!input A & b
do i=1,5
    if(i==1) b(i)=-aw(i)*g(i)
    if(1<i .AND. i<5) b(i)=0
    if(i==5) b(i)=-ae(i)*g(i+2)
    
    A(i,i)=-ap(i)
    if(1<=i .AND. i<=4) A(i,i+1)=ae(i)
    if(2<=i .AND. i<=5) A(i,i-1)=aw(i)
enddo
end