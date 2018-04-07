program Work1st
    use para
    use FileToMatrix_mod
    implicit none
    real(kind = 8) :: u(dimen,nt),x(dimen,nt)
    real(kind = 8) :: rho(nt),G(nt)
    integer :: ind(nt)
    real(kind = 8) :: A(nt,nt),AA(nt-2,nt-2),b(nt-2)
    integer :: i,ierr
    integer :: Row,Column
    real(kind = 8) :: phi(nt)
    real(kind = 4),allocatable ::temp1(:,:)
    
!------------------------------Input-----------------------------------
!Input x
    call FileToMatrix(xPath,temp1,Row,Column)
    x = REAL(temp1,8)
    if (Column /= nt) then
        write(*,*) "ERROR:>>Input Number is wrong!"
        stop
    end if
!Input ind
    call FileToMatrix(indPath,temp1,Row,Column)
    ind = reshape(INT(temp1),(/nt/))
    if (Column /= nt) then
        write(*,*) "ERROR:>>Input Number is wrong!"
        stop
    end if
!Input u
    if (uInput) then
        call FileToMatrix(uPath,temp1,Row,Column)
        u = REAL(temp1,8)
            if (Column /= nt) then
                write(*,*) "ERROR:>>Input Number is wrong!"
                stop
            end if
        else
        u = uValue
    endif
!Input rho
    if (rhoInput) then
        call FileToMatrix(rhoPath,temp1,Row,Column)
        rho = RESHAPE(real(temp1,8),(/nt/))
            if (Column /= nt) then
                write(*,*) "ERROR:>>Input Number is wrong!"
                stop
            end if
        else
        rho = rhoValue
    endif  
!Input G
    if (GInput) then
        call FileToMatrix(GPath,temp1,Row,Column)
        G = RESHAPE(REAL(temp1,8),(/nt/))
            if (Column /= nt) then
                write(*,*) "ERROR:>>Input Number is wrong!"
                stop
            end if
        else
        G = GValue
    endif

!Input Boundary Condition
    phi=0.
    phi(1)=1.0

!---------------------------Transform------------------------------------
    if (form == 1) then
        call Cvt_CD(dimen,u,x,ind,nt,rho,G,A)
    else if (form == 2) then
        call Cvt_UW(dimen,u,x,ind,nt,rho,G,A)
    else if (form == 3) then
        call Cvt_MIX(dimen,u,x,ind,nt,rho,G,A)
    else
        stop
        write(*,*) "ERROR:>>Wrong Form number!"
    end if

!--------------------------Calculation-----------------------------------
    AA = A(2:nt-1,2:nt-1)
    b=-phi(1)*A(2:nt-1,1)-phi(nt)*A(2:nt-1,nt)
    call solve(AA,b,phi(2:nt-1),nt-2,1)
    
!---------------------------OutPut-------------------------------------
    call output(phi,nt)
end program