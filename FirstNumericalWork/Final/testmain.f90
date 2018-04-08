program Work1sttest
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
    real(kind = 4),allocatable ::temp1(:,:),temp2(:)
    
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

end program