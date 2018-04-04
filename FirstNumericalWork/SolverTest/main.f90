program main
use FileToMatrix_mod
    implicit none
    real(kind = 4),allocatable :: A(:,:),b(:,:)
    real(kind = 8),allocatable :: x(:,:)
    integer :: Row,Column,temp
    call FileToMatrix('A.txt',A,Row,temp)
    call FileToMatrix('b.txt',b,temp,Column)
    ALLOCATE(x(Row,Column))
    CALL solve(REAL(A,8),real(b,8),x,Row,Column)
    write(*,*) x
    call system("pause")
end program
