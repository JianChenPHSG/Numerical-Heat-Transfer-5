subroutine test_all(B,nt)
use variable
implicit none
integer,intent(inout) :: nt
real(kind = 8),allocatable :: B(:,:)
integer :: ERR_MESSAGE

nt =nt + 1
DEALLOCATE(A,STAT=ERR_MESSAGE)
IF(ERR_MESSAGE.NE.0) PRINT *,'DEALLOCATION ERROR[test_all]'
ALLOCATE(B(nt,nt))
end subroutine