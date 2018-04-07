subroutine output(phi,nt)
    implicit none
    real(kind = 8),intent(in) :: phi(nt)
    integer,intent(in) :: nt
    integer :: unit,ierror
    character(len = 100) :: filename
    integer :: i
    unit = 25
    filename = 'output.txt'
    open(UNIT=unit,FILE=filename,STATUS='NEW',ACTION='WRITE',&
    	IOSTAT=ierror)
    if (ierror /= 0) then
        write(*,*) "ERROR:mistake occur when opening the file.[output]"
    endif
    write(unit,100) (phi(i),i=1,nt)
100 format(F6.4)
    close(unit)


    
end subroutine output