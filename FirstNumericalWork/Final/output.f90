subroutine output(phi)
    use para
    implicit none
    real(kind = 8),intent(in) :: phi(nt)
    integer :: unit,ierror
    character(len = 100) :: filename
    character(len = 100) :: temp
    character(len = 100) :: temp1
    integer :: i
    unit = 25
    write(temp,*) real(uValue,4)
    if (form  == 1) then
        temp1="CD"
        else if (form == 2) then
        temp1 = "UW"
        else if (form == 3) then
        temp1 = "MIX"
        else
        write(*,*)"ERROR:>> Wrong form is choosed![output]"
    end if
        
    filename = 'output'//TRIM(ADJUSTL(temp1))//TRIM(ADJUSTL(temp))//'.txt'
    open(UNIT=unit,FILE=filename,STATUS='REPLACE',ACTION='WRITE',&
    	IOSTAT=ierror)
    if (ierror /= 0) then
        write(*,*) "ERROR:mistake occur when opening the file.[output]"
    endif
    write(unit,100) (phi(i),i=1,nt)
100 format(F9.7)
    close(unit)


    
end subroutine output