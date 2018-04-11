subroutine AdapMeshIni(phi,x,nt,tempnum,list,num)
use para
implicit none
integer,intent(in):: nt
real(kind = 8),intent(in),allocatable :: phi(:)
real(kind = 8),intent(in),allocatable :: x(:,:)
integer,intent(out),allocatable :: list(:)
integer,intent(out) :: num,tempnum
integer :: i,d
real(kind = 8),allocatable:: dphi(:,:)
    ALLOCATE(list(nt-1))
    list = 0
    num = 0
    ALLOCATE(dphi(dimen,nt-1))
    do i=1,nt-1
        do d=1,dimen
            dphi(d,i) = (phi(i+1)-phi(i))/(x(d,i+1)-x(d,i))
            if (i>1) then
                if (dabs(dphi(dimen,i)-dphi(dimen,i-1))>0.5*MIN(dphi(dimen,i),dphi(dimen,i-1))) then
                    num = num + 1
                    list(num) = i

                end if
            end if
        end do
    end do
    !tempnum: inset number
    tempnum = 0
    if (num /= 1) then
        do i=1,num
            if (list(i)+1 == list(i+1) .or. i==num ) then
                tempnum = tempnum + 1
            else
                tempnum = tempnum + 2
            end if
        end do
    else
    tempnum = num + 1
    end if
    write(*,*) tempnum

    do i=1,num
        if (list(i)+1 == list(i+1) .or. i==num ) then
            tempnum = tempnum + 1
        else
            tempnum = tempnum + 2
        end if
    end do

end subroutine