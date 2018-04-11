program Work1st
use para
use FileToMatrix_mod
implicit none
    integer :: nt
    real(kind = 8),allocatable :: u(:,:),x(:,:)
    real(kind = 8),allocatable :: rho(:),G(:)
    integer,allocatable :: ind(:)
    real(kind = 8),allocatable :: A(:,:),AA(:,:),b(:)
    integer :: i,ierr,j,d,ERR_MESSAGE
    integer :: Row,Column
    real(kind = 8),allocatable :: phi(:)
    real(kind = 4),allocatable ::temp1(:,:)
!Adapative mesh varible
    real(kind = 8),allocatable :: nu(:,:),nx(:,:)
    real(kind = 8),allocatable :: nrho(:),nG(:)
    integer,allocatable :: nind(:)
    integer,allocatable :: list(:)
    real(kind = 8),allocatable:: dphi(:,:)
    integer :: tempnum,num,tempnum2,itemp1,itemp2
    real(kind = 8) :: temp2,temp3


!------------------------------Input-----------------------------------
OPEN(unit = 10, file =xPath, status='old',action='read')
nt=Getcolumn(10)
CLOSE(10)
ALLOCATE(u(dimen,nt))
ALLOCATE(x(dimen,nt))
ALLOCATE(rho(nt))
ALLOCATE(G(nt))
ALLOCATE(ind(nt))
ALLOCATE(phi(nt))
ALLOCATE(A(nt,nt))
ALLOCATE(AA(nt-2,nt-2))
ALLOCATE(b(nt-2))

!Input x
    call FileToMatrix(xPath,temp1,Row,Column)
    x = REAL(temp1,8)
    if (Column /= nt) then
        write(*,*) "ERROR:>>Input Number is wrong![xinput]"
        stop
    end if
!Input ind
    call FileToMatrix(indPath,temp1,Row,Column)
    ind = reshape(INT(temp1),(/nt/))
    if (Column /= nt) then
        write(*,*) "ERROR:>>Input Number is wrong![indinput]"
        stop
    end if
!Input u
    if (uInput) then
        call FileToMatrix(uPath,temp1,Row,Column)
        u = REAL(temp1,8)
            if (Column /= nt) then
                write(*,*) "ERROR:>>Input Number is wrong![uinput]"
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
                write(*,*) "ERROR:>>Input Number is wrong![rhoInput]"
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
                write(*,*) "ERROR:>>Input Number is wrong![GInput]"
                stop
            end if
        else
        G = GValue
    endif
do while (.true.)
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
        write(*,*) "ERROR:>>Wrong Form number![form]"
    end if

!--------------------------Calculation-----------------------------------
    AA = A(2:nt-1,2:nt-1)
    b=-phi(1)*A(2:nt-1,1)-phi(nt)*A(2:nt-1,nt)
    call solve(AA,b,phi(2:nt-1),nt-2,1)
!--------------------------Adaptive Mesh---------------------------------

    !call AdapMeshIni(phi,x,nt,tempnum,list,num)
!determint the insert number, and inserting position.
    ALLOCATE(list(nt-1))
    list = 0
    num = 0
    ALLOCATE(dphi(dimen,nt-1))
    do i=1,nt-1
        do d=1,dimen
            dphi(d,i) = (phi(i+1)-phi(i))/(x(d,i+1)-x(d,i))
            if (i>1) then
            temp2 = dabs(dphi(dimen,i)-dphi(dimen,i-1))
            temp3 = 0.5*MIN(dabs(dphi(dimen,i)),dabs(dphi(dimen,i-1)))

                if ((temp2 > temp3 .OR. rho(i)*u(1,i)*(x(d,i+1)-x(d,i))/G(i)>2).AND. temp2>1e-2) then
                !if (temp2 > temp3 .AND. temp2 > 0.1) then
                    num = num + 1
                    list(num) = i
                end if
            end if
        end do
    end do
    if (num == 0) then
        exit
    end if
    !tempnum: inset number
    tempnum = 0
    if (num /= 1) then
        do i=1,num
            if (.not.(list(i)+1 == list(i+1) .OR. i/=num)) then
                if (list(i) == nt -1) then
                tempnum = tempnum + 1
                else
                tempnum = tempnum + 2
                endif
            else
                tempnum = tempnum + 1
            end if
        end do
    else 
    tempnum = num + 1
    end if

    tempnum2 = tempnum
!generate new x,u,rho,G,ind
    ALLOCATE(nx(dimen,nt+tempnum),nu(dimen,nt+tempnum))
    ALLOCATE(nrho(nt+tempnum),nG(nt+tempnum),nind(nt+tempnum))
    nx = 0.
    nu = 0.
    nrho = 0.
    nG = 0.
    nind = 0
    nx(:,1:nt) = x(:,1:nt)
    nu(:,1:nt) = u(:,1:nt)
    nrho(1:nt) = rho(1:nt)
    nG(1:nt)   = G(1:nt)
    nind(1:nt) = ind(1:nt)
    tempnum = 0

    do i=1,num
        !insert the new value
        nx(:,tempnum+list(i)+1)=0.5*(x(:,list(i))+x(:,list(i)+1))
        nu(:,tempnum+list(i)+1)=0.5*(u(:,list(i))+u(:,list(i)+1))
        nrho(tempnum+list(i)+1)=0.5*(rho(list(i))+rho(list(i)+1))
        nG(tempnum+list(i)+1)  =0.5*(G(list(i))+G(list(i)+1))
        nind(tempnum+list(i)+1)= 0
        tempnum = tempnum + 1
        nx(:,tempnum+list(i)+1:tempnum+list(i)+1+(nt-list(i)-1))=x(:,list(i)+1:nt)
        nu(:,tempnum+list(i)+1:tempnum+list(i)+1+(nt-list(i)-1))=u(:,list(i)+1:nt)
        nrho(tempnum+list(i)+1:tempnum+list(i)+1+(nt-list(i)-1))=rho(list(i)+1:nt)
        nG(tempnum+list(i)+1:tempnum+list(i)+1+(nt-list(i)-1))  =G(list(i)+1:nt)
        nind(tempnum+list(i)+1:tempnum+list(i)+1+(nt-list(i)-1))=ind(list(i)+1:nt)
        if (.not.(list(i)+1 == list(i+1) .OR. i/=num) ) then
            if (list(i) /= nt -1) then
            nx(:,tempnum+list(i)+2) = 0.5*(x(:,list(i)+1)+x(:,list(i)+2))
            nu(:,tempnum+list(i)+2) = 0.5*(u(:,list(i)+1)+u(:,list(i)+2))
            nrho(tempnum+list(i)+2) = 0.5*(rho(list(i)+1)+rho(list(i)+2))
            nG(tempnum+list(i)+2)   = 0.5*(G(list(i)+1)+G(list(i)+2))
            nind(tempnum+list(i)+2) = 0
            tempnum = tempnum + 1
            itemp1 = tempnum+list(i)+2
            itemp2 = tempnum+list(i)+2+(nt-list(i)-2)
            nx(:,itemp1:itemp2) = x(:,(list(i)+2):nt)
            nu(:,itemp1:itemp2)  = u(:,list(i)+2:nt)
            nrho(itemp1:itemp2) = rho(list(i)+2:nt)
            nG(itemp1:itemp2)   = G(list(i)+2:nt)
            nind(itemp1:itemp2) = ind(list(i)+2:nt)
            end if
        end if
    enddo
    if (tempnum /= tempnum2) then
        STOP
    end if
    DEALLOCATE(AA,A,b,list,dphi)
    DEALLOCATE(x,u,rho,G,ind,phi)

    nt = nt + tempnum
    
    ALLOCATE(u(dimen,nt))
    ALLOCATE(x(dimen,nt))
    ALLOCATE(rho(nt))
    ALLOCATE(G(nt))
    ALLOCATE(ind(nt))
    ALLOCATE(phi(nt))
    ALLOCATE(A(nt,nt))
    ALLOCATE(AA(nt-2,nt-2))
    ALLOCATE(b(nt-2))
    u = nu
    x = nx
    rho = nrho
    G = nG
    ind =nind
    write(*,*) nt
    DEALLOCATE(nx,nu,nrho,nG,nind)
    !EXIT!temperary termination.
end do
!---------------------------OutPut-------------------------------------
    call output(phi,nt)
  !  phi(2:nt-1) = phi(1)+(phi(nt)-phi(1))*(EXP(rho(2:nt-1)*u(dimen,2:nt-1)*x(dimen,2:nt-1)/G(2:nt-1))-1)/(EXP(rho(nt)*u(dimen,nt)*x(dimen,nt)/G(nt))-1)
phi(2:nt-1)=(EXP(rho(2:nt-1)*u(dimen,2:nt-1)*x(dimen,2:nt-1)/G(2:nt-1))-1)/(EXP(rho(nt)*u(dimen,nt)*x(dimen,nt)/G(nt))-1)
phi(2:nt-1) = phi(2:nt-1)*(phi(nt)-phi(1))+phi(1)
    CALL Exactoutput(phi,nt)
    STOP
end program