module FileToMatrix_mod
contains
    integer function Getcolumn(iFileUnit)
        implicit none
        integer, intent(in) :: iFileUnit
        character(len = 512) :: str
        integer :: i,ierr,err
        logical :: IsSeparator
        Getcolumn = 0
        IsSeparator = .true.
        rewind( iFileUnit )
        read(iFileUnit,'(a512)',ioStat = ierr) str
        if ( ierr /= 0) then
            write(*,*) "ERROR:Can't read the character.[Getcolumn]"
        endif
        rewind( iFileUnit )
        do i=1,len_Trim(str)
            select case (str(i:i))
            case( " " , "," , char(9) )
                IsSeparator = .true.
            case default
                if ( IsSeparator) then
                    Getcolumn=Getcolumn + 1
                endif
                IsSeparator = .false.
            endselect
        enddo       
    end function Getcolumn

    integer function GetRow(iFileUnit)
        implicit none
        integer, intent(in) :: iFileUnit
        integer :: ierr
        GetRow = 0
        rewind( iFileUnit )
        do while (.true.)
            Read( iFileUnit , * , ioStat = ierr ) 
            If( ierr /= 0 ) Exit
            GetRow = GetRow + 1
        enddo
        rewind( iFileUnit )
    end function GetRow
    
    subroutine FileToMatrix(FilePath,M,Row,Column)

    implicit none
    character(len = *),intent(in) :: FilePath
    integer,parameter :: iFileUnit = 10
    integer,intent(out) :: Row,Column
    integer :: i,j,err
    real(kind = 4 ),allocatable,intent(out):: M(:,:)
    !save :: M
    open(unit = iFileUnit,file = FilePath , status='old',action='read')
    Row    = GetRow(iFileUnit)
    Column = Getcolumn(iFileUnit)
    allocate(M(Row,Column),stat = err)
    if (err /= 0) then
        write(*,*) "ERROR:Can't allocate M.[FileToMatrix]"
        stop
    endif
    read(iFileUnit,*) ((M(i,j),j=1,Column),i=1,Row)
    close(iFileUnit)
end subroutine FileToMatrix

end module FileToMatrix_mod

