module para
implicit none
!dimen:the porblem dimension,
!	 = 1; 1D
!	 = 2; 2D
!	 = 3; 3D
integer,parameter :: dimen = 1
!dp : the precision
integer,parameter :: dp = 4

!xPath: File input, and path is xPath
character(len=*),parameter :: xPath = "xinput.txt"

!indPath: File input, and path is indPath
character(len=*),parameter :: indPath = "indinput.txt"
!ind ::inner node:(0),boundary node(1);

!uInput:Method Input u
!     = .true. ; File input, and path is uPath
!     = .false.; determine the value by uValue
logical,parameter :: uInput = .false.
character(len=*),parameter :: uPath = "null"
real(kind = 8),parameter ::uValue = 2.5

!rhoInput:Method Input rho
!     = .true. ; File input, and path is rhoPath
!     = .false.; determine the value by rhoValue
logical,parameter :: rhoInput = .false.
character(len=*),parameter :: rhoPath = "null"
real(kind = 8),parameter ::rhoValue = 1.

!GInput:Method Input rho
!     = .true. ; File input, and path is GPath
!     = .false.; determine the value by GValue
logical,parameter :: GInput = .false.
character(len=*),parameter :: GPath = "null"
real(kind = 8),parameter ::GValue = .1

!nt : Number of the node
!integer :: nt

!form: The form choosed.
!   =1;CD
!   =2;UW
!   =3;MIX
integer,parameter :: form = 1
end module para