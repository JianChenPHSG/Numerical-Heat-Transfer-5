
program testx
real(kind = 8) :: a=-0.1,b=0.2,c
c = min(dabs(a),dabs(b))
write(*,*) c
end program