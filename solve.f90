subroutine solve(A,B,X,N,M)
!---------------------------------subroutine  comment
!  Version   :  V1.0    
!  Coded by  :  zcy
!  Date      :  2018-4-4
!-----------------------------------------------------
!  Purpose   :  高斯列主元消去法计算矩阵方程组
!                 AX=B
!-----------------------------------------------------
!A :A(N,N)                                          [in]
!B :B(N,M)                                          [in]
!N :the row number.                                 [in]
!M :the column number.                              [in]
!X :X(N,M)the solve of equation.                   [out]

    implicit none

    integer,       intent(in)  :: N, M
    real(kind = 8),intent(in)  :: A(N,N),B(N,M)
    real(kind = 8),intent(out) :: X(X,M)

    integer :: i, k
    integer :: id_max  !主元素坐标
    real(kind = 8) :: elmax,temp
    real(kind = 8) :: Aup(N,N),Bup(N,M),AB(N,N+M)
    real(kind = 8) :: vtemp1(N+M),vtemp2(N+M)
    real(kind = 8) :: vtmp(N),xtemp(N)
    integer        :: list(N)=(/(i,i=1,5)/)
    real(kind = 8) :: Xtempp(N,M)

!设置增广阵AB=[A B]
    AB(:,1:N)    = A
    AB(:,N+1:N+M)= B
    do k=1,N-1
        elmax = dabs(AB(k,k))
        id_max = k

    	do i=k+1,N
    		if (dabs(AB(i,k)) > elmax) then
                elmax = AB(i,k)
                id_max = i
    		endif
    	enddo

        vtemp1 = AB(k,:)
        vtemp2 = AB(id_max,:) 
        AB(k,:) = vtemp2
        AB(id_max,:) = vtemp1
        list(k) = id_max
        list(id_max) = k


        do i = k+1,N
            temp = AB(i,k) / AB(k,k)
            AB(i,:) = AB(i,:) - temp * AB(k,:)
        enddo
    enddo

    Aup(:,:)=AB(1:N,1:N)

    do i=1,M
        vtmp = AB(:,N+i)
        call uptri(Aup,vtmp,xtmp,N)
        X(:,i) = xtmp
    enddo
    
    Xtempp = X
    do i=1,N
    	X(list(i),:) = Xtempp(i,:)
    enddo

end subroutine solve

subroutine uptri(A,b,x,N)
!---------------------------------subroutine  comment
!  Version   :  V1.0    
!  Coded by  :  zcy
!  Date      :  2018-4-4
!-----------------------------------------------------
!  Purpose   :  高斯列主元消去法计算矩阵方程组
!                 AX=B
!-----------------------------------------------------
!A :A(N,N)                                          [in]
!B :B(N,M)                                          [in]
!N :the row number.                                 [in]
!X :X(N,M)the solve of equation.                   [out]
    implicit none
    integer,       intent(in)  :: N
    real(kind = 8),intent(in)  :: A(N,N),b(N)
    real(kind = 8),intent(out) :: x(N) 
    integer :: i,j
    x(N)=b(N) / A(N,N)
    do i=n-1,1,-1
    	x(i) = b(i)
    	do j=i+1,N
    		x(i) = x(i) - A(i,j)*x(j)
    	enddo
    	x(i) = x(i) / A(i,i)
    enddo
end subroutine uptri