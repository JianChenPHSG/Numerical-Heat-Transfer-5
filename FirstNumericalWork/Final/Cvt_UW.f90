subroutine Cvt_UW(dimen,u,x,ind,nt,rho,G,A)
!a_Pf_P=a_Ef_E+a_Wf_W
!a_E=De+[-Fe,0];a_W=Dw+[Fw,0]
!input:
!dimen:the dimension of the problem
!u	  :the velocity of the node,positive sign represent the direction is from upward.  [in]
!x	  :the position of the node.													   [in]
!ind  :inner node:(0),boundary node(1);                                                [in]
!nt	  :the number of total node.                                           	           [in]
!rho  :density in node.                                                                [in]
!G    :diffusion coefficient.                                                          [in]
!A    :The matrix of coefficient.                                                     [out]

    implicit none
    integer,intent(in) :: nt,dimen
    real(kind = 8),intent(in) :: u(dimen,nt),x(dimen,nt)
    real(kind = 8),intent(in) :: rho(nt),G(nt)
    integer,intent(in) :: ind(nt)
    real(kind = 8),intent(out) :: A(nt,nt)

    integer :: i,j
    real(kind = 8) :: De,Dw,Fe,Fw,aE,aW,aP
    
    A = 0.
    do i=1,nt
        if (ind(i) == 1) cycle
        !Diffusion term CD
        De = 0.5*(G(i)+G(i+1))/abs(x(1,i+1)-x(1,i))
        Dw = 0.5*(G(i)+G(i-1))/abs(x(1,i)-x(1,i-1))
        !Convection term CD
        Fe = 0.5*(rho(i)*u(1,i)+rho(i+1)*u(1,i+1))
        Fw = 0.5*(rho(i)*u(1,i)+rho(i-1)*u(1,i-1))
        aE = De + max(-Fe,0.)
        aW = Dw + max( Fw,0.)
        aP = aE + aW + (Fe - Fw)
        A(i,i) = aP
        A(i,i+1) = -aE
        A(i,i-1) = -aW
    enddo
end subroutine Cvt_UW