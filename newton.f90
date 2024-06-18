module newton
    implicit none
    contains

    ! Newton-Raphson method for solving a cubic equation
    subroutine newton_r(a,b,c,d,xin,tol, x)
        integer, intent(in) :: a,b,c,d
        real, intent(in) :: tol
        real, intent(in) :: xin
        real :: f,df,x1, x0, x
        integer :: i
        integer, parameter :: max_iter = 10000

        x0 = xin

        do i=1, max_iter
            f = a*x0**3 + b*x0**2 + c*x0 + d
            df = 3*a*x0**2 + 2*b*x0 + c
            x1 = x0 - f/df
            if (abs(x1-x0) .lt. tol) then
                x = x1
                return
            end if
            x0 = x1
        end do
    end subroutine newton_r
end module newton