module quadratic
    implicit none
    contains

    ! Function to calculate the roots of a quadratic equation
    ! ax^2 + bx + c = 0
    subroutine quad_roots(a,b,c,x1,x2)
        real, intent(in) ::  a,b,c 
        real, intent(out) :: x1,x2
        real :: dis 

        dis = b**2 - 4*a*c
        x1 = (-b + sqrt(dis))/(2*a)
        x2 = (-b - sqrt(dis))/(2*a)
    end subroutine quad_roots

    ! Function to calculate the critical points of a cubic equation
    subroutine criticalpoints(a,c,b,x1,x2,xi)
        integer, intent(in) :: a,b,c
        real, intent(out) :: x1,x2,xi
        real :: da, db, dc, dis

        ! Coefficients of the derivative of the cubic equation
        da = 3. * real(a)
        db = 2. * real(b)
        dc = real(c)

        dis = (db**2) - (3*da*dc)

        x1 = (-db + sqrt(dis))/(3*da)
        x2 = (-db - sqrt(dis))/(3*da)
        xi = -db/(3*da)
    end subroutine criticalpoints
end module quadratic