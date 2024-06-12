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
end module quadratic