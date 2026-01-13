module rationalroot
    use factors
    use mergesort
    use quadratic
    implicit none

    contains

    ! Does x = 1
    ! If x is 1 and the coefficients are real integers
    ! then the sum of a, b, c, and d will be 0
    function x_equals_1(a,b,c,d) result(yes_or_no)
        integer, intent(in) :: a,b,c,d
        logical :: yes_or_no

        yes_or_no = .false.

        if (a + b + c + d .eq. 0) yes_or_no = .true.
    end function x_equals_1

    ! Does x = -1
    ! If x is -x and the coefficients are real integers
    ! then the sum of a and c will equal the sum of b and d.
    function x_equals_minus_1(a,b,c,d) result(yes_or_no)
        integer, intent(in) :: a,b,c,d 
        logical :: yes_or_no

        yes_or_no = .false.

        if ((a+c) .eq. (b+d)) yes_or_no = .true.
    end function x_equals_minus_1

    ! Find the candidates for rational roots
    ! This function finds all possible candidates for the synthetic 
    ! division sorting them.
    function candidates (a,d) result(pq)
        integer, intent(in) :: a,d
        real, allocatable, dimension(:) :: pq
        integer, allocatable, dimension(:) :: q
        integer, allocatable, dimension(:) :: p
        integer :: arrsize, count, i,j

        count = 1

        q = factoring(a)
        p = factoring(d)

        arrsize = size(q) * size(p) * 2
        allocate(pq(arrsize))

        ! Find the + candidates
        do i = 1, size(q)
            do j = 1, size(p)
                pq(count) = real(p(j)) / real(q(i))
                count = count + 1
            end do
        end do
    
        ! Find the - candidates
        do i = 1, size(q)
            do j = 1, size(p)
                pq(count) = -(real(p(j)) / real(q(i)))
                count = count + 1
            end do
        end do
    
        ! Sort the array
        call mergesort_real(pq, size(pq))
    end function candidates

    ! Perform synthetic division
    ! No matter how many videos I watch on synthetic division
    ! I still do not understand it.
    ! But this is what I cobbled together to get the quadratic equation.
    function syntheticdivision(a,b,c,d,x1) result(roots)
        integer, intent(in) :: a,b,c,d
        real, intent(in) :: x1
        real, allocatable, dimension(:) :: roots
        real :: qa, qb, qc, rem, x2, x3

        qa = a
        qb = (qa*x1) + b
        qc = (qb*x1) + c
        rem = (qc*x1) + d

        if (rem .eq. 0.) then
            call quad_roots(qa, qb, qc, x2, x3)
        else
            x2 = -huge(1.)
            x3 = -huge(1.)
        end if

        allocate(roots(3))
        roots(1) = x1
        roots(2) = x2
        roots(3) = x3
    end function syntheticdivision
end module rationalroot