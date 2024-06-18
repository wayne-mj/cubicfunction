program cubic
    use factors
    use mergesort
    use quadratic
    use newton
    use rationalroot
    implicit none

    ! Rewriten to work with real numbers
    integer :: a, b, c, d, i
    integer :: x, xp 
    integer, parameter :: start = -1
    integer, parameter :: end = 5
    real :: y, xreal, xout 
    real, allocatable, dimension(:) :: pq
    real, allocatable, dimension(:) :: temp
    character(1), parameter :: newline = achar(10)
    logical :: graph

    ! Set the flags to false
    graph = .false.

    ! A little bit of a menu
    print *, newline // 'Cubic equation solver' // newline // 'Enter the coefficients of the equation ax^3 + bx^2 + cx + d = 0' // newline

    ! Get the coefficients all at once
    write (*, '(A)', advance='no') 'Enter a, b, c, d: '
    read (*,*) a, b, c, d

    
    pq = candidates(a, d)

    do i = 1, size(pq)
        temp = syntheticdivision(a, b, c, d, pq(i))
        print *, temp
    end do
    
    print *, " "

    do i = 1, size(pq)
        call newton_r(a, b, c, d, pq(i), 1e-7, xout)
        print *, xout
    end do

    ! If there are real roots, write the data to a file
    if (graph .eqv. .true.) then
        open (unit=10, file='cubic.dat', status='replace')
        do x = start, end
            do xp = 1,9
                xreal = real(x) + (real(xp) / 10.)
                y = (a*(real(xreal)**3)) + (b*(real(xreal**2))) + (c * (real(xreal))) + d
                write (10, '(2F15.2)') real(xreal),  real(y)
            end do
        end do
        flush (10)
        close (10)
    end if

end program cubic
