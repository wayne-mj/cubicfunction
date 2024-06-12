program cubic
    use factors
    use mergesort
    use quadratic
    implicit none

    ! Rewriten to work with real numbers
    integer :: a, b, c, d, t, i,j, arrsize, count
    integer, allocatable :: q(:), p(:)
    integer :: x, xp 
    integer, parameter :: range = 5
    real :: x1, x2, x3, rem, y, xi, qa, qb, qc, xreal 
    real, allocatable :: pq(:), temp(:)
    character(1), parameter :: newline = achar(10)
    logical :: graph, notone, notminusone

    ! Set the flags to false
    notone = .false.
    notminusone = .false.
    graph = .false.

    count =1

    ! A little bit of a menu
    print *, newline // 'Cubic equation solver' // newline // 'Enter the coefficients of the equation ax^3 + bx^2 + cx + d = 0' // newline

    ! Get the coefficients all at once
    write (*, '(A)', advance='no') 'Enter a, b, c, d: '
    read (*,*) a, b, c, d
    
    ! print *, a,b,c,d
    ! Check if x = 1
    t = a + b + c + d
    if (t .eq. 0) then
        notone = .true.
        x1 = 1
    end if

    ! Check if x = -1
    if ((a+c) .eq. (b+d)) then
        notminusone = .true.
        x1 = -1
    end if

    ! Factor p/q where p = d, and q = a
    q = factoring(a)
    p = factoring(d)

    ! Calculate p / q
    arrsize = size(p)*size(q)
    allocate(pq(arrsize))
    do i = 1, size(q)
        do j = 1, size(p)
            pq(count) = real(p(j)) / real(q(i))
            count = count + 1
        end do
    end do

    call mergesort_real(pq, size(pq))
    
    ! Synthetically divide the factors
    ! If the remainder is zero, then the factor is a root
    if (notone .eqv. .false. .or. notminusone .eqv. .false.) then
        qa = a
        qb = (qa * x1) + b
        qc = (qb * x1) + c
        rem = (qc * x1) + d
        call quad_roots(qa, qb, qc, x2, x3)
        print *, "Roots: ", x1, x2, x3
        graph = .true.
    else
        do i = 1, size(pq)
            qa = a
            qb = (qa * pq(i)) + b
            qc = (qb * pq(i)) + c
            rem = (qc * pq(i)) + d

            if (rem == 0.) then
                call quad_roots(qa, qb, qc, x2, x3)
                print *, "Roots: ", pq(i), x2, x3
                graph = .true.
            ! else
            !     print *, "No roots"
            end if
        end do
    end if

    ! If there are real roots, write the data to a file
    if (graph .eqv. .true.) then
        open (unit=10, file='cubic.dat', status='replace')
        do x = -range, range
            do xp = 1,range + 4
                xreal = real(x) + (real(xp) / 10.)
                y = (a*(real(xreal)**3)) + (b*(real(xreal**2))) + (c * (real(xreal))) + d
                write (10, '(2F15.2)') real(xreal),  real(y)
            end do
        end do
        flush (10)
        close (10)
    end if
end program cubic
