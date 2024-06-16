program cubic
    use factors
    use mergesort
    use quadratic
    use newton
    implicit none

    ! Rewriten to work with real numbers
    integer :: a, b, c, d, t, i,j, arrsize, count
    integer, allocatable :: q(:), p(:)
    integer :: x, xp 
    integer, parameter :: start = -1
    integer, parameter :: end = 5
    real :: x1, x2, x3, rem, y, cx1, cx2, cxi, qa, qb, qc, xreal, lastx1
    real, allocatable :: pq(:), temp(:)
    character(1), parameter :: newline = achar(10)
    logical :: graph, notone, notminusone, found

    ! Set the flags to false
    notone = .false.
    notminusone = .false.
    graph = .false.
    found = .false.

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
    arrsize = (size(p)*size(q)) * 2
    allocate(pq(arrsize))
    do i = 1, size(q)
        do j = 1, size(p)
            pq(count) = real(p(j)) / real(q(i))
            count = count + 1
        end do
    end do

    do i = 1, size(q)
        do j = 1, size(p)
            pq(count) = -(real(p(j)) / real(q(i)))
            count = count + 1
        end do
    end do

    call mergesort_real(pq, size(pq))
    
    ! Debugging
    !print *, "Factors: ", pq

    ! Synthetically divide the factors
    ! If the remainder is zero, then the factor is a root
    if (notone .eqv. .false. .or. notminusone .eqv. .false.) then
        qa = a
        qb = (qa * x1) + b
        qc = (qb * x1) + c
        rem = (qc * x1) + d
        call quad_roots(qa, qb, qc, x2, x3)
        call criticalpoints(a, b, c, cx1, cx2, cxi)
        print *, "Roots: ", x1, x2, x3
        print *, "Critical points: ", cx1, cx2, cxi
        graph = .true.
        found = .true.
    else
        do i = 1, size(pq)
            qa = a
            qb = (qa * pq(i)) + b
            qc = (qb * pq(i)) + c
            rem = (qc * pq(i)) + d

            ! Debugging
            ! print *, "Remainder: ", rem

            if (rem == 0.) then
                call quad_roots(qa, qb, qc, x2, x3)
                call criticalpoints(a, b, c, cx1, cx2, cxi)
                print *, "Roots: ", pq(i), x2, x3
                print *, "Critical points: ", cx1, cx2, cxi
                graph = .true.
                found = .true.
             else
                 print *, "No rational roots found."
                 found = .false.
                !  print *, "Trying Newton Raphson method."
                !  call newton_r(a, b, c, d, pq(i), 1.0e-7, x1)
                !  print *, "Possible root: ", x1
            end if
        end do

        ! If no rational roots are found, try the Newton Raphson method
        if (found .eqv. .false.) then
            print *, "Trying Newton Raphson method."
            do i = 1, 150
                call newton_r(a, b, c, d, real(i), 1.0e-7, x1)
                ! Remove repeated roots
                if (x1 .ne. lastx1) then
                    print *, "Possible root: ", x1
                end if
                lastx1 = x1
            end do
        end if
    end if

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
