module factors
    implicit none
    
    contains

    ! Factoring function
    function factoring(a) result(factors)
        integer, intent(in) :: a
        integer, allocatable :: factors(:)
        integer :: i, count, local
        logical :: neg

        ! I might be dealing with negative numbers
        neg = .false.
        local = a

        count = 0

        ! If the number is 0, return - why are we even here?
        if (local == 0) then
            return
        end if

        ! If the number is negative, make it positive
        ! and remember that it was negative
        ! Negative numbers have the same factors as positive numbers
        ! Negative numbers cause the Do loop to SEGFAULT
        if (local < 0) then
            neg = .true.
            local = -a
        end if

        if (local == 1) then
            count = 1
            allocate(factors(count))
            factors(count) = 1
            if (neg) then
                factors(count) = -1
                return
            end if
            
        end if

        do i = 1, local
            if (mod(local, i) == 0) then
                count = count + 1
                if (.not.allocated(factors)) then
                    allocate(factors(count))
                else
                    call resizearray(factors, count)
                end if
                if (neg) then
                    factors(count) = -i
                else
                    factors(count) = i
                end if
                ! print *, "This is :", i
            end if
        end do
    end function factoring

    ! Resize array
    subroutine resizearray(array, newsize)
        integer, allocatable :: array(:)
        integer, intent(in) :: newsize
        integer, allocatable :: temp(:)

        allocate(temp(newsize))
        if (allocated(array)) then
            temp(1:size(array)) = array
            deallocate(array)
        end if
        array = temp

        deallocate(temp)
    end subroutine resizearray

end module factors