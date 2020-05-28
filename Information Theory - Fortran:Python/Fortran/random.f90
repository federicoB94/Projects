module random
    implicit none
    DOUBLE PRECISION, PARAMETER :: pi = 4.d0*DATAN(1.d0)

contains

    function Random_Normal(N, mean, std) result(Normal)
    !
    ! Retruns an array of dimension N with normal distributed numbers
    !
    !   Parameters
    !   ----------
    !   N : INTEGER
    !       Dimension of the returned array
    !   mean : DOUBLE PRECISION
    !       Mean of the distribution
    !   std : DOUBLE COMPLEX
    !       Standard deviation of the distribution
    !
    !   Return
    !   ------
    !   Normal : DOUBLE PRECISION, DIMENSION(N)
    !       Array with N normal random number
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: N
        DOUBLE PRECISION, INTENT(IN) :: mean, std
        ! returns
        DOUBLE PRECISION, ALLOCATABLE :: Normal(:)

        ! local variables
        DOUBLE PRECISION, ALLOCATABLE :: u1(:), u2(:)

        if ( std < 0.d0 ) then
            print*, "[WARNING] 'std' is negative, using the absolute value"
        end if

        if ( ALLOCATED(Normal) ) DEALLOCATE(Normal)
        ALLOCATE(Normal(N))

        ALLOCATE(u1(N))
        ALLOCATE(u2(N))

        CALL RANDOM_NUMBER(u1)
        CALL RANDOM_NUMBER(u2)

        !Box Muller
        Normal = SQRT(-2.d0*DLOG(u1))*DCOS(2.d0*pi*u2)
        Normal = mean + ABS(std)*Normal

        DEALLOCATE(u1)
        DEALLOCATE(u2)

        RETURN

    end function Random_Normal

    function Random_Integer(low, high) result(randint)
        !
        ! Returns a random integer in the range [low, high)
        !
        !   Parameters
        !   ----------
        !   low : INTEGER
        !       Lowest (signed) integer to be drawn from the distribution
        !   high : INTEGER
        !       One above the largest (signed) integer to be drawn from the distribution
        !
        !   Returns
        !   -------
        !   randint : INTEGER
        !       Random integer in [low, high)
        !
            implicit none
            ! parameters
            INTEGER, INTENT(IN) :: low, high
            ! returns
            INTEGER :: randint

            ! local variables
            REAL :: r

            ! check if low < high
            if ( low > high ) then
                print*, "[ABORT] 'low' must be <= 'high'"
                CALL ABORT()
            end if

            CALL RANDOM_NUMBER(r)
            randint = low + FLOOR((high-low)*r)

            RETURN

        end function Random_Integer

    function Random_Configuration(N) result(config)
        !
        ! Generates a random configuration of spins +/- 1
        !
        !   Parameters
        !   ----------
        !   N : INTEGER
        !       Number of spins in the configuration
        !
        !   Returns
        !   -------
        !   config : INTEGER, DIMENSION(N)
        !       Array where to store the configuration.
        !

            implicit none
            ! parameters
            INTEGER, INTENT(IN) :: N
            ! returns
            INTEGER, ALLOCATABLE :: config(:)

            ! local variables
            REAL, ALLOCATABLE :: r(:)

            if ( ALLOCATED(config) ) DEALLOCATE(config)
            ALLOCATE(config(N))

            ALLOCATE(r(N))
            CALL RANDOM_NUMBER(r)

            config = -1 +2*FLOOR(r*2.)

            RETURN

        end function Random_Configuration

end module random
