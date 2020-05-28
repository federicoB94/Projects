module others
    use debug
    implicit none

contains

    function idx_from_config(config) result(idx)
    !
    ! Returns the integer representation of a spin configuration.
    ! Each configuration can be represented in the 2**N space as
    ! a vector of 0, and one 1 in position 'idx'.
    !
    !   Parameters
    !   ----------
    !   config : INTEGER, DIMENSION(N)
    !       Spin configuration (array with +-1)
    !
    !   Returns
    !   -------
    !   idx : INTEGER
    !       Integer representation of config in the 2**N vector.
    !       It corresponds to the index of the element 1 in that array.
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: config(:)
        ! return
        INTEGER :: idx

        ! local variables
        INTEGER :: ii, N

        N = SIZE(config)
        idx = 0
        do ii = 1, N
            if ( config(ii) == 1 ) idx = idx + 2**(N-ii)
        end do

        RETURN

    end function idx_from_config

    function config_from_idx(idx, N) result(config)
    !
    ! Returns the spin configuration given the integer representation.
    ! It is the inverse of 'idx_from_config'
    !
    !   Parameters
    !   ----------
    !   idx : INTEGER
    !       Integer representation of config in the 2**N vector.
    !       It corresponds to the index of the element 1 in that array.
    !   N : INTEGER
    !       Total number of spis in the configuration.
    !
    !   Returns
    !   -------
    !   config : INTEGER, DIMENSION(N)
    !       Spin configuration (array with +-1)
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: idx, N
        ! return
        INTEGER, ALLOCATABLE :: config(:)

        ! local variables
        INTEGER :: ii
        INTEGER, ALLOCATABLE :: bit(:)

        ! checks on inputs
        if ( N < 1 ) then
            print*, "[ABORT] 'N' must be > 1"
            print*, "Found = ", N
        end if

        if ( idx < 0 .OR. idx > 2**N-1 ) then
            print*, "[ABORT] 'idx' must be >= 0 and < 2**N"
            print*, "Found       = ", idx
            print*, "Max allowed = ", 2**N-1
            CALL ABORT()
        end if

        if ( ALLOCATED(config) ) DEALLOCATE(config)
        ALLOCATE(config(N))
        config = -1

        ! get the base-2 value of idx
        bit = int2bit(idx)
        do ii = 1, size(bit)
            if ( bit(ii) == 1 ) config(ii) = 1
        end do
        ! need to reverse config vector
        config = config(N:1:-1)

        RETURN

    end function config_from_idx

    function int2bit(Number) result(bit)
    !
    ! Returns the binary representation of the integer as an array.
    ! First element of the array is the LSB.
    !
    !   Parameters
    !   ----------
    !   Number : INTEGER
    !       Integer to convert. Must be >= 0.
    !
    !   Return
    !   ------
    !   bit : INTEGER, DIMENSION(INT(log2(N) + 1))
    !       Bit representation of N. First element is the LSB.
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: Number
        ! return
        INTEGER, ALLOCATABLE :: bit(:)

        ! local variables
        INTEGER :: N, nbits, ii

        ! checks on N
        if ( N < 0 ) then
            print*, "[ABORT] 'N' must be >= 0"
            print*, "Found = ", N
            CALL ABORT()
        end if

        N = Number

        if ( N == 0 ) then
            nbits = 1
        else
            nbits = INT(LOG(REAL(N)) / LOG(2.) + 1)
        end if

        ALLOCATE(bit(nbits))

        do ii = 1, nbits
            bit(ii) = MOD(N, 2)
            N = N / 2
        end do

        RETURN

    end function int2bit

    function logPsiDiff(Si2, Si1, Ai, Bj, Wij) result(logPsi)
    !
    ! Returns the log of the ratio between the
    ! Network Quantum States described by spin configurations
    ! Si2 and Si1:
    !
    ! log( Psi(Si2) / Psi(Si1) ) = log( Psi(Si2) ) - log( Psi(Si1) )
    !
    !   Parameters
    !   ----------
    !   Si2 : INTEGER, DIMENSION(N)
    !       Final spin configuration (+- 1 array)
    !   Si1 : INTEGER, DIMENSION(N)
    !       Initial spin configuration (+- 1 array)
    !   Ai : DOUBLE COMPLEX, DIMENSION(N)
    !       Biases of the visible units
    !   Bj : DOUBLE COMPLEX, DIMENSION(M)
    !       Biases of the hidden units
    !   Wij : DOUBLE COMPLEX, DIMENSION(N,M)
    !       Weights between visible and hidden units
    !
    !   Return
    !   ------
    !   logPsi : DOUBLE COMPLEX
    !       log( Psi(Si2) ) - log( Psi(Si1) )
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: Si2(:), Si1(:)
        DOUBLE COMPLEX, INTENT(IN) :: Ai(:), Bj(:), Wij(:,:)
        ! return
        DOUBLE COMPLEX :: logPsi

        ! local variables
        INTEGER :: M
        DOUBLE COMPLEX, ALLOCATABLE :: Tj2(:), Tj1(:)

        ! checks on Si
        if ( SIZE(Si2) /= SIZE(Si1) ) then
            print*, "[ABORT] Wrong dimensions between spin configurations"
            print*, "'Si2' has shape = ", SIZE(Si2)
            print*, "'Si1' has shape = ", SIZE(Si1)
            print*, "Expected equal shape"
            CALL ABORT()
        end if

        ! checks on RBM input dimensions
        if ( SIZE(Wij, 1) /= SIZE(Ai) .OR. SIZE(Wij, 2) /= SIZE(Bj) ) then
            print*, "[ABORT] Wrong shapes in RBM parameters"
            print*, "Visible units dimension = ", SIZE(Ai)
            print*, "Hidden  units dimension = ", SIZE(Bj)
            print*, "Weights dimensions      = ", SIZE(Wij,1), ", ", SIZE(Wij,2)
            print*, "(expected = ", SIZE(Ai), ", ", SIZE(Bj), ")"
            CALL ABORT()
        end if

        M = SIZE(Bj)
        ALLOCATE(Tj2(M))
        ALLOCATE(Tj1(M))

        Tj2 = Bj + MATMUL(TRANSPOSE(Wij), Si2)
        Tj1 = Bj + MATMUL(TRANSPOSE(Wij), Si1)

        ! log( Psi(Si2) ) - log( Psi(Si1) )
        logPsi = SUM(Ai*(Si2-Si1)) + SUM(LOG(COSH(Tj2))) - SUM(LOG(COSH(Tj1)))

        DEALLOCATE(Tj2)
        DEALLOCATE(Tj1)

        RETURN

    end function logPsiDiff

    subroutine Inverse(A)
    !
    ! Computes the inverse of the matrix using LAPACK subroutine ZGETRF().
    !
    !   Parameters
    !   ----------
    !
    !   [INOUT] A : DOUBLE COMPLEX, DIMENSION(N,N)
    !       Square matrix to be inverted.
    !
        implicit none
        ! parameters
        DOUBLE COMPLEX, INTENT(INOUT) :: A(:,:)

        ! local variables
        INTEGER :: N, info, Lwork
        INTEGER, ALLOCATABLE :: ipiv(:,:)
        DOUBLE COMPLEX, ALLOCATABLE :: work(:)

        ! check that A is square matrix
        if ( SIZE(A, 1) /= SIZE(A,2) ) then
            print*, "[ABORT] Matrix is not squared, cannot be inverted"
            CALL ABORT()
        end if

        N = SIZE(A, 1)
        ALLOCATE(ipiv(N,N))

        ! call LAPACK zgtrf() to get LU decomposition
        CALL ZGETRF(N, N, A, N, ipiv, info)
        ! check exit
        if ( info < 0 ) then
            print*, "[ABORT] Illegal value found in LU decomposition"
            CALL ABORT()
        end if

        ! call LAPACK zgetri() to get optimal value of Lwork
        ALLOCATE(work(1))
        Lwork = -1
        CALL ZGETRI(N, A, N, ipiv, work, Lwork, info)
        Lwork = INT(work(1))
        DEALLOCATE(work)
        ALLOCATE(work(Lwork))
        ! call LAPACK zgetri() to get the inverse
        CALL ZGETRI(N, A, N, ipiv, work, Lwork, info)
        if ( info < 0 ) then
            print*, "[ABORT] Illegal argument value"
            CALL ABORT()
        else if ( info > 0 ) then
            print*, "[ABORT] Matrix is singular"
            CALL ABORT()
        end if

        RETURN

    end subroutine Inverse

end module others