module rbm
    use random
    use others
    use debug
    implicit none

contains

    subroutine RBM_init(N, M, Ai, Bj, Wij, mean, std)
    !
    ! Randomly initializes the weights of the RBM,
    ! with normal distribution
    !
    !   Parameters
    !   ----------
    !   [IN] N : INTEGER
    !       number of visible units
    !   [IN] M : INTEGER
    !       number of hidden units
    !   [OUT] Ai : DOUBLE COMPLEX, DIMENSION(N)
    !       Biases of the visible units; the subroutine allocates the space.
    !   [OUT] Bj : DOUBLE COMPLEX, DIMENSION(M)
    !       Biases of the hidden units; the subroutine allocates the space.
    !   [OUT] Wij : DOUBLE COMPLEX, DIMENSION(N, M)
    !       Weights between visible and hidden units; the subroutine allocates the space.
    !   [IN] mean : DOUBLE PRECISION
    !       Mean of the normal distribution
    !   [IN] std : DOUBLE PRECISION
    !       Standard deviation of the normal distribution
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: N, M
        DOUBLE PRECISION, INTENT(IN) :: mean, std
        DOUBLE COMPLEX, ALLOCATABLE, INTENT(OUT) :: Ai(:), Bj(:), Wij(:,:)
        ! local variables
        DOUBLE COMPLEX, ALLOCATABLE :: r(:)

        if ( ALLOCATED(Ai)  ) DEALLOCATE(Ai)
        if ( ALLOCATED(Bj)  ) DEALLOCATE(Bj)
        if ( ALLOCATED(Wij) ) DEALLOCATE(Wij)

        ALLOCATE( Ai(N) )
        ALLOCATE( Bj(M) )
        ALLOCATE( Wij(N,M))

        ! check on std
        if ( std < 0.d0 ) then
            print*, "[WARNING] 'std' is negative, using the absolute value"
        end if

        Ai = CMPLX(Random_Normal(N, mean, ABS(std)), &
                   Random_Normal(N, mean, ABS(std)), KIND=8)

        Bj = CMPLX(Random_Normal(M, mean, ABS(std)), &
                   Random_Normal(M, mean, ABS(std)), KIND=8)

        ALLOCATE(r(N*M))

        r = CMPLX(Random_Normal(N*M, mean, ABS(std)), &
                  Random_Normal(N*M, mean, ABS(std)), KIND=8)
        Wij = RESHAPE( r, (/N,M/) )

        DEALLOCATE(r)

        RETURN

    end subroutine RBM_init

    subroutine RBM_update(Ai, Bj, Wij, S_kk, Fk, g)
    !
    ! Update the weights of the RBM.
    !
    !   Parameters
    !   ----------
    !   [INOUT] Ai : DOUBLE COMPLEX, DIMENSION(N)
    !       Biases of the visible units
    !   [INOUT] Bj : DOUBLE COMPLEX, DIMENSION(M)
    !       Biases of the hidden units
    !   [INOUT] Wij : DOUBLE COMPLEX, DIMENSION(N,M)
    !       Weights between visible and hidden units
    !   [INOUT] S_kk : DOUBLE COMPLEX, DIMENSION(k,k)
    !       Regularized covariance matrix.
    !       k = N + M + N*M (# of network weights)
    !   [IN] Fk : DOUBLE COMPLEX, DIMENSION(k)
    !       Forces array.
    !       k = N + M + N*M (# of network weights)
    !   [IN] g : DOUBLE PRECISION
    !       Learning rate.
    !

        implicit none
        ! parameters
        DOUBLE COMPLEX, INTENT(INOUT) :: Ai(:), Bj(:), Wij(:,:), S_kk(:,:)
        DOUBLE COMPLEX, INTENT(IN)    :: Fk(:)
        DOUBLE PRECISION, INTENT(IN)  :: g

        ! local variables
        INTEGER :: N, M, k
        DOUBLE COMPLEX, ALLOCATABLE :: SF(:)

        N = SIZE(Ai) ! # visible units
        M = SIZE(Bj) ! # hidden units

        ! checks on RBM input dimensions
        if ( SIZE(Wij, 1) /= SIZE(Ai) .OR. SIZE(Wij, 2) /= SIZE(Bj) ) then
            print*, "[ABORT] Wrong shapes in RBM parameters"
            print*, "Visible units dimension = ", SIZE(Ai)
            print*, "Hidden  units dimension = ", SIZE(Bj)
            print*, "Weights dimensions      = ", SIZE(Wij,1), ", ", SIZE(Wij,2)
            print*, "(expected = ", SIZE(Ai), ", ", SIZE(Bj), ")"
            CALL ABORT()
        end if

        k = N+M+N*M

        ! checks on covariance matrix S_kk
        if ( SIZE(S_kk, 1) /= k .OR. SIZE(S_kk, 2) /= k ) then
            print*, "[ABORT] Wrong shape in covariance matrix"
            print*, "Found    = ", SIZE(S_kk, 1), ", ", SIZE(S_kk, 2)
            print*, "Expected = ", k, ", ", k
            CALL ABORT()
        end if

        ALLOCATE(SF(k))

        CALL Inverse(S_kk)
        S_kk = MERGE(S_kk, CMPLX(0.d0, 0.d0, KIND=8), ABS(S_kk)>1d-09)

        CALL debugging(.FALSE., var=SIZE(S_kk, dim=2), message="S_kk")
        CALL debugging(.FALSE., var=SIZE(Fk), message="Fk")

        SF = g*MATMUL(S_kk, Fk)
        SF = MERGE(SF, CMPLX(0.d0, 0.d0, KIND=8), ABS(SF)>1d-09)

        CALL debugging(.FALSE., var=SUM(SF)/SIZE(SF), message="Weigths update")
        ! update weights
        Ai = Ai - SF(:N)
        Bj = Bj - SF(N+1:M)
        Wij = Wij - TRANSPOSE(RESHAPE(SF(N+M+1:), (/M,N/)))

        DEALLOCATE(SF)

        RETURN

    end subroutine RBM_update

    function Skk(Opk, iter) result(S_kk)
    !
    ! Returns the covariance matrix given in (A4).
    ! Explicit regularization is applied as described in Appendix A.
    !
    !   Parameters
    !   ----------
    !   Opk : DOUBLE COMPLEX, DIMENSION(p,k)
    !       Matrix with the derivatives of the RBM wave function
    !       wrt to the network parameters.
    !       Each row represents a different realization.
    !       p = # of different realizations
    !       k = N + M + N*M (# of network weights)
    !   iter : INTEGER
    !       Weights update iteration. It is needed to apply the regularization.
    !
    !   Return
    !   ------
    !   S_kk : DOUBLE COMPLEX, DIMENSION(k,k)
    !       Regularized covariance matrix.
    !
        implicit none
        ! parameters
        DOUBLE COMPLEX, INTENT(IN) :: Opk(:,:)
        INTEGER, INTENT(IN) :: iter
        ! return
        DOUBLE COMPLEX, ALLOCATABLE :: S_kk(:,:)

        ! local variables
        INTEGER :: k, p, ii, jj, mm
        DOUBLE PRECISION :: lp
        DOUBLE COMPLEX, ALLOCATABLE :: Ok_mean(:)
        ! constant
        DOUBLE PRECISION, PARAMETER :: l0   = 100.d0, &
                                       b    = 0.9d0,  &
                                       lmin = 1d-04

        ! get the dimension of Opk
        p = SIZE(Opk, DIM=1) ! number of realizations
        k = SIZE(Opk, DIM=2) ! N+M+N*M

        if ( ALLOCATED(S_kk) ) DEALLOCATE(S_kk)
        ALLOCATE( S_kk(k,k) )

        ! < Ok* Ok' >
        S_kk = CMPLX(0.d0, 0.d0, KIND=8)
        ! loop over rows of Opk
        do ii = 1, p
            ! loop over elements of a row
            do jj = 1, k
                ! loop over elements of a row
                do mm = 1, k
                    S_kk(jj,mm) = S_kk(jj,mm) + CONJG(Opk(ii,jj))*Opk(ii,mm)
                end do
            end do
        end do
        S_kk = S_kk / p

        ! < Ok >
        ALLOCATE(Ok_mean(k))
        do ii = 1, k
            Ok_mean(ii) = SUM(Opk(:, ii)) / p
        end do

        ! < Ok* Ok' > - < Ok* > < Ok' >
        lp = MAX(l0*b**iter, lmin)
        do ii = 1, k
            do jj = 1, k
                S_kk(ii,jj) = S_kk(ii,jj) - CONJG(Ok_mean(ii))*Ok_mean(jj)
                ! regularization on diagonal terms
                if ( ii == jj ) then
                    S_kk(ii,ii) = S_kk(ii,ii) + lp
                end if
            end do
        end do

        DEALLOCATE(Ok_mean)

        S_kk = MERGE(S_kk, CMPLX(0.d0, 0.d0, KIND=8), ABS(S_kk)>1d-09)

        RETURN

    end function Skk

    function Forces(Opk, Eloc) result(Fk)
    !
    ! Returns the forces as in (A5).
    !
    !   Parameters
    !   ----------
    !   Opk : DOUBLE COMPLEX, DIMENSION(p,k)
    !       Matrix with the derivatives of the RBM wave function
    !       wrt to the network parameters.
    !       Each row represents a different realization.
    !       p = # of different realizations
    !       k = N + M + N*M (# of network weights)
    !   Eloc : DOUBLE COMPLEX, DIMENSION(p)
    !       Array with the local energies of the MC spin configurations
    !
    !   Return
    !   ------
    !   Fk : DOUBLE COMPLEX, DIMENSION(k)
    !       Forces array
    !
        implicit none
        ! parameters
        DOUBLE COMPLEX, INTENT(IN) :: Opk(:,:), Eloc(:)
        ! return
        DOUBLE COMPLEX, ALLOCATABLE :: Fk(:)

        ! local variables
        INTEGER :: p, k, ii
        DOUBLE COMPLEX, ALLOCATABLE :: El_Okstar(:)

        p = SIZE(Opk, 1)
        k = SIZE(Opk, 2)

        ! checks on inputs dimensions
        if ( SIZE(Eloc) /= p ) then
            print*, "[ABORT] Wrong dimensions in input"
            print*, "'Opk'  dimension = ", p, ", ", k
            print*, "'Eloc' dimension = ", SIZE(Eloc)
            print*, "(expected = ", p, ")"
            CALL ABORT()
        end if

        CALL debugging(.FALSE., var=k, message="Forces - k")

        if ( ALLOCATED(Fk) ) DEALLOCATE(Fk)
        ALLOCATE(Fk(k))
        ALLOCATE(El_Okstar(k))

        ! < Eloc Ok* >
        do ii = 1, k
            El_Okstar(ii) = DOT_PRODUCT(Eloc, CONJG(Opk(:,ii)))
        end do
        El_Okstar = El_Okstar / p

        CALL debugging(.FALSE., var=El_Okstar, message="< Eloc Ok* >")
        CALL debugging(.FALSE., var=SUM(Eloc)/p * SUM(CONJG(Opk), DIM=1)/p, message="< Eloc > < Ok* >")

        ! < Eloc Ok* > - < Eloc > < Ok* >
        Fk = El_Okstar - SUM(Eloc)/p * SUM(CONJG(Opk), DIM=1)/p

        CALL debugging(.FALSE., var=SIZE(Fk), message="Forces - Fk size - end")

        DEALLOCATE(El_Okstar)

        Fk = MERGE(Fk, CMPLX(0.d0, 0.d0, KIND=8), ABS(Fk)>1d-09)

        RETURN

    end function Forces

    function LocalEnergy(config, H, Ai, Bj, Wij) result(Eloc)
    !
    ! Returns the array with local energy for each spin
    ! configuration sampled from the Metropolis.
    !
    !   Parameters
    !   ----------
    !   config : INTEGER, DIMENSION(p,N)
    !       Matrix in which each row corresponds to a configuration
    !       sapled by Metropolis.
    !       p = # of configurations
    !       N = # number of spins (eg. visible units)
    !   H : DOUBLE COMPLEX, DIMENSION(2**N, 2**N)
    !       Matrix describing the hamiltonian of the system
    !   Ai : DOUBLE COMPLEX, DIMENSION(N)
    !       Biases of visible units
    !   Bj : DOUBLE COMPLEX, DIMENSION(M)
    !       Biases of hidden units
    !   Wij : DOUBLE COMPLEX, DIMENSION(N,M)
    !       Weights between visible and hidden units
    !
    !   Return
    !   ------
    !   Eloc : DOUBLE COMPLEX, DIMENSION(p)
    !       Array with local energy values for each configuration
    !
        implicit none
        ! parameters
        INTEGER, INTENT(IN) :: config(:,:)
        DOUBLE COMPLEX, INTENT(IN) :: H(:,:), Ai(:), Bj(:), Wij(:,:)
        ! return
        DOUBLE COMPLEX, ALLOCATABLE :: Eloc(:)

        ! local variables
        INTEGER :: p, N, ii, jj, idx
        INTEGER, ALLOCATABLE :: S1(:), S2(:)
        DOUBLE COMPLEX :: Hij

        p = SIZE(config, 1) ! # of configurations
        N = SIZE(config, 2) ! # of spins

        ! checks on hamiltonian dimension
        if ( SIZE(H,1) /= 2**N .OR. SIZE(H,2) /= 2**N ) then
            print*, "[ABORT] Wrong shape in hamiltonian"
            print*, "Found    = ", SIZE(H,1), ", ", SIZE(H,2)
            print*, "Expected = ", 2**N, ", ", 2**N
            CALL ABORT()
        end if

        ! checks on visible units
        if ( SIZE(Ai) /= N ) then
            print*, "[ABORT] Wrong dimension in visible units"
            print*, "Found    = ", SIZE(Ai)
            print*, "Expected = ", N
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

        if ( ALLOCATED(Eloc) ) DEALLOCATE(Eloc)
        ALLOCATE(Eloc(p))
        Eloc = CMPLX(0.d0, 0.d0, KIND=8)
        CALL debugging(.FALSE., var=SUM(Eloc), message="+++ Eloc sum")

        ALLOCATE(S1(N))
        ALLOCATE(S2(N))

        CALL debugging(.FALSE., message="+++ before loop")
        ! loop over MC configurations
        do ii = 1, p
            CALL debugging(.FALSE., var=ii, message="+++Outer loop")
            ! get S
            S1 = config(ii,:)
            ! get integer representation of spin configuration
            idx = idx_from_config(S1)
            CALL debugging(.FALSE., var=idx, message="integer S1")
            ! loop over all S'
            do jj = 1, 2**N
                ! need to pass idx+1 since Fortran starts from 1,
                ! while the integer representation from 0
                Hij = H(idx+1,jj) ! < S | H | S' >
                CALL debugging(.FALSE., var=jj, message="+++Inner loop")
                if ( ABS(Hij) > 1d-09 ) then ! if ( Hij /= 0 ) then
                    ! need to pass jj-1 since Fortran starts from 1,
                    ! while the integer representation from 0
                    S2 = config_from_idx(jj-1,N)
                    Eloc(ii) = Eloc(ii) + Hij*EXP(logPsiDiff(S2, S1, Ai, Bj, Wij))
                end if
            end do
            CALL debugging(.FALSE., var=Eloc(ii), message="+++ Eloc sum")
        end do
        CALL debugging(.FALSE., message="+++ after loop")

        DEALLOCATE(S1)
        DEALLOCATE(S2)

        RETURN

    end function LocalEnergy

end module rbm