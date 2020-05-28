program main
    use simulation
    use rbm
    use Lanczos_mod
    use hamiltonian
    use debug

    implicit none

    ! Ising
    DOUBLE PRECISION :: ll
    DOUBLE COMPLEX, ALLOCATABLE :: H(:,:)
    ! RBM parameters
    INTEGER :: N, M, alpha, iterations
    DOUBLE PRECISION :: g
    DOUBLE COMPLEX, ALLOCATABLE :: Ai(:), Bj(:), Wij(:,:)
    ! Metropolis parameters
    INTEGER :: iter, burnin, autocorr
    ! auxiliary variables
    INTEGER, ALLOCATABLE :: config(:,:)
    DOUBLE PRECISION, ALLOCATABLE :: Lancz(:), GS_Energy(:)
    DOUBLE COMPLEX, ALLOCATABLE :: Opk(:,:), S_kk(:,:), Eloc(:), Fk(:)
    ! iterators
    INTEGER :: ii, info
    CHARACTER(len=100) :: out
    ! command line arguments
    CHARACTER(len=32) :: arg

    ! check if correct number of command line arguments
    if ( COMMAND_ARGUMENT_COUNT() /= 11 ) then
        print*, "[ABORT] Wrong number of command line arguments."
        print*, "Found: ", COMMAND_ARGUMENT_COUNT()
        print*, "Expected: 11"
        print*, "Please use the Python interface 'run.py'"
        print*, "to configure the parameters."
        CALL ABORT()
    end if

    ! get command line arguments
    CALL GET_COMMAND_ARGUMENT( 1, arg)
    READ(arg,*) N
    CALL GET_COMMAND_ARGUMENT( 2, arg)
    READ(arg,*) ll
    CALL GET_COMMAND_ARGUMENT( 3, arg)
    if ( TRIM(arg) == "0" ) then
        H = Ising_1D(N, ll)
    else
        H = Ising_2D(INT(SQRT(REAL(N))), ll)
    end if
    CALL GET_COMMAND_ARGUMENT( 4, arg)
    READ(arg,*) alpha
    M = alpha*N
    CALL GET_COMMAND_ARGUMENT( 5, arg)
    READ(arg,*) g
    CALL GET_COMMAND_ARGUMENT( 6, arg)
    READ(arg,*) iterations
    CALL GET_COMMAND_ARGUMENT( 7, arg)
    READ(arg,*) iter
    CALL GET_COMMAND_ARGUMENT( 8, arg)
    READ(arg,*) burnin
    CALL GET_COMMAND_ARGUMENT( 9, arg)
    READ(arg,*) autocorr
    CALL GET_COMMAND_ARGUMENT(10, arg)
    READ(arg,*) info
    CALL GET_COMMAND_ARGUMENT(11, arg)
    READ(arg,*) out

    CALL debugging(.FALSE., var=H, message="Ising")

    ! Lanczos
    Lancz = Lanczos(H, N, 200)
    CALL debugging(.TRUE., var=Lancz(1), message="Ground State (Lanczos)")
    OPEN(15, file="tmp.txt")
    WRITE(15,*) Lancz(1)
    CLOSE(15)

    ! initializa the RBM
    CALL RBM_init(N, M, Ai, Bj, Wij, 0.0d0, 0.01d0)
    CALL debugging(.FALSE., var=Wij, message="Wij")

    ALLOCATE(GS_Energy(iterations))

    do ii = 1, iterations
        CALL debugging(.FALSE., message="Before Metropolis")
        ! Metropolis simulation
        CALL Metropolis(Ai, Bj, Wij, iter, burnin, autocorr, config, Opk)
        CALL debugging(.FALSE., var=config, message="config")
        CALL debugging(.FALSE., var=COUNT(config /= -1 .OR. config /= 1), message="config errors")
        ! covariance matrix
        S_kk = Skk(Opk, ii)
        CALL debugging(.FALSE., var=SUM(ABS(S_kk))/SIZE(S_kk), message="Skk")
        CALL debugging(.FALSE., message="Before Eloc")
        ! local energy
        Eloc = LocalEnergy(config, H, Ai, Bj, Wij)
        CALL debugging(.FALSE., var=SUM(Eloc),message="Eloc sum")
        ! Forces
        Fk = Forces(Opk, Eloc)
        CALL debugging(.FALSE., var=SUM(ABS(Fk))/SIZE(Fk), message="Fk")
        CALL debugging(.FALSE., message="Before update")
        ! update weights
        CALL debugging(.FALSE., var=SUM(ABS(S_kk)) / SIZE(S_kk), message="Skk")
        CALL RBM_update(Ai, Bj, Wij, S_kk, Fk, g)

        GS_Energy(ii) = DREAL(SUM(Eloc) / SIZE(Eloc))

        if ( MOD(ii, info) == 0 ) then
            print*, ii, " | ",  DREAL(SUM(Eloc) / SIZE(Eloc))
        end if

        DEALLOCATE(S_kk)
        DEALLOCATE(Fk)
        DEALLOCATE(Eloc)
        DEALLOCATE(config)
        DEALLOCATE(Opk)
    end do

    CALL debug_print(GS_Energy, TRIM(out))

end program main
