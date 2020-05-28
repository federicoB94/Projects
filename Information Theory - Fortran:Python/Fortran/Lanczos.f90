module Lanczos_mod
    use random
    implicit none

contains

    function Lanczos(H, N, n_iter) result(W)
        !
        ! Compute the eighenvalues of a given hamiltonian
        ! using the Lanczos algorithm
        !
        !   Parameters
        !   ----------
        !   H  : DOUBLE COMPLEX, DIMENSION(2**N, 2**N)
        !       Matrix representation of the Hamiltonian
        !   N  :  INTEGER
        !       number of particles in the system
        !   n_iter  : INTEGER
        !       number of iter taken by the algorithm
        !
        !   Return
        !   ------
        !   W : DOUBLE PRECISION
        !       Eigenvalues in ascending order.

        implicit none

        ! Parameters
        DOUBLE COMPLEX , DIMENSION(:,:) :: H
        INTEGER :: n_iter, N

        ! Return
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: W

        ! Local variable
        INTEGER :: M, LWORK, LIWORK, INFO, i, LDZ, NZC, IL, IU
        DOUBLE PRECISION :: VL, VU
        DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: v_j_last, v_j, w_j_, w_j, WORK
        DOUBLE COMPLEX:: alpha, beta
        DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE:: alpha_list, beta_list, D, E
        DOUBLE COMPLEX  , DIMENSION(:,:), ALLOCATABLE :: Z
        INTEGER, DIMENSION(:), ALLOCATABLE:: ISUPPZ, IWORK
        LOGICAL:: TRYRAC

        !For the Lanczos algorithm we have to build a tridiagonal matrix.
        !The diagonal is represented by alpha_list while the off-diagonal is
        !represented by beta_list. Here beta_list is of the same dimension of
        !alpha list because the subroutine for the diagonalizarion require
        !that specific but in practice beta_list has n_iter - 1 elements.
        ALLOCATE(alpha_list(n_iter))
        ALLOCATE(beta_list (n_iter))

        ALLOCATE(v_j(2**N))
        ALLOCATE(v_j_last(2**N))

        !inizialize a random vector in the Hilbert Space of dimension 2**N,
        !the same dimension of the Hamiltonian, and normalize it
        call random_number(v_j)
        v_j = v_j / norm2(v_j)


        ALLOCATE(w_j_(2**N))
        ALLOCATE(w_j (2**N))

        w_j_ = matmul(H,v_j)

        alpha= dot_product(w_j_,v_j)
        !save the value of alpha
        alpha_list(1) = alpha
        w_j = w_j_ - alpha*v_j

        !Iterative procedure of Lanczos
        do i=2,n_iter
            beta= norm2(w_j)
            !save beta
            beta_list(i-1) = beta

            if (abs(beta) < 1d-12) then
                print *,"ERROR: beta=0"
                exit
            end if

            v_j_last= v_j
            v_j = w_j / beta

            w_j_= matmul(H,v_j)
            alpha=dot_product(w_j_,v_j)
            alpha_list(i) = alpha !save alpha

            w_j = w_j_ - alpha*v_j - beta* v_j_last

        end do


        !Copy the diagonal and the off diagonal. We pass the copy to the
        !subroutine beacuse D and E are overwrited and this first call of
        ! the diagonalization subroutine id for setup the parameters.
        ALLOCATE(D(n_iter))
        ALLOCATE(E(n_iter))

        D  = alpha_list
        E  = beta_list
        VL = 0.d0
        VU = 0.d0
        IL = 0
        IU = 0
        ALLOCATE(W(n_iter))
        M = 1
        ALLOCATE(Z(LDZ,max(1,M)))
        LDZ = n_iter
        NZC = -1
        ALLOCATE(ISUPPZ(2*max(1,M)))
        TRYRAC = .TRUE.
        ALLOCATE(WORK(1))
        LWORK = -1
        ALLOCATE(IWORK(1))
        LIWORK = -1

        !Call subroutine with NCZ=-1,LWORK=-1 and LIWORK =-1 to get the
        !values that optimize the subroutine itself
        call zstemr ('N', 'A', n_iter, D, E, VL, VU, IL, IU, M, W, Z, LDZ, NZC,&
                      ISUPPZ, TRYRAC, WORK, LWORK, IWORK, LIWORK, INFO)

        !We print INFO. If INFO = 0 we have that the subroutine was called
        ! without any problem
        if ( info /= 0 ) then
            print*, "[ABORT] First call to ZSTMR exit without success"
            call abort()
        end if

        M   = INT(Z(1,1))
        NZC = n_iter
        LWORK  = WORK(1)
        LIWORK = IWORK(1)

        DEALLOCATE(WORK)
        DEALLOCATE(IWORK)
        DEALLOCATE(Z)
        DEALLOCATE(ISUPPZ)

        ALLOCATE(WORK(LWORK))
        ALLOCATE(IWORK(LIWORK))
        ALLOCATE(Z(LDZ,max(1,M)))
        ALLOCATE(ISUPPZ(2*max(1,M)))

        TRYRAC=.TRUE.

        !Second call of the subroutine for the diagonalization with the setup
        !obtained from the previous call. We have that W is overwrited with
        !the eigenvalues of the hamiltonian.
        call zstemr ('N', 'A', n_iter, D, E, VL, VU, IL, IU, M, W, Z, LDZ, NZC,&
                      ISUPPZ, TRYRAC, WORK, LWORK, IWORK, LIWORK, INFO)

        if ( info /= 0 ) then
            print*, "[ABORT] Second call to ZSTMR exit without success"
            call abort()
        end if

        RETURN

    end function Lanczos


end module Lanczos_mod