module hamiltonian
    implicit none

    ! Pauli matrices
    DOUBLE COMPLEX, DIMENSION(2,2), PARAMETER :: &
        Sx = RESHAPE((/0.d0, 1.d0, &
                       1.d0, 0.d0/), (/2,2/))
    DOUBLE COMPLEX, DIMENSION(2,2), PARAMETER :: &
        Sz = RESHAPE((/1.d0, 0.d0, &
                       0.d0,-1.d0/), (/2,2/))

    interface TensorProduct
        module procedure TensorProduct2mat
    end interface

contains

    function Ising_1D(N, ll) result(H)
    !
    ! Computes the Ising Hamiltonian for N 1/2 spin particles
    ! on 1D lattice.
    !
    !   Parameters
    !   ----------
    !   N  : INTEGER
    !       number of particles in the system
    !   ll : DOUBLE PRECISION
    !       strenght of single spin term
    !
    !   Return
    !   ------
    !   H  : DOUBLE COMPLEX, DIMENSION(2**N, 2**N)
    !       Matrix representation of the Ising Hamiltonian
    !
        implicit none
        ! parameters
        INTEGER :: N
        DOUBLE PRECISION :: ll
        ! return
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: H

        if ( ALLOCATED(H) ) DEALLOCATE(H)
        ALLOCATE(H(2**N,2**N))

        H = -Ising_1D_single(N, ll) - Ising_1D_interaction(N)

        RETURN

    end function Ising_1D

    function Ising_1D_single(N, ll) result(H0)
    !
    ! Computes the single spin term of the Ising Hamiltonian
    ! for N 1/2 spin particles on 1D lattice.
    !
    !   Parameters
    !   ----------
    !   N  : INTEGER
    !       number of particles in the system
    !   ll : DOUBLE PRECISION
    !       strenght of single spin term
    !
    !   Return
    !   ------
    !   H0 : DOUBLE COMPLEX, DIMENSION(2**N, 2**N)
    !       Matrix representation of the single spin term
    !       of the Ising Hamiltonian
    !
        implicit none
        ! parameters
        INTEGER :: N
        DOUBLE PRECISION :: ll
        ! return
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: H0
        ! local variables
        INTEGER :: ii
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: tmp

        ! allocate space for matrix
        if ( ALLOCATED(H0) ) DEALLOCATE(H0)
        ALLOCATE(H0(2**N, 2**N))

        H0 = 0.d0

        ! Cycle over the N spins and make the tensor products.
        ! The tensor product between M identity matrices of
        ! size (2,2) is an Identity matrix of size (2**M, 2**M),
        ! so it is not calculted using the TensorProduct function.
        ! The only tensor products that need to be done explicity are:
        !
        ! I(2**(ii-1), 2**(ii-1)) x Sz x I(2**(N-ii), 2**(N-ii))
        !
        ! Special handling for first/last spins is required,
        ! since they do not have the Identity matrix before/after.

        do ii = 1, N
            ! first particle
            if      ( ii == 1 ) then
                H0 = H0 + TensorProduct(Sx, UnitMat(2**(N-ii)))
            ! last particle
            else if ( ii == N ) then
                H0 = H0 + TensorProduct(UnitMat(2**(ii-1)), Sx)
            ! particles in the middle of the chain
            else
                ! do not need to allocate space for tmp,
                ! since the TensorProduct function takes care of it
                tmp = TensorProduct(UnitMat(2**(ii-1)), Sx)
                H0  = H0 + TensorProduct(tmp, UnitMat(2**(N-ii)))
                DEALLOCATE(tmp)
            end if
        end do

        H0 = ll*H0

        RETURN

    end function Ising_1D_single

    function Ising_1D_interaction(N) result(H1)
    !
    ! Computes the single spin term of the Ising Hamiltonian
    ! for N 1/2 spin particles on 1D lattice.
    !
    !   Parameters
    !   ----------
    !   N  : INTEGER
    !       number of particles in the system
    !   ll : DOUBLE PRECISION
    !       strenght of single spin term
    !
    !   Return
    !   ------
    !   H1 : DOUBLE COMPLEX, DIMENSION(2**N, 2**N)
    !       Matrix representation of the single spin term
    !       of the Ising Hamiltonian
    !
        implicit none
        ! parameters
        INTEGER :: N
        ! return
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: H1
        ! local variable
        INTEGER :: ii
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: tmp1, tmp2

        ALLOCATE(H1(2**N, 2**N))
        H1 = 0.d0

        ! Cycle over the N spins and make the tensor products.
        ! The tensor product between M identity matrices of
        ! size (2,2) is an Identity matrix of size (2**M, 2**M),
        ! so it is not calculted using the TensorProduct function.
        ! The only tensor products that need to be done explicity are:
        !
        ! I(2**(ii-1), 2**(ii-1)) x Sx x Sx x I(2**(N-ii-1), 2**(N-ii-1))
        !
        ! Special handling for first/last (N-1) spins is required,
        ! since they do not have the Identity matrix before/after.

        do ii = 1, N-1
            ! first particle
            if ( ii == 1 ) then
                ! do not need to allocate space for tmp1,
                ! since the TensorProduct function takes care of it
                tmp1 = TensorProduct(Sz, Sz)
                H1   = H1 + TensorProduct(tmp1, UnitMat(2**(N-ii-1)))
                DEALLOCATE(tmp1)
            ! last particle (N-1)
            else if ( ii == N-1 ) then
                ! do not need to allocate space for tmp1,
                ! since the TensorProduct function takes care of it
                tmp1 = TensorProduct(Sz, Sz)
                H1   = H1 + TensorProduct(UnitMat(2**(ii-1)), tmp1)
                DEALLOCATE(tmp1)
            else
                ! do not need to allocate space for tmp1 and tmp2,
                ! since the TensorProduct function takes care of it
                tmp1 = TensorProduct(UnitMat(2**(ii-1)), Sz)
                tmp2 = TensorProduct(Sz, UnitMat(2**(N-ii-1)))
                H1   = H1 + TensorProduct(tmp1, tmp2)
                DEALLOCATE(tmp1)
                DEALLOCATE(tmp2)
            end if
        end do

        RETURN

    end function Ising_1D_interaction

    function Ising_2D(N, ll) result(H)
    !
    ! Computes the Ising Hamiltonian for N 1/2 spin particles
    ! on 2D lattice.
    !
    !   Parameters
    !   ----------
    !   N  : INTEGER
    !       square number of particles in the system
    !   ll : DOUBLE PRECISION
    !       strenght of single spin term
    !
    !   Return
    !   ------
    !   H  : DOUBLE COMPLEX, DIMENSION(2**N*N,2**N*N)
    !       Matrix representation of the Ising Hamiltonian
    !
        implicit none
        ! parameters
        INTEGER :: N
        DOUBLE PRECISION :: ll
        ! return
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: H, Interact_lato, Interact_sopra
        integer :: i,j

        if ( ALLOCATED(H) ) DEALLOCATE(H)
        if ( ALLOCATED(Interact_lato) ) DEALLOCATE(Interact_lato)
        if ( ALLOCATED(Interact_sopra) ) DEALLOCATE(Interact_sopra)
        ALLOCATE(H(2**(N*N),2**(N*N)))
        ALLOCATE(Interact_lato(2**(N*N),2**(N*N)))
        ALLOCATE(Interact_sopra(2**(N*N),2**(N*N)))

        Interact_lato  = cmplx(0d0, 0d0, KIND=8)
        Interact_sopra = cmplx(0d0, 0d0, KIND=8)

        !interaction with letf and right

        do i =1 , N
            do j= 1,N
                if (j .eq. 1) then
                    Interact_lato = Interact_lato + TensorProduct2mat( UnitMat( 2**((i-1) * N )),&
                                    TensorProduct2mat( Sz , TensorProduct2mat( Sz , UnitMat( 2**((N-i) * N + N-2)) ) ) )
                else if (j /= 1 .AND. mod(j,N) /= 0) then
                    Interact_lato = Interact_lato + TensorProduct2mat( UnitMat( 2**((i-1)*N +j -1 )) ,&
                                    TensorProduct2mat( Sz , TensorProduct2mat( Sz , UnitMat( 2**(N-j-1 + (N-i)*N) ) ) ) )
                end if
            end do
        end do

        !interaction above an bellow
        do i =1, N
            do j =1,N
                if (i /= N) then
                    Interact_sopra = Interact_sopra + TensorProduct2mat( UnitMat(2**( (i-1)*N + j-1 )) ,&
                                        TensorProduct2mat(  TensorProduct2mat( Sz, UnitMat(2**(N-1))) ,  TensorProduct2mat( Sz,&
                                        UnitMat(  2**((N-i-1)*N + N-j)  ) )  ) )
                end if
            end do
        end do

        H= - Ising_1D_single(N*N, ll) - Interact_lato - Interact_sopra

        RETURN

    end function Ising_2D

    function TensorProduct2mat(m1, m2) result(mat)
    !
    ! Function to calculate the tensor product between two matrices
    !
    !   Parameters
    !   ----------
    !   m1  : DOUBLE COMPLEX, DIMENSION(r1, c1)
    !       first vector
    !   m2  : DOUBLE COMPLEX, DIMENSION(r2, c2)
    !       second vector
    !
    !   Return
    !   ------
    !   mat : DOUBLE COMPLEX, DIMENSION(r1*r2, c1*c2)
    !       tensor product
    !
        implicit none
        ! parametrs
        DOUBLE COMPLEX, DIMENSION(:,:) :: m1, m2
        ! return
        DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: mat
        ! local variables
        !   number of rows of m1,m2
        INTEGER :: r1, r2
        !   number of columns of m1,m2
        INTEGER :: c1, c2
        !   indexes
        INTEGER :: ii, jj

        r1 = SIZE(m1, 1)
        r2 = SIZE(m2, 1)
        c1 = SIZE(m1, 2)
        c2 = SIZE(m2, 2)

        ! allocate space for TP matrix
        if ( ALLOCATED(mat) ) DEALLOCATE(mat)
        ALLOCATE(mat(r1*r2, c1*c2))

        do ii = 0, r1-1
            do jj = 0, c1-1
                ! compute each block of size (r2, c2) multipling each element
                ! of m1 by the whole matrix m2
                mat(r2*ii+1:r2*(ii+1), c2*jj+1:c2*(jj+1)) = m1(ii+1,jj+1)*m2
            end do
        end do

        RETURN

    end function TensorProduct2mat

    function UnitMat(N) result(U)
    !
    ! Returns the Identity matrix of dimension N
    !
    !   Parameters
    !   ----------
    !   N : INTEGER
    !       matrix dimension
    !
    !   Return
    !   ------
    !   U : DOUBLE COMPLEX, DIMENSION(N,N)
    !       Identity matrix of dimension (N,N)
    !
        implicit none
        ! parameters
        INTEGER :: N
        ! return
        DOUBLE COMPLEX, ALLOCATABLE :: U(:,:)
        ! local variables
        INTEGER :: ii

        if ( N == 0 ) then
            ALLOCATE(U(1,1))
            U = CMPLX(1.d0, 1.d0, KIND=8)
        else
            ALLOCATE(U(N,N))
            U = CMPLX(0.d0, 0.d0, KIND=8)
            do ii = 1, N
                U(ii,ii) = 1.d0
            end do
        end if

        RETURN

    end function UnitMat

end module hamiltonian