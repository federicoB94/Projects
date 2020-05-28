program test
    use simulation
    use rbm
    use debug
    use hamiltonian
    use others
    implicit none

    INTEGER :: S(5,3), ii
    INTEGER, ALLOCATABLE :: config(:,:)
    DOUBLE PRECISION :: g
    DOUBLE COMPLEX :: Ai(3), Bj(3), Wij(3,3), Psi
    DOUBLE COMPLEX, ALLOCATABLE :: Opk(:,:), H(:,:), Eloc(:), S_kk(:,:), Fk(:)

    Ai = (/-0.05d0, 0.08d0, 0.02d0/)
    CALL debugging(.TRUE., var=Ai, message="Ai")
    Bj = (/0.1d0, -0.05d0, -0.08d0/)
    CALL debugging(.TRUE., var=Bj, message="Bj")

    Wij(1,:) = (/-0.03d0,  0.02d0, 0.1d0 /)
    Wij(2,:) = (/ 0.07d0, -0.12d0, 0.03d0/)
    Wij(3,:) = (/-0.1d0 , -0.03d0, 0.05d0/)
    CALL debugging(.TRUE., var=Wij, message="Wij")

    CALL Metropolis(Ai, Bj, Wij, 100, 0, 1, config, Opk)
    CALL debugging(.TRUE., var=COUNT(config ==  1), message="'+1' in Metropolis configurations")
    CALL debugging(.TRUE., var=COUNT(config == -1), message="'-1' in Metropolis configurations")

    S(1, :) = (/1,  1,  1/)
    S(2, :) = (/1, -1, -1/)
    S(3, :) = (/1, -1, -1/)
    S(4, :) = (/1,  1, -1/)
    S(5, :) = (/1, -1, -1/)
    CALL debugging(.TRUE., var=S, message="S")

    H = Ising_1D(3, 0.2d0)
    CALL debugging(.TRUE., var=H, message="H")

    Eloc = LocalEnergy(S,H,Ai,Bj,Wij)
    CALL debugging(.TRUE., var=Eloc, message="Eloc")

    Psi = logPsiDiff(S(2,:), S(1,:), Ai, Bj, Wij)
    CALL debugging(.TRUE., var=Psi, message="log Psi diff")

    ALLOCATE(S_kk(15,15))
    S_kk = 0.d0
    do ii = 1, 15
        S_kk(ii,ii) = 1.d0
    end do
    S_kk(3,3) = -0.5d0
    CALL debugging(.TRUE., var=S_kk, message="S_kk")

    ALLOCATE(Fk(15))
    Fk = 1.d0
    Fk(8) = -0.7d0
    Fk(14) = 0.25d0
    CALL debugging(.TRUE., var=Fk, message="Fk")

    g = 0.5d0
    CALL RBM_update(Ai, Bj, Wij, S_kk, Fk, g)
    CALL debugging(.TRUE., var=Ai, message="Ai after update")
    CALL debugging(.TRUE., var=Bj, message="Bj after update")
    CALL debugging(.TRUE., var=Wij, message="Wij after update")



end program test