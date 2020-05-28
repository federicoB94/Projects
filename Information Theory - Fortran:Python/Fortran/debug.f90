module debug
    implicit none
    ! wheter to print " DEBUGGING SUBROUTINE " title
    LOGICAL :: dbTitle = .FALSE.

    interface debugging
    !
    ! Subroutine to be used as a checpoint for debugging.
    !   It requires a LOGICAL variable to decide whether the debugging
    !   message should be printed or not.
    !   The message could be personalized by the user using the optional
    !   string input (max 250 characters length).
    !   Multiple interfaces are provided to print also the value of a
    !   variable under inspection. Supported types:
    !   - Single Value
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX, CHARACTER(LEN=*)
    !   - Array [DIMENSION(:)]
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   - Matrix [DIMENSION(:,:)]
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !
    !   Parameters
    !   ----------
    !   active : LOGICAL
    !       .TRUE. to print the debugging message, .FALSE. otherwise
    !   var : TYPE(*) (optional)
    !       variable value to be printed; supported types:
    !       - single values:
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX,
    !           CHARACTER(LEN=*), LOGICAL
    !       - array [DIMENSION(:)]
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !       - matrix [DIMENSION(:,:)]
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   comment : CHARACTER (optional, max length=250)
    !       optional comment to be printed for debugging
    !       (default = "Checkpoint without specific description")
    !   logfile : CHARACTER (optional, avaiable only for array and matrix)
    !       If present, the values will be printed in that file, otherwise
    !       the standard output will be used.
    !       In the first case, the format used is thought to be read by a program;
    !       The first row contains the dimension of the object (for matrices, number
    !       of rows and columns are separated by a semicolumn ';').
    !       The following lines contain the values of the array (one per row) or
    !       a row of the matrix (entries are divided by semicolumns ';').
    !       Real and imaginary parts of complex numbers are split with a comma ','.
    !       If the standard output is chosen, the values (in particular for matrices)
    !       are displayed in a more human readable form (so the number could be truncated).
    !
        module procedure debug_value, debug_array, debug_matrix

    end interface

    interface debug_print
    !
    ! Subroutine to print variables value in debugging.
    !   Supported types:
    !   - Single Value
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX,
    !       CHARACTER(LEN=*), LOGICAL
    !   - Array [DIMENSION(:)]
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   - Matrix [DIMENSION(:,:)]
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   If the variable supplied does not belong to any of the types listed,
    !   a message of 'unsupported type' is printed.
    !   For arrays and matrices the user can choose to print to the standard
    !   output or to a file.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*)
    !       variable value to be printed; supported types:
    !       - single values:
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX,
    !           CHARACTER(LEN=*), LOGICAL
    !       - array [DIMENSION(:)]
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !       - matrix [DIMENSION(:,:)]
    !           INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   logfile : CHARACTER (optional, avaiable only for array and matrix)
    !       If present, the values will be printed in that file, otherwise
    !       the standard output will be used.
    !       In the first case, the format used is thought to be read by a program;
    !       The first row contains the dimension of the object (for matrices, number
    !       of rows and columns are separated by a semicolumn ';').
    !       The following lines contain the values of the array (one per row) or
    !       a row of the matrix (entries are divided by semicolumns ';').
    !       Real and imaginary parts of complex numbers are split with a comma ','.
    !       If the standard output is chosen, the values (in particular for matrices)
    !       are displayed in a more human readable form (so the number could be truncated).
    !
        module procedure print_value, &
            print_array_std,  print_array_file, &
            print_matrix_std, print_matrix_file
    end interface

contains

    ! -------------------- !
    ! interface: debugging !
    ! -------------------- !

    subroutine debug_value(active, var, message)
    !
    ! Subroutine to create a debug checkpoint and monitor the value of a variable.
    !
    !   Parameters
    !   ----------
    !   active : LOGICAL
    !       .TRUE. to print the debugging message, .FALSE. otherwise
    !   var : TYPE(*) (optional)
    !       variable value to be printed; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX,
    !       CHARACTER(LEN=*), LOGICAL
    !   message : CHARACTER(optional, max length=250)
    !       optional comment to be printed for debugging
    !       (default = "Checkpoint without specific description")
    !
        implicit none
        LOGICAL,  INTENT(IN) :: active
        CLASS(*), INTENT(IN), OPTIONAL :: var
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: message

        CHARACTER(LEN=250) :: message_

        if ( .NOT. active ) RETURN

        if ( PRESENT(message) ) then
            message_ = message
        else
            message_ = "Checkpoint without specific description"
        end if

        print*
        if( dbTitle .eqv. .TRUE. ) then
            print*, REPEAT("=", 14), " DEBUGGING SUBROUTINE ", REPEAT("=", 14)
        else
            print*, REPEAT("=", 50)
        end if
        print*, TRIM(message_)

        if (PRESENT(var)) then
            CALL debug_print(var)
        end if

        print*, REPEAT("=", 50)
        print*

    end subroutine debug_value

    subroutine debug_array(active, var, message, logfile)
    !
    ! Subroutine to create a debug checkpoint and monitor the value of an array.
    !
    !   Parameters
    !   ----------
    !   active : LOGICAL
    !       .TRUE. to print the debugging message, .FALSE. otherwise
    !   var : TYPE(*), DIMENSION(:) (optional)
    !       variable value to be printed; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   message : CHARACTER(optional, max length=250)
    !       optional comment to be printed for debugging
    !       (default = "Checkpoint without specific description")
    !   logfile : CHARACTER(LEN=*) (optional)
    !       If present, the values will be printed in that file, otherwise
    !       the standard output will be used.
    !
        implicit none
        LOGICAL,  INTENT(IN) :: active
        CLASS(*), DIMENSION(:), INTENT(IN) :: var
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: message
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: logfile

        CHARACTER(LEN=250) :: message_

        if ( .NOT. active ) RETURN

        if ( PRESENT(message) ) then
            message_ = message
        else
            message_ = "Checkpoint without specific description"
        end if

        print*
        if( dbTitle .eqv. .TRUE. ) then
            print*, REPEAT("=", 14), " DEBUGGING SUBROUTINE ", REPEAT("=", 14)
        else
            print*, REPEAT("=", 50)
        end if
        print*, TRIM(message_)
        if ( PRESENT(logfile) ) then
            CALL debug_print(var, logfile)
        else
            CALL debug_print(var)
        end if

        print*, REPEAT("=", 50)
        print*

    end subroutine debug_array

    subroutine debug_matrix(active, var, message, logfile)
    !
    ! Subroutine to create a debug checkpoint and monitor the value of a matrix.
    !
    !   Parameters
    !   ----------
    !   active : LOGICAL
    !       .TRUE. to print the debugging message, .FALSE. otherwise
    !   var : TYPE(*), DIMENSION(:,:) (optional)
    !       variable value to be printed; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   message : CHARACTER(optional, max length=250)
    !       optional comment to be printed for debugging
    !       (default = "Checkpoint without specific description")
    !   logfile : CHARACTER(LEN=*) (optional)
    !       If present, the values will be printed in that file, otherwise
    !       the standard output will be used.
    !
        implicit none
        LOGICAL,  INTENT(IN) :: active
        CLASS(*), DIMENSION(:,:), INTENT(IN) :: var
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: message
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: logfile

        CHARACTER(LEN=250) :: message_

        if ( .NOT. active ) RETURN

        if ( PRESENT(message) ) then
            message_ = message
        else
            message_ = "Checkpoint without specific description"
        end if

        print*
        if( dbTitle .eqv. .TRUE. ) then
            print*, REPEAT("=", 14), " DEBUGGING SUBROUTINE ", REPEAT("=", 14)
        else
            print*, REPEAT("=", 50)
        end if
        print*, TRIM(message_)
        if ( PRESENT(logfile) ) then
            CALL debug_print(var, logfile)
        else
            CALL debug_print(var)
        end if

        print*, REPEAT("=", 50)
        print*

    end subroutine debug_matrix

    ! ---------------------- !
    ! interface: debug_print !
    ! ---------------------- !

    subroutine print_value(var)
    !
    ! Subroutine to monitor and print the value of a variable to standard output.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*)
    !       variable to minitor; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX,
    !       CHARACTER(LEN=*), LOGICAL
    !
        implicit none
        CLASS(*), INTENT(IN) :: var

        select type(var)
            type is (INTEGER)
                print*, "[INTEGER]", var
            type is (REAL)
                print*, "[REAL]", var
            type is (DOUBLE PRECISION)
                print*, "[DOUBLE PRECISION]", var
            type is (COMPLEX)
                print*, "[COMPLEX]", var
            type is (COMPLEX(kind=8))
                print*, "[DOUBLE COMPLEX]", var
            type is (CHARACTER(LEN=*))
                print'(x, a, i3, a, x, a)', "[CHARACTER(LEN=", LEN(var), ")]", var
            type is (LOGICAL)
                print*, "[LOGICAL]", var
            class default
                print*, "Unknown type"
        end select

    end subroutine print_value

    subroutine print_array_std(var)
    !
    ! Subroutine to monitor and print the value of an array to standard output.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*), DIMENSION(:)
    !       array to minitor; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !
        implicit none
        CLASS(*), DIMENSION(:), INTENT(IN) :: var

        INTEGER :: jj

        select type(var)
            type is (INTEGER)
                print'(x, a, i3, a)', "[INTEGER array, dimension=", SIZE(var), "]"
                WRITE(*, *) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (REAL)
                print'(x, a, i3, a)', "[REAL array, dimension=", SIZE(var), "]"
                WRITE(*, *) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (DOUBLE PRECISION)
                print'(x, a, i3, a)', "[DOUBLE PRECISION array, dimension=", SIZE(var), "]"
                WRITE(*, *) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (COMPLEX)
                print'(x, a, i3, a)', "[COMPLEX array, dimension=", SIZE(var), "]"
                WRITE(*, *) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (COMPLEX(kind=8))
                print'(x, a, i3, a)', "[DOUBLE COMPLEX array, dimension=", SIZE(var), "]"
                WRITE(*, *) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            class default
                print*, "Unknown type"
        end select

    end subroutine print_array_std

    subroutine print_array_file(var, logfile)
    !
    ! Subroutine to monitor and print the value of an array to a file.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*), DIMENSION(:)
    !       array to minitor; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   logfile : CHARACTER(LEN=*)
    !       filename where the array values are printed.
    !       In row 1, the array dimension is written.
    !       From row 2, values of the array are printed one per row.
    !       For complex numbers, real and imaginary parts are separated by ','.
    !
        implicit none
        CLASS(*), DIMENSION(:), INTENT(IN) :: var
        CHARACTER(LEN=*), INTENT(IN) :: logfile

        INTEGER :: jj

        print'(x, a, x, a)', "Printing the array in file:", TRIM(logfile)
        print'(3x, a)'     , "Array dimension is printed in 1st row"
        print'(3x, a)'     , "Values of the array are printed one per row (starting from row 2)"
        print'(3x, a)'     , "For complex number, Re(z) and Im(z) are separeted by ','"
        OPEN(15, file=TRIM(logfile))

        select type(var)
            type is (INTEGER)
                print*, "[INTEGER array]"
                WRITE(15,*) "# ", SIZE(var)
                WRITE(15,*) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (REAL)
                print*, "[REAL array]"
                WRITE(15,*) "# ", SIZE(var)
                WRITE(15,*) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (DOUBLE PRECISION)
                print*, "[DOUBLE PRECISION array]"
                WRITE(15,*) "# ", SIZE(var)
                WRITE(15,*) (var(jj), NEW_LINE(' '), jj=1,SIZE(var))
            type is (COMPLEX)
                print*, "[COMPLEX array]"
                WRITE(15,*) "# ", SIZE(var)
                WRITE(15,*) (REAL(var(jj)), ",", AIMAG(var(jj)), NEW_LINE(' '), jj=1,SIZE(var))
            type is (COMPLEX(kind=8))
                print*, "[DOUBLE COMPLEX array]"
                WRITE(15,*) "# ", SIZE(var)
                WRITE(15,*) (REAL(var(jj)), ",", AIMAG(var(jj)), NEW_LINE(' '), jj=1,SIZE(var))
            class default
                print*, "Unknown type"
        end select

        CLOSE(15)

    end subroutine print_array_file

    subroutine print_matrix_std(var)
    !
    ! Subroutine to monitor and print the value of a matrix to standard output.
    !   It should be used only for small matrices; the format is adjusted
    !   in order to be human readable and hence some values may result truncated.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*), DIMENSION(:,:)
    !       array to minitor; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !
        implicit none
        CLASS(*), DIMENSION(:,:), INTENT(IN) :: var

        INTEGER :: ii, jj

        select type(var)
            type is (INTEGER)
                print'(x, a, i3, a, i3, a)', &
                    "[INTEGER matrix, row=", SIZE(var,1), ", col=", SIZE(var,2), "]"
                do ii = 1, SIZE(var, 1)
                    WRITE(*, '(100(i9, 2x))') (var(ii,jj), jj=1,SIZE(var,2))
                end do

            type is (REAL)
                print'(x, a, i3, a, i3, a)', &
                    "[REAL matrix, dimension=", SIZE(var,1), ", col=", SIZE(var,2), "]"
                do ii = 1, SIZE(var,1)
                    WRITE(*, '(100(g10.5, 2x))') (var(ii, jj), jj=1,SIZE(var,2))
                end do

            type is (DOUBLE PRECISION)
                print'(x, a, i3, a, i3, a)', &
                    "[DOUBLE PRECISION matrix, dimension=", SIZE(var,1), ", col=", SIZE(var,2), "]"
                do ii = 1, SIZE(var,1)
                    WRITE(*, '(100(g10.5, 2x))') (var(ii, jj), jj=1,SIZE(var,2))
                end do

            type is (COMPLEX)
                print'(x, a, i3, a, i3, a)', &
                    "[COMPLEX matrix, dimension=", SIZE(var,1), ", col=", SIZE(var,2), "]"
                do ii = 1, SIZE(var,1)
                    WRITE(*, '(100(a, g10.5, a, g10.5, a, 2x))') &
                        ("(", REAL(var(ii, jj)), ", ", AIMAG(var(ii, jj)), ")", jj=1,SIZE(var,2))
                end do

            type is (COMPLEX(kind=8))
                print'(x, a, i3, a, i3, a)', &
                    "[DOUBLE COMPLEX matrix, dimension=", SIZE(var,1), ", col=", SIZE(var,2), "]"
                do ii = 1, SIZE(var,1)
                    WRITE(*, '(100(a, g13.5, a, g13.5, a, 2x))') &
                        ("(", REAL(var(ii, jj)), ", ", AIMAG(var(ii, jj)), ")", jj=1,SIZE(var,2))
                end do

            class default
                print*, "Unknown type"
        end select

    end subroutine print_matrix_std

    subroutine print_matrix_file(var, logfile)
    !
    ! Subroutine to monitor and print the value of a matrix to a file.
    !   It can be used even for big matrices, that will be read by programs.
    !
    !   Parameters
    !   ----------
    !   var : TYPE(*), DIMENSION(:)
    !       array to minitor; supported types:
    !       INTEGER, REAL, DOUBLE PRECISION, COMPLEX, DOUBLE COMPLEX
    !   logfile : CHARACTER(LEN=*)
    !       filename where the matrix values are printed.
    !       In row 1, the matrix dimension is written (row; cols).
    !       From row 2, the rows of the matrix are printed, one per line;
    !       different elements are split by ';'.
    !       For complex numbers, real and imaginary parts are separated by ','.
    !
        implicit none
        CLASS(*), DIMENSION(:,:), INTENT(IN) :: var
        CHARACTER(LEN=*), INTENT(IN) :: logfile

        INTEGER :: ii, jj

        print'(x, a, x, a)', "Printing the matrix in file:", TRIM(logfile)
        print'(3x, a)'     , "Matrix dimension is printed in 1st row (rows; cols)"
        print'(3x, a)'     , "Rows of the matrix are printed one per line (starting from row 2)"
        print'(3x, a)'     , "Different values in same row are divided by semicolumn ';'"
        print'(3x, a)'     , "For complex number, Re(z) and Im(z) are separeted by ','"
        OPEN(15, file=TRIM(logfile))

        select type(var)
            type is (INTEGER)
                print*, "[INTEGER matrix]"
                WRITE(15,*) "# ", SIZE(var,1), ";", SIZE(var,2), ";"
                do ii = 1, SIZE(var,1)
                    WRITE(15,*) (var(ii,jj), ";" , jj=1,SIZE(var,2))
                end do

            type is (REAL)
                print*, "[REAL matrix]"
                WRITE(15,*) "# ", SIZE(var,1), ";", SIZE(var,2), ";"
                do ii = 1, SIZE(var,1)
                    WRITE(15,*) (var(ii,jj), ";" , jj=1,SIZE(var,2))
                end do

            type is (DOUBLE PRECISION)
                print*, "[DOUBLE PRECISION matrix]"
                WRITE(15,*) "# ", SIZE(var,1), ";", SIZE(var,2), ";"
                do ii = 1, SIZE(var,1)
                    WRITE(15,*) (var(ii,jj), ";" , jj=1,SIZE(var,2))
                end do

            type is (COMPLEX)
                print*, "[COMPLEX matrix]"
                WRITE(15,*) "# ", SIZE(var,1), ";", SIZE(var,2), ";"
                do ii = 1, SIZE(var,1)
                    WRITE(15,*) (REAL(var(ii,jj)), ",", AIMAG(var(ii,jj)), ";" , jj=1,SIZE(var,2))
                end do

            type is (COMPLEX(kind=8))
                print*, "[DOUBLE COMPLEX matrix]"
                WRITE(15,*) "# ", SIZE(var,1), ";", SIZE(var,2), ";"
                do ii = 1, SIZE(var,1)
                    WRITE(15,*) (REAL(var(ii,jj)), ",", AIMAG(var(ii,jj)), ";" , jj=1,SIZE(var,2))
                end do

            class default
                print*, "Unknown type"
        end select

        CLOSE(15)

    end subroutine print_matrix_file

end module debug
