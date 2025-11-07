! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-14: Procedures with Array Arguments
! Date: 12th August 2025, 
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Excercise for Writing Function for more generalized form, testing polynomial evolution
! Example Program 14.2: Generalized version
function polynomial(x,a,n)
    implicit none
    real, intent(in)::x
    integer, intent(in)::n 
    real, dimension(0:n), intent(in)::a 
    integer:: i 
    real::polynomial
    polynomial = a(n)
    do i=n,1,-1
        polynomial =a(i-1)+ (x*polynomial)
    end do
end function polynomial
!## Actual Program
program poly_v1
    implicit none
    integer:: i, m 
    integer, parameter :: max_degree=30
    real,dimension(0:max_degree)::p 
    real::z,poly_value,polynomial 
    ! Evolution of polynomial
    print*,"Order of polynomial:"
    read*, m 
    print*,"Variable value:"
    read*, z
    print*, "Enter coefficent:"
    read*,(p(i),i = 0,m)
    poly_value = polynomial(z,p(0:m),m) !p(0:m)  slice, gives the function only the "m" number of coefficients, not unused elements of max_degree, which are mostly unkowns/zeros
    print*,"Polynomial COefficient"
    print*,(p(i),i=0,m)
    print*, "Polynomial Value:", poly_value
end program poly_v1
!--------------------------------------------------------------------------------------------------------
! ## Example Program 14.3: Interface version
function polynomial(x,a)
    implicit none
    real, intent(in)::x
    real, dimension(0:), intent(in)::a 
    integer:: i,n !local variable
    real::polynomial
    n = size(a)
    polynomial = a(n)
    do i=n,1,-1
        polynomial =a(i-1)+ (x*polynomial)
    end do
end function polynomial
!## Actual Program
program poly_v2
    implicit none
    integer:: i, m 
    integer, parameter :: max_degree=30
    real,dimension(0:max_degree)::p 
    real::z,poly_value !Note: "polynomial" variable not defined here. 
    ! Interface block, used to give all the declarations used in main program
    interface 
        function polynomial(x,a)
            implicit none
            real, intent(in)::x
            real, dimension(0:), intent(in)::a 
            real::polynomial
        end function polynomial
    end interface
    ! Evolution of polynomial
    print*,"Order of polynomial:"
    read*, m 
    print*,"Variable value:"
    read*, z
    print*, "Enter coefficent:"
    read*,(p(i),i = 0,m)
    poly_value = polynomial(z,p(0:m))
    print*, "Polynomial Value:", poly_value
end program poly_v2
!--------------------------------------------------------------------------------------------------------
! !## Example Program 14.4: Module version
module poly_data
    implicit none 
    save
    integer, parameter:: max_degree = 30
    real, dimension(0:max_degree):: a
end module poly_data

function polynomial(x,n)
    use poly_data
    implicit none
    real, intent(in)::x
    integer, intent(in):: n 
    integer :: i !local variable
    real::polynomial
    polynomial = a(n)
    do i=n,1,-1
        polynomial =a(i-1)+ (x*polynomial)
    end do
end function polynomial
!## Actual Program
program poly_v3
    use poly_data
    implicit none
    integer:: i,m 
    real::z,poly_value, polynomial
    ! Evolution of polynomial
    print*,"Order of polynomial:"
    read*, m 
    print*,"Variable value:"
    read*, z
    print*, "Enter coefficent:"
    read*,(a(i),i = 0,m)
    poly_value = polynomial(z,m)
    print*, "Polynomial Value:", poly_value
end program poly_v3

! ##====================================================================================================================================
! ## Example-program 14.7, Exchanging max. element value of the row to 1st value of the row with 1st column
module matrix_data
    implicit none
    save 
    integer, parameter :: max_rows=20, max_cols=20
    integer, dimension(max_rows, max_cols):: h
end module matrix_data

subroutine mat_print(row,col)
    use matrix_data 
    implicit none
    integer, intent(in)::row, col
    integer :: i,j
    do i=1, row
        print*,(h(i,j),j = 1,col)
    end do
end subroutine mat_print

subroutine exchange(x,y)
    implicit none
    integer :: x,y,temp
    temp = x 
    x = y
    y = temp
end subroutine exchange

subroutine max_row(rows, cols)
    use matrix_data
    implicit none 
    integer, intent(in):: rows,cols 
    integer:: i,j 
    do i=1,rows
        do j=1,cols 
            if (h(i,j)>h(i,1)) then 
                call exchange(h(i,j),h(i,1))
            end if 
        end do
    end do 
end subroutine max_row

program exchange_values
    use matrix_data
    implicit none
    integer:: m_rows, n_cols, i, j 
    print*, "Enter Number of rows and columns"
    read*, m_rows, n_cols
    print*, "Matrix element values"
    do i = 1,m_rows
        read*,(h(i,j),j = 1,n_cols)
    end do
    ! Printing matrix row-wise
    print*,"*********************************"
    print*,"Matrix (as loaded)"
    call mat_print(m_rows,n_cols)
    ! Transforming the matrix based on exchange
    call max_row(m_rows,n_cols)
    ! Print Transform matrix row-wise
    print*,"*********************************"
    print*,"Matrix (Transformed)"
    call mat_print(m_rows,n_cols)
end program exchange_values

!##====================================================================================================================================
!  Function of Dummy Arguments
! Example program 14.10
subroutine integral_trapz(f,n,int_f, x0, xn)
    implicit none
    real, external:: f
    real, intent(in)::x0, xn
    integer, intent(in):: n
    real, intent(out)::int_f
    integer:: i 
    real:: h, sum 
    ! Integration by Trapezoidal
    h = (xn-x0)/n 
    sum = (f(x0)+f(xn))/2
    do i = 1, n-1
        sum = sum + f(x0+i*h)
    end do
    int_f = sum*h 
end subroutine integral_trapz

function f(x)
    implicit none 
    real, intent(in):: x
    real:: f 
    f = x*sin(x)
end function f
! ---------------------------------------------------------
! Version#1 with out using interface
program integral_f_v1
    implicit none
    real,external:: f 
    real:: int_val
    real:: x0, xn
    integer:: n
    print*, "Enter the number of points, n:"
    read*, n
    print*, "Domian of integration: x0, xn = "
    read*, x0, xn
    call integral_trapz(f,n, int_val, x0, xn)
    print*, "Integral =", int_val
end program integral_f_v1
! ---------------------------------------------------------
! Version 2 using interface
program integral_f_v2
    implicit none
    interface
    function f(x)
        implicit none 
        real, intent(in):: x
        real:: f 
    end function f
    end interface
    real:: int_val
    real:: x0, xn
    integer:: n
    print*, "Enter the number of points, n:"
    read*, n
    print*, "Domian of integration: x0, xn = "
    read*, x0, xn
    call integral_trapz(f,n, int_val, x0, xn)
    print*, "Integral =", int_val
end program integral_f_v2


