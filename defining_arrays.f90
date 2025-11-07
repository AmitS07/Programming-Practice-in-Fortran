! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-10: Defining and Manipulating Arrays
! Date: 6th July 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!!Example Program 10.4
program poly_eval
    implicit none
    integer:: i,n,m
    real::x,poly
    real, allocatable:: a(:)
    print*,"Enter the order of polynomial, x and no. of coeff."
    read*, n,x, m
    allocate(a(0:m))
    print*,"Enter component of a:"
    read*, a
    ! Checking a and n
    if (m/=n) then
        print*,"Opps! poly order is bigger than no. coeff."
        stop
    end if 
    poly = a(n)
    do i=n,1,-1
        poly = a(i-1)+x*poly
    end do
    print*,"Polynomial:",poly
end program poly_eval
!--------------------------------------------------------------------------------------------------------
!! Example Program 10.8, 
! Motive: To read the variables in DO loop and test the "ALLOCATTABLE" function to allocate the space in memory based on the size hspe of the array
program line_fitting
    implicit none
    ! Defining variables:
    real :: m, c, t, numerator, denominator
    real :: sumx = 0, sumy=0, sumxy = 0, sumxsq = 0
    integer :: i, n, yes, done
    real, allocatable, dimension(:) :: x, y !  ! Allocating empty arrays for space, the shape of the array will be define later
    print*, "Number of points in line:"
    read*, n
    allocate(x(n),y(n),stat = yes) ! Allocating array's shape, Note: stat is for status of the allay, whether allocated or not
    ! *************************************************
    if (yes/=0) then ! Checking whethere allocation is done or not
        print*,"Sapce not allocated to x, y arrays"
        stop
    end if
    ! *************************************************
    ! Actual code
    read*, (x(i), y(i), i = 1,n) ! It Read with implicit DO loop
    ! *************************************************
    print*, "Dimensions of allocated arrays:"
    print*, "Rank of x:", rank(x)
    print*, "Size of x:", size(x)
    print*, "Shape of x:", shape(x)
    ! *************************************************
    do i = 1, n 
        sumx = sumx + x(i)
        sumy = sumy + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumxsq = sumxsq + x(i)*x(i)
    end do
    t = n 
    numerator = t*sumxy-sumx*sumy
    denominator = t*sumxsq - sumx*sumy
    m = numerator/denominator
    c = (sumy-m*sumx)/t 
    print*, "Equation of stright line:"
    print*, "y =",m,"x+",c
    ! ************************************************* 
end program line_fitting

!--------------------------------------------------------------------------------------------------------
!  Example program 10.9
!  Cost calculation
program stock_cost
    implicit none
    integer:: brand,  watts
    integer, dimension(2,3):: bulbs
    real, dimension(2,3):: cost 
    real:: abulb, tcost=0
    print*, "Bulb stock:"
    read*, bulbs 
    print*,"Cost of Bulbs:"
    read*, cost 
    ! FInding total cost
    do brand=1,2
        do watts=1,3
            if(bulbs(brand, watts)==0) then 
                print*, "Out of stock"
            else
                abulb = bulbs(brand, watts)
                tcost = tcost + cost(brand, watts)*abulb
            endif
        end do
    end do
    ! totaal cost calculation ny using intrisic function, this work when matrices are conformable
    ! tcost = sum(cost*bulbs)
    print*,"Total value of bulbs =", tcost
end program stock_cost


!--------------------------------------------------------------------------------------------------------
!!  Example program 10.11, sorting numbers
program sort 
    implicit none
    integer, allocatable, dimension(:)::marks
    integer:: i, k, ok, limit, lastsw, n
    print*, "Enter the number of items:"
    read*, n ! number of items
    allocate(marks(n),stat=ok)
    if (ok/=0) then
        print*,"Space not allocated"
        stop
    end if
    ! Sorting routine.
    print*,"Enter the vaules to the each items"
    read*, marks
    lastsw = n
    limit  = n-1
    do k = 1, n
        if(lastsw ==1) exit
        lastsw = 1
        do i = 1, limit 
            if (marks(i)>=marks(i+1)) cycle
            call exchange(marks(i), marks(i+1))
            lastsw = i
        end do
    limit = lastsw -1
    end do
    print*, "Sorted items:", marks
    ! ===============================================
    contains
        subroutine exchange(a,b)
            integer, intent(inout):: a,b 
            integer:: temp
            temp = a
            a=b
            b=temp
        end subroutine exchange
end program sort

--------------------------------------------------------------------------------------------------------
!!  Example program 10.12, Fibonacci numbers
program fibonacci_number
    implicit none
    integer, allocatable, dimension(:):: fibonacci
    integer :: i, n, ok
    print*, "Enter the number of items:"
    read*, n ! number of items
    allocate(fibonacci(n),stat=ok)
    if (ok/=0) then
        print*,"Space not allocated"
        stop
    end if
    fibonacci(0) = 0
    fibonacci(1) = 1
    do i = 2, n
        fibonacci(i) = fibonacci(i-1) + fibonacci(i-2)
    end do
    print*, "Fibonacci Numbers:", fibonacci
end program fibonacci_number

--------------------------------------------------------------------------------------------------------
!!  Example program 10.14, Matrix multipication
program matmul_test
    implicit none
    integer, allocatable, dimension(:,:):: a, b, c 
    integer :: a_row, a_col, b_row, b_col, c_row, c_col
    integer:: ok
    print*, "Enter #rows and #column for a, b, c"
    read*, a_row, a_col, b_row, b_col
    !  making c-matrix dimension
    c_row = a_row
    c_col = b_col
    ! Allocting  matrix a, b, and c
    allocate(a(a_row, a_col), stat = ok)
    if (ok /=0) then; print*, "a not allocated"
        stop
    end if
    allocate(b(b_row, b_col), stat = ok)
    if (ok /=0) then; print*, "b not allocated"
        stop
    end if
    allocate(c(c_row, c_col), stat = ok)
    if (ok /=0) then; print*, "c not allocated"
        stop
    end if
    !  Reading data for a and b
    print*,"Enter column-wise a matrix"
    read*, a 
    print*,"Enter column-wise b matrix"
    read*, b 
    ! initialize c-matrix
    c = 0
    ! matmul using intrinci function
    c = matmul(a,b)
    print*, "Matrix-c column-wise:",c 
end program matmul_test
