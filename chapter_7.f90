! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-7: Implementing Loops in Proogram
! Date: 19th April 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example:7.2
program average_height
    ! Defining variables
    implicit none
    integer:: roll_n, sex_code, total_girl = 0, total_boy = 0, valid_data = 0
    real:: ht, avg_girl_ht, avg_boy_ht, total_girl_ht = 0, total_boy_ht = 0
    ! looping 
    do 
        print*, "Enter the roll-n, sex_code, height of student:"
        read*, roll_n, sex_code, ht
        if (roll_n==0) exit
        if (sex_code==1) then
            total_boy_ht = total_boy_ht+ht
            total_boy = total_boy+1
        elseif (sex_code==2) then
            total_girl_ht = total_girl_ht+ht
            total_girl = total_girl+1
        else 
            print*, "Some Error"
        endif 
        valid_data = valid_data+1
    enddo
    avg_boy_ht = total_boy_ht/real(total_boy)
    avg_girl_ht = total_girl_ht/real(total_girl)
    print*, "Total Boys= ", total_boy, "Avg. Height of Boys =", avg_boy_ht
    print*, "Total Girls= ", total_girl, "Avg. Height of Girls =", avg_girl_ht
endprogram average_height

!-------------------------------------------------------------------------
! Example:7.3
program celsius_fahrenheit
    ! Defining Variables
    implicit none
    integer:: cel_initial, cel_final, icrmt, cel
    real::fah
    ! Reading Variables
    print*,"Enter the Initial, Final and Increment of temp in Celsius:"
    read*, cel_initial, cel_final, icrmt
    ! Iterations
    ! nu_icrmt = (cel_final-cel_initial+icrmt)/icrmt
    do cel=cel_initial, cel_final, icrmt
        fah = 1.8*cel+32
        print*, "Temp. in Celsius",cel, "=", "Temp. in Fahrenheit",fah 
    enddo
endprogram celsius_fahrenheit
!-------------------------------------------------------------------------
! Example 7.5
function factorial(n) result(x)
    implicit none
    integer :: n
    integer:: x,i
    x = 1
    if (n>0) then
        do i= 1,n
            x = x*i
        enddo
    elseif (n==0) then
        x = 1
    else 
        print*,"Opps! n should non-ve"
        x= -1
    endif
endfunction factorial

program series_sum
    ! Defining Varibales
    implicit none
    integer:: n, i, a, factorial
    real:: x, sum, term
    print*, "Enter the step number for sum, and variable:"
    read*, n, x
    ! Summation
    sum = 0.0
    do i = 0, n-1,1
        a = (2*i+1)
        term = ((-1)**i)*(x**a)/real(factorial(a))
        sum  = term+sum
    enddo
    print *, "Sum of series:", sum
    print *, "Built-in sin(x):", sin(x)
endprogram series_sum
!-------------------------------------------------------------------------
! Example-7.6

function readtxt(n) result(xy)
    implicit none
    real, allocatable :: xy(:,:)   ! Result variable
    integer, intent(in) :: n
    real:: x,y
    integer:: i, unit
    allocate (xy(n,2)) ! Allocate memory for the 2D array (2 columns: one for x, one for y)
    unit = 7
    open(unit, file='xy_values.txt', status ='old', action ="read",iostat=i)
    do i = 1,n,1  ! Read x and y values from the file into the array
        read(unit, *) x,y
        xy(i,1) = x ! store x in 1st column
        xy(i,2) = y ! store y in 2nd column
    enddo
    close(unit) !close the file
endfunction readtxt

program line_fit
    ! Importing values
    implicit none
    integer:: i,n
    real:: x,y,sum_x,sum_y,sum_xy,sum_xx, m,c 
    real,allocatable:: data(:,:) ! Need to define the data-array as well
    ! Explicit interface to the readtxt function
    interface
        function readtxt(n) result(xy)
            implicit none
            integer, intent(in) :: n
            real, allocatable :: xy(:,:)
        end function readtxt
    end interface

    print*,"Enter the number of data-points:"
    read*, n
    data = readtxt(n)
    !main_function
    ! Initialize sums to zero
    sum_x = 0.0
    sum_y = 0.0
    sum_xy = 0.0
    sum_xx = 0.0
    do i=1,n,1
        x = data(i,1)
        y = data(i,2)
        sum_x = x+sum_x
        sum_y = y+sum_y
        sum_xy = sum_xy+(x*y)
        sum_xx = sum_xx+(x*x)
    enddo
    ! m and c estimation
    m = ((n*sum_xy)-(sum_x*sum_y))/((n*sum_xx)-(sum_x**2))
    c = (sum_y-m*sum_x)/n
    print*, "Equation of line"
    print*, "y=",m,"x +",c
endprogram line_fit