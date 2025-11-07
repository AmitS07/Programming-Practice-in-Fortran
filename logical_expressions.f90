! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-8: Logical Expressions and More Control Statements
! Date: 15th May 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example:8.2
program sharevote
    ! Defining variables
    implicit none
    logical:: agl_vot, bha_vot, cha_vot,measure ! Carefull, these are not "real" and "interger" variables
    integer:: serial
    ! puting into lop for examination
    do
        print*, "Enter the logical share-holder and serial "
        read*, agl_vot, bha_vot, cha_vot, serial
        print*, "*****************************************"
        if(serial==0) exit
        measure = (agl_vot.and.bha_vot).or.(agl_vot.and.cha_vot)
        if(measure)then
            print*,"Serial-No.",serial,"Measure:Passed"
        else
            print*,"serial-No.",serial,"Measure:Failed"
        endif
    enddo
endprogram sharevote


!-------------------------------------------------------------------------
function read_txt(filename, ncols) result(data_array)
    implicit none
    character(len=*), intent(in) :: filename
    integer, intent(in) :: ncols
    real, allocatable :: data_array(:,:)
    integer :: unit, iostat, nrows, i
    real :: dummy
    
    ! First count the rows
    nrows = 0
    open(newunit=unit, file=filename, status='old', iostat=iostat)
    if (iostat /= 0) then
        print*, "Error opening file: ", trim(filename)
        stop
    end if
    
    do
        read(unit, *, iostat=iostat) (dummy, i=1,ncols)
        if (iostat /= 0) exit
        nrows = nrows + 1
    end do
    close(unit)
    
    ! Now allocate and read
    allocate(data_array(nrows, ncols))
    open(newunit=unit, file=filename, status='old')
    do i = 1, nrows
        read(unit, *, iostat=iostat) data_array(i,:)
        if (iostat /= 0) then
            print*, "Error reading row ", i, " in file ", trim(filename)
            stop
        end if
    end do
    close(unit)
end function read_txt

program checking_service
    implicit none
    ! Declare the function interface
    interface
        function read_txt(filename, ncols) result(data_array)
            character(len=*), intent(in) :: filename
            integer, intent(in) :: ncols
            real, allocatable :: data_array(:,:)
        end function read_txt
    end interface
    
    ! Define variables with fixed lengths
    character(len=256) :: file_name  ! Fixed length string for filename
    integer :: emp_id, join_day, join_month, join_year
    integer :: today_day, today_month, today_year
    integer :: diff_day, diff_month, diff_year, ncols, i, nrows
    real, allocatable :: data(:,:)
    
    ! Reading data
    print*, "Enter day of comparison (day month year):"
    read*, today_day, today_month, today_year
    
    print*, "Enter the filename:"
    read '(a)', file_name  ! Read the filename as a string
    
    print*, "Enter number of columns in data file:"
    read*, ncols
    
    ! Read data from file
    data = read_txt(trim(file_name), ncols)
    nrows = size(data, 1)  ! Get number of rows from the array
    
    ! Process each employee
    do i = 1, nrows
        emp_id = int(data(i,1))
        join_day = int(data(i,2))
        join_month = int(data(i,3))
        join_year = int(data(i,4))
        
        ! Calculate differences
        diff_day = today_day - join_day
        diff_month = today_month - join_month
        diff_year = today_year - join_year
        
        ! Check eligibility
        if (diff_year > 1 .or. &
            (diff_year == 1 .and. diff_month > 0) .or. &
            (diff_year == 1 .and. diff_month == 0 .and. diff_day > 0)) then
            print*, "Emp-No.", emp_id, "Service: Eligible"
        else
            print*, "Emp-No.", emp_id, "Service: Not Eligible"
        end if
    end do
end program checking_service


! -----------------------------------------------------------------------------
! --- EXAMPLES WITH THE CASE STATEMNET
! -----------------------------------------------------------------------------
! EXampple: 8.6

program discount_price
    implicit none
    integer:: serial_n, prod_code
    real:: price, discount, final_amount
    print*, "Enter Serial No.,  Prod_code, price"
    do
        read*,serial_n,prod_code,price
        if (serial_n==0) exit
        discount = 0.0
        select case(prod_code)
        case(1)
            if (price>=5000) discount =0.1
        case(2)
            if (price>=2000) discount =0.1
        case(3)
            if (price>=1000) discount =0.1
        case default
        end select
    final_amount = price*(1-discount)
    print*, serial_n, prod_code, final_amount
    end do
end program discount_price

EXampple: 8.8
program days_months
    implicit none
    integer:: days, month, year
    logical:: leap_year
    do
        print*, "Month: Year: "
        read*, month, year
        if (year==0) exit
        if(MOD(year,400)==0) then
            leap_year = .true.
        else if (MOD(year,100)==0) then
            leap_year = .false.
        else if (MOD(year,4)==0) then
            leap_year = .true.
        else
            leap_year = .false.
        end if 
        ! Select case
        select case(month)
        case(1,3,5,7,8,10,12)
            days = 31
            print*, "Days in this months:", days
        case(4,6,9,11)
            days = 30
            print*, "Days in this months:", days
        case(2)
            select case(leap_year)
            case(.true.)
                days = 29
                print*, "Days in this months:", days
            case(.false.)
                days = 28
                print*, "Days in this months:", days
            end select
        case default
        end select
    end do
end program days_months

! EXampple: 8.7
program quadratic_case
    ! Defining variables
    implicit none
    real:: a,b,c,d, p1,p1_1, p2, r1,r2
    integer:: root_code
    real, parameter:: e =1e-7
    ! Importing values
    print*, "Enter the values of a"
    read*, a
    print*, "Enter the values of b,"
    read*, b
    print*, "Enter the value of c"
    read*, c
    ! establishing the case
    d = (b**2)-(4*a*c)
    if (d<0) then
        root_code = 1
    else if (ABS(d)<=e) then
        root_code = 2
    else 
        root_code = 3
    end if
    ! Examine the cases
    select case(root_code)
    case(1)
        d = -(d)
        p1 = SQRT(d)/(2*a)
        p1_1 = -p1
        p2 = -b/(2*a)
        print*,"Two real, imaginary roots:", p2, p1, p1_1
    case(2)
        p2 = -b/(2*a)
        print*, "Two real repeated roots:", p2, p2
    case(3)
        p1 = SQRT(d)/(2*a)
        p2 = -b/(2*a)
        r1 = p2+p1
        r2 = p2-p1
        print*, "Two equal-real roots", r1, r2
    end select
end program quadratic_case

