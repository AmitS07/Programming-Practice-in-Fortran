! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-15: Derived (Data) Types
! Date: 17th August 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example program 15.2: Finding completion of one year service
program serice_derived_version
    implicit none
    ! Defining Derived Type variable
    type date 
        integer:: day, month, year
    end type date
    ! Defining variables
    integer:: emp_no !Empoly number
    type(date):: today_date, joining_date, diff_date
    ! Reading variable values
    print*,"Today date (D/M/Y):"
    read*, today_date
    do 
        print*,"Employee number:"
        read*, emp_no
        if (emp_no == 0) exit 
        print*,"Joining date (D/M/Y):"
        read*, joining_date 
        diff_date%day = today_date%day - joining_date%day 
        diff_date%month = today_date%month - joining_date%month
        diff_date%year = today_date%year - joining_date%year
        if ((diff_date%year > 1).or. &
            ((diff_date%year == 1).and.(diff_date%month > 0)).or.&
            (((diff_date%year == 1).and.(diff_date%month == 0)).and.(diff_date%day >=0))) then
            print*,"Employee no.", emp_no, "eligible"
        else 
            print*,"Employee no.", emp_no, "not eligible"
        end if 
    print*,"++++++++++++++++++++++++++++"
    end do
end program serice_derived_version
! ==============================================================================================================
! Example program 15.4: Use of derived type in array
program structure_derived_array
    implicit none 
    type item
        integer:: item_code
        character (len=20):: item_name
        integer::qty_in_stock
        real:: price
    end type item
    type(item), dimension(10):: inventory 
    integer:: no_items, i 
    real::invent_value=0
    print*,"Enter the Number of items in store:"
    read*, no_items 
    do i=1, no_items
        print*,"Enter details of iteam (Code|Name|Qunt|Price):",i
        read*,inventory(i)
        if(inventory(i)%qty_in_stock == 0) then 
            print*,"Item-code:", inventory%item_code,"Out of stock"
        else
            invent_value = invent_value + inventory(i)%qty_in_stock*inventory(i)%price
        end if
        print*,"++++++++++++++++++++++++++"
    end do
    print*,"Inventory Value:", invent_value
end program structure_derived_array
! ==============================================================================================================
! Example Program 15.6 Printing Supplier details from supplier codes
module supplier_details
    implicit none 
    !  Derived type array
    type address
        character(len=20):: street 
        character(len=15):: city
        integer:: pin_code 
    end type address
    type supplier 
        integer:: supp_code
        character(len=20):: supp_name 
        type(address)::supp_address
    end type supplier
end module supplier_details

subroutine print_address(supp_group, in_code, no_supp)
    use supplier_details
    implicit none 
    integer, intent(in):: in_code,no_supp
    type(supplier), dimension(no_supp), intent(in)::supp_group 
    integer:: i 
    do i = 1, no_supp
        if (in_code == supp_group(i)%supp_code) then
            print 99, supp_group(i)%supp_name, supp_group(i)%supp_address%street, & 
                    supp_group(i)%supp_address%city, supp_group(i)%supp_address%pin_code
                    99  format(1X, A20 / A20 / A15 / 'PIN', I7)
        end if
    end do
end subroutine print_address
!  Note the above print statement line is divided in to two lines becasue fotran 90/95/2003, has maximum 132 charcter lengths
program supplier_data_base
    use supplier_details
    implicit none 
    type(supplier), dimension(50)::supp_group 
    integer :: code, i, no_supp 
    print*, "Enter the number of suppliers:"
    read*, no_supp
    do i=1,no_supp
        print*,"Enter Supplier code:"
        read*,supp_group(i)%supp_code
        print*,"Enter Supplier Name:"
        read*,supp_group(i)%supp_name
        print*,"Enter Supplier street:"
        read*,supp_group(i)%supp_address%street
        print*,"Enter Supplier city:"
        read*,supp_group(i)%supp_address%city
        print*,"Enter Supplier city' pin code:"
        read*,supp_group(i)%supp_address%pin_code
        print*, "Next Supplier details:"
    end do
    print*,no_supp, "Data Input"
    do i = 1,3
        print*,"Enter code of suppler of out of stock item"
        read*, code 
        print*, "Address of supplier with code:", code 
        call print_address(supp_group, code, no_supp)
    end do
end program supplier_data_base