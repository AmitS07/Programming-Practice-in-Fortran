! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-9: FUnctions and Subroutines-Basics
! Date: 12th June 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example 9.1 (Example-Program 9.3) Learn writing functions
!! Writing function
function S(x) result(V)
    implicit none
    real:: V
    real, intent(in) :: x ! intent(in) indicates  x will be used as input and "read-only" argument
    if(x<-3.0) then
        V = -3.0
    else if (x>3.0) then
        V = 3.0
    else
        V = x
    end if
end function S

!! Program 
program fun_s
    implicit none
    real :: a,b,z,S 
    print*,"Enter a,b"
    read*, a,b
    ! Calculation, calling function S(x)
    z = (S(a)+S(b))/S(a+b)
    print*,"Function value:",z
end program fun_s

!-------------------------------------------------------------------------
! Example 9.4 (Example program-9.7)
function reverse_digit(n)
    implicit none
    integer:: reverse_digit, digit,no
    integer, intent(in):: n
    reverse_digit = 0
    no = n
    do 
        if (no==0) exit
        digit = MOD(no,10)
        reverse_digit = reverse_digit*10 + digit
        no = no/10
    end do
end function reverse_digit

program reverse_number
    implicit none
    integer::reverse_digit, a,b 
    print*, "Enter the digit:"
    read*, a 
    b = reverse_digit(a)
    print*, "Reversed Number:",b
end program reverse_number

!!!!-------------------------------------------------------------------------
! Learning Subroutine: The subrutines are generally written to account for all posisble cases, see the example below
!  Example program 9.8: Subroutine to find the roots of a quadratic equation

Subroutine quad_roots(a,b,c,x_real1,x_real2,x_img1,x_img2,i)
    implicit none
    ! Defining all variables types
    real:: discr, d
    real, intent(in):: a,b,c
    real, intent(out):: x_real1,x_real2,x_img1,x_img2
    integer, intent(out):: i
    real,parameter::epsilon = 0.5e-6 ! i.e. ~ zero
    ! Calculation
    if (abs(a)<= epsilon) then
        x_real1 = -c/b; x_real2 = 0
        x_img1 = 0 ; x_img2 = 0
        i = 1
        return
    end if
    discr = b*b-4*a*c
    if(discr>=0) then
        d = sqrt(discr)
        x_real1 = (-b+d)/(2*a); x_real2 = (-b-d)/(2*a)
        x_img1 = 0 ; x_img2 = 0
        i = 2
    else
        d = sqrt(-discr)
        x_real1 = -b/(2*a); x_real2 = x_real1
        x_img1 = d/(2*a) ; x_img2 = -1*x_img1
        i = 3
    end if 
end subroutine quad_roots

!! Now the quadratic roots calculation for given coefficnet of quadratic equation, usng above subroutine
! Example Program 9.9:
program finding_quad_roots
    implicit none
    ! Defining varibales
    integer:: serial, flag
    real:: p,q,r,x_r1,x_r2,x_i1,x_i2 
    do 
        read*, serial, p,q,r 
        print*, "Serial",serial, "p",p, "q",q, "r",r
        if(serial==0) exit
        call quad_roots(p,q,r,x_r1,x_r2,x_i1,x_i2, flag)
        select case(flag)
        case(1)
            print*,"Only one Root:",x_r1
        case(2)
            print*,"Two-real Roots:"
            print*,"Root1:",x_r1, "Root2:",x_r2
        case(3)
            print*,"Complex roots"
            print*,"Root1:",x_r1,"+i",x_i1
            print*,"Root2:",x_r2,"+i",x_i2
        end select
    end do
end program finding_quad_roots

!!!-------------------------------------------------------------------------------------
! Example program: 9.11
program student_results
    ! main Program
    implicit none
    real:: Mark1, Mark2, Mark3, total, avg
    integer:: roll_no, no_records, bad_record, divison
    logical:: err 
    no_records = 0
    do 
        print*, "Enter Roll No.", "Mark1", "Mark2", "Mark3"
        read*, roll_no, Mark1, Mark2, Mark3
        if (roll_no==0) exit
        no_records = no_records+1
        call check_marks
        if(err) cycle
        call record 
        print*, "Roll No.:", roll_no, "Marks 1:", Mark1, "Marks 2:", Mark2,"Marks 3:", Mark3, &
                "Total Marks:", total, "Average Marks:", avg, "Divison:", divison
    end do
    print*,"No. of records processed:", no_records
    print*,"No. of bad records: ", bad_record
    ! Now, writing Sub routines
    contains ! This is to indicate the subroutines used in this program are internal
    subroutine check_marks
        err = .false.
        if ((Mark1>100).or.(Mark1<1)) err = .true.
        if ((Mark2>100).or.(Mark2<1)) err = .true.
        if ((Mark3>100).or.(Mark3<1)) err = .true.
        if (err) then
            Print*, "Error in the Record with Roll No.:", roll_no
            bad_record = bad_record+1
        end if
    end subroutine check_marks

    subroutine record
        total = Mark1+ Mark2+ Mark3
        avg = total/3
        if (avg>=60) then
            divison = 1
        else if(avg>=50) then
            divison = 2
        else if(avg>=40) then
            divison = 3
        else
            divison = 0
        end if
    end subroutine record
end program student_results





