! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-6: Conditional-Statements
! Date: 6th April 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example:6.4
program largest_in_three
    ! Difining variables
    implicit none
    integer:: a,b,c
    ! Importing-values
    print*, "Enter three values:"
    read*, a,b,c
    ! selecting-larger values, by if-else
    if (a>b) then
        if (a>c) then
            print*,"The Largest value is:",a
        else 
            print*,"The Largest value is:",c
        endif
    else if (b>c) then
        print*,"The Largest value is:",b
    else 
        print*,"The Largest value is:",c
    endif
end program largest_in_three


! Example:6.6
program roots_quadratic
    ! Defining variables
    implicit none
    real:: a,b,c,d, p1,p1_1, p2, r1,r2
    real, parameter:: e =1e-7
    ! Importing values
    print*, "Enter the values of a"
    read*, a
    print*, "Enter the values of b,"
    read*, b
    print*, "Enter the value of c"
    read*, c
    ! Doing analysis
    d = (b**2)-(4*a*c)
    if (d<0) then
        d = -(d)
        p1 = SQRT(d)/(2*a)
        p1_1 = -p1
        p2 = -b/(2*a)
        print*,"Two real, imaginary roots:", p2, p1, p1_1
    else if (ABS(d)<=e) then
        p2 = -b/(2*a)
        print*, "Two real repeated roots:", p2, p2
    else 
        p1 = SQRT(d)/(2*a)
        p2 = -b/(2*a)
        r1 = p2+p1
        r2 = p2-p1
        print*, "Two equal-real roots", r1, r2
    endif
end program roots_quadratic

! Example-6.7
program income_tax
    ! Defiining variable
    implicit none
    real:: income, tax 
    ! Reading values to the variables
    print*, "Enter the income:"
    read*, income
    ! Tax analysis
    if (income<=35000) then
        tax = 0
        print*, "with this income tax is:", tax
    else if (income>35000 .and. income<=60000) then
        tax = 0.2*(income-35000)
        print*, "with this income tax is 20%", tax
    else if (income>60000.and. income<=120000) then
        tax = 0.3*(income-60000) +  0.2 * (60000 - 35000) 
        print*, "with this income tax is 30%", tax
    else
        tax = 0.2 * (60000 - 35000) + 0.3 * (120000 - 60000) + 0.4 * (income - 120000)
        print*, "with this income tax is 40%", tax
    endif ! Important to end if-else
end program income_tax