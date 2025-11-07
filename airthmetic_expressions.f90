! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-4: Airthmetic Expressions
! Date: 29th March 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! Example 4.17
program expression
    ! Define variables
    implicit none
    real:: alpha, omega, phi, t, result
    ! Input values to variables
    print*, "Alpha angle:"
    read*, alpha
    print*, "Omge-Angle:"
    read*, omega
    print*, "Phi-angle:"
    read*, phi
    print*, "time:"
    read*, t
    ! Expression
    result = alpha*COS(omega*t+phi)/(alpha**2 + omega**2)**0.5
    ! Output results
    print*, "The Expression results is", result
end program expression
!!---------------------------------------------------------------------------------!!
! Example 4.19
program expression2
    !Defining Variavbles
    implicit none
    real:: a, alpha, x, result
    ! Values of the variables
    print*, "Value for a:"
    read*, a
    print*, "alpha_value:"
    read*, alpha
    print*, "value of x:"
    read*, x
    ! Expression
    result = (1-EXP(-alpha*SQRT(x)))/(1+x*EXP(-ABS(x)))
    ! Output
    print*, "Expression result:", result
end program expression2

!!---------------------------------------------------------------------------------!!
! Example:4.22, Area Of triangle and length of one -side of triangle
program triangle
    ! Defining Variables
    implicit none
    real:: a, b, c, theta, theta_rad, area
    real, parameter:: pi = 3.14159265
    ! Importing values to variables
    print*, "Enter  given sides of Triangle: a, b"
    read*, a
    print*, "Enter other side of the Trangle: b"
    read*, b
    print*, "Enter angle b/w a and b (in degree): theta"
    read*, theta
    ! Calculations
    !  convert degree to radian
    theta_rad = pi*theta/180
    ! 3rd side of the triangle
    c = SQRT(b**2 + a*(a-2*b*COS(theta_rad)))
    area = 0.5*a*b*SIN(theta_rad)
    ! Print results
    print*, "The length of 3rd side of the triangle is", c
    print*, "The Area of the Triangle is", area

end program triangle


!!---------------------------------------------------------------------------------!!
! Example 4.23, Adding digits of a number
program adding_digits
    ! Defining variables
    implicit none
    integer:: number, n, digit1, digit2, digit3, digit4, sum
    ! Importing values and reading
    print*, "Enter 4 digit number:"
    read*, number
    n = ABS(number)
    ! Adding the digits
    digit1 = n-(n/10)*10
    n = n/10
    digit2 = n-(n/10)*10
    n = n/10
    digit3 = n-(n/10)*10
    n = n/10
    digit4 = n
    sum = digit1+digit2+digit3+digit4
    ! Output
    print*, "Sum of  digits:", sum
end program adding_digits
