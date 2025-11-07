! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-2: Use of Modules
! Date: 29th March 2024
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!Program 2.1
!This is find area and perimeter
program area_perimeter
    real:: p,q,area,perimeter
    print*, "Enter length:"
    read*, p
    print*, "Enter width:"
    read*, q

    area = p*q
    perimeter = 2*(p+q)

    print*, "Area =",area , "Perimeter =",perimeter
end program area_perimeter

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Program 2.4
! This is to convert Celsius to Fahrenheit
program temp_conversion
    real:: cel,fah 
    print*, "Enter temp in Celsius:"
    read*, cel 
    fah = 1.8*cel+32
    print*,"Temp. in Fahrenheit:", fah
end program temp_conversion