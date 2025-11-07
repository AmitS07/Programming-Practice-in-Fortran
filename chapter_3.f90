! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-3: Numerical Constrants anad Variables
! Date: 26th March 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!Program 3.1
!Height to which water is filled
program tub_fill
    ! Defining varaiables
    implicit none !to  make sure no variable namw has a default type
    real:: radius, initial_water_height, time, final_height_water,flow_rate
    real, parameter:: pi = 3.14159265
    ! Importing data sets
    print*,"Radius of the tub:"
    read*, radius
    print*,"initial water height in tub:"
    read*, initial_water_height
    print*, "Time of the water flow:"
    read*, time
    print*, "Water flow rates:"
    read*, flow_rate
    !Analysis
    final_height_water = initial_water_height+(flow_rate*time)/(pi*radius*radius)
    print*,"Height of water in tub after time (m)", time,"is",final_height_water

end program tub_fill



