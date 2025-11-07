! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-12: Processing Strings of Characters
! Date: 11th August 2025, 
! Author: Amit(amitdravid@outlook.com), The University of Alabama, Al, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!  Example program:: 12.3, Counting 'a' or 'A' in Names
program a_count_name
    implicit none
    character(len=40)::name, temp_name
    integer:: count, name_length, i 
    outer : do
        read 40, temp_name
        40 format(A40)
        name = trim(temp_name)
        if (name == "End_Names") exit 
        name_length = len_trim(name)
        count = 0
            inner : do i=1, name_length
            if(name(i:i)=="a".or.name(i:i)=="A")count = count +1
            end do inner
        print 50, temp_name, count
        50 format(1X, A40, 2X, "Count of a's = ", 2X, I2)
    end do outer
end program a_count_name

! Example Program:: 12.5, Inserting a string in a given string
program insert_string
    implicit none
    integer:: p
    character(len=40):: given_string, string_to_insert 
    character(len=80):: new_string
    print*, "At what position string to insert:"
    print*, "Input: Given string and String to be inserted:"
    read*, p, given_string, string_to_insert
    new_string = given_string(1:p-1)//trim(string_to_insert)//given_string(P:)
    print*,"Inserted string now:", new_string
end program insert_string