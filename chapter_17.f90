! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-17: Processing Files in Fortran
! Date: 27th October 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! ! Exanmple- 17.1 Creating a Sequential file in Fortran
program create_file
    implicit none
    integer:: roll_no, marks, ierr ! ierr = input error
    character(len=25)::name
    integer, parameter:: dsk = 20
    ! Open command to open the file
    open(unit = dsk, File="Student_file.txt", status = "replace", action = "write", form = "formatted", iostat= ierr) !iostat  = input/output statement
    ! Check file status
    if (ierr/=0) then
        print*, "File dsk cannot be open, IOSTAT =", ierr
        stop
    end if
    !--------------------------------------------
    print*, "Enter roll_no, name, and mark (enter 0 to stop):"
    do
        read*, roll_no, name, marks 
        if (roll_no ==0) exit
        ! Write using fixed format: 4-digit roll_no, 25-char name, 3-digit mark
        write (dsk,  '(I4,1X,A25,1X,I3)') roll_no, name, marks
    end do
    close(dsk)
    print*, "Data saved successfully."
end program create_file

! --------------------------------------------------------------------------------------------------------------------------------
! ! Example-17.3 Searching the saved data in Sequential file
program search_file
    implicit  none
    integer:: roll_no, marks, search_no, ierr
    integer, parameter:: dsk = 20
    character(len=25):: name
    ! Read unit number and roll number to search
    print*, "Enter roll number to search:"
    read*, search_no
    open(unit = dsk, File="Student_file.txt", status = "old", iostat= ierr)
    ! Check file status
    if (ierr/=0) then
        print*, "File dsk cannot be open, IOSTAT =", ierr
        stop
    end if
    !--------------------------------------------
    do 
        read(dsk, '(I4,1X,A25,1X,I3)', iostat=ierr) roll_no, name, marks
        if (ierr < 0) then
            print*, "End of file reached. Roll number", search_no, "not found."
            exit
        end if
        if(roll_no == search_no) then
            print*, "Record of the file found"
            print*, roll_no, name, marks
            exit
        end if
    end do
    close(dsk)
end program search_file
! --------------------------------------------------------------------------------------------------------------------------------
! --------------------------------------------------------------------------------------------------------------------------------
!! Example program 17.6: Program to create a "Direct" random file
program direct_rand_file
    implicit none
    integer:: roll_no, marks(3)
    character(len=25):: name
    integer, parameter:: dsk = 21
    !  Opeing file
    open(unit = dsk, file = "student_data_rand.txt", access = "direct", recl = 37, form = "formatted") ! recl = record size
     print*, "Enter roll_no, name, and mark (enter 0 to stop):"
    do 
        read(*, "(I3,A25,3I3)") roll_no, name, marks
        if(roll_no ==0) exit
        write(dsk, "(I3,A25,3I3)", rec = roll_no)roll_no, name, marks ! rec = key to the file
    end do
    close(dsk)
    print*, "Data saved successfully."
end program direct_rand_file
!%% Reading the Saved file
program read_direct_file
    implicit none
    integer :: roll_no, marks(3)
    character(len=25) :: name
    integer, parameter :: dsk = 21

    open(unit=dsk, file="student_data_rand.txt", access="direct", recl=37, form="formatted")
    print*, "Enter roll number to read:"
    read*, roll_no
    read(dsk, "(I3,A25,3I3)", rec=roll_no) roll_no, name, marks
    print*, "Roll No:", roll_no
    print*, "Name:", name
    print*, "Marks:", marks
    close(dsk)
end program read_direct_file

!! Example-Program 17.7: Updating the "Direct" access random saved file
program update_dir_rand_file
    implicit none 
    integer :: roll_no, marks(4), i, total_marks
    character(len=25) :: name
    integer, parameter :: dsk = 15
    integer :: ios
    ! Open file with larger record length (for 5 integers now)
    open(unit = dsk, file = "student_data_rand.txt", access = "direct", recl = 43, form = "formatted", iostat=ios)
    if (ios /= 0) then
        print*, "Error opening file, IOSTAT =", ios
        stop
    end if
    print*, "Enter roll_no and new subject mark (0 to stop):"
    do
        read(*, '(2I3)') roll_no, marks(4)
        if (roll_no == 0) exit
        ! Try reading existing record for that roll_no
        read(dsk, '(I3,A25,3I3)', rec = roll_no, iostat=ios) roll_no, name, (marks(i), i=1,3)
        if (ios /= 0) then
            print*, "Record not found for roll no.", roll_no
            cycle
        end if
        ! Compute total and update
        total_marks = sum(marks)
        write(dsk, '(I3,A25,5I3)', rec = roll_no) roll_no, name, marks, total_marks
        print*, "Record updated for roll no.", roll_no
    end do

    close(dsk)
    print*, "All updates complete."
end program update_dir_rand_file

