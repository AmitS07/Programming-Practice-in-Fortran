! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-18: Pointer Data Type and Applications
! Date: 31st October 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!! Example program 18.2
program create_list
    implicit none
    character:: data
    type ch_node
        character::letter 
        type(ch_node), pointer::next 
    end type ch_node
    type(ch_node),target::new_node
    type(ch_node), pointer::ch_list,tail_ptr, ptr
    nullify(ch_list, tail_ptr, ptr)
    ch_list=>new_node;tail_ptr=>new_node
    print*,"Type a letter, enter blank if no more letters"
    do
        read 10, data
        10 format(A1) !It select 1st letter from left side
        if(data== " ")exit
        tail_ptr%letter = data
        allocate(tail_ptr%next)
        tail_ptr=>tail_ptr%next
    end do
    nullify(tail_ptr%next)
    ! Printing list
    if(.not.associated(ch_list)) then 
        print*, "List is empty"
    else
        ptr=>ch_list
        do 
            print*,ptr%letter
            ptr=>ptr%next
            if(.not.associated(ptr)) exit
        end do
    end if
end program create_list
! --------------------------------------------------------------------------------------------------------------------------------
! Example program 18.4, Picking duplicate number from list using "Binary-tree"
! Note: Binary tree can also be used to sort the elements from the list
program duplicate_removal
    type node 
        integer:: value
        type(node), pointer:: left, right
    end type node
    type(node), pointer:: tree
    integer:: number 
    nullify(tree) ! to start with empty tree 
    print*, "Enter the elements of list with space, type -1 to end:"
    do 
        read*, number
        if (number==-1) exit
        call build_tree(tree, number)
    end do 
    contains 
    ! Subroutine to make tree
    recursive subroutine build_tree(tree, number)
    implicit none 
    type(node), pointer:: tree
    integer, intent(in):: number 
    if(.not.associated(tree)) then 
        allocate(tree)
    tree%value = number 
    nullify(tree%left, tree%right)
    else if(number<tree%value) then 
        call build_tree(tree%left, number)
    else if(number>tree%value) then 
        call build_tree(tree%right, number)
    else 
        print*, "opps! Number =",number, "is duplicate"
    end if
    end subroutine build_tree
end program duplicate_removal
