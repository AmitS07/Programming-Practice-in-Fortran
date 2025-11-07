! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-18: Use of Modules
! Date: 3rd November 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
!! Example program 19.1: Use of Module for vectors 
module vector_data
    implicit none
    integer, parameter::max_comp=12
    type vector 
        integer:: no_comp
        real, dimension(max_comp):: components
    end type vector
    ! For dot product
    interface operator (.dot.)
        module procedure scalar_product
    end interface
    ! scalar prodcut to vector
    interface operator (*)
        module procedure r_times_vec
    end interface
    ! 
    contains
    type(vector) function create_vec(array,n) ! n dimension of vector
    integer, intent(in)::n 
    real, dimension(n), intent(in):: array 
    if (n>max_comp) then 
        print*, "No. of elements exceeds given max- number"
        create_vec%no_comp = 0
    else 
        create_vec%no_comp = n
        create_vec%components(1:n) = array(1:n)
    end if
    end function create_vec 
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Function to find the no. of components in a vector
    integer function vector_size(vec)
        type(vector), intent(in)::vec 
        vector_size = vec%no_comp
    end function vector_size
    ! Functions to put the elements to a vector to in a array
    function vec_array(vec)
        type(vector), intent(in):: vec 
        real, dimension(vec%no_comp) :: vec_array(vec%no_comp)
        vec_array(1:vec%no_comp) = vec%components(1:vec%no_comp)
    end function vec_array
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Vector product function
    real function scalar_product(v1,v2)
    type(vector), intent(in)::v1,v2
    real::dot_prod
    integer::i 
    dot_prod = 0.0 
    if (v1%no_comp /=v2%no_comp) then 
        print*,"v1 and v2 are in differet size"
        scalar_product = 0.0
    else 
        do i=1,v1%no_comp 
            dot_prod = dot_prod + v1%components(i)*v2%components(i)
        end do 
        scalar_product = dot_prod
    end if 
    end function scalar_product
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !  Functon to find magnitude of a vector 
    real function magnitude(vec)
    type(vector), intent(in):: vec 
    real :: sum_sq=0
    integer :: i
    do i=1, vec%no_comp
        sum_sq = sum_sq + vec%components(i)**2
    end do 
    magnitude = sqrt(sum_sq)
    end function magnitude 
    !  a vector multipication to vector
    type(vector) function r_times_vec(r,vec)
        type(vector), intent(in):: vec
        real, intent(in) ::r 
        integer :: i
        r_times_vec%no_comp = vec%no_comp 
        do i = 1, vec%no_comp 
            r_times_vec%components(i) = vec%components(i)*r 
        end do 
    end function r_times_vec
end module vector_data
!!! Program too test the module-vector
program test_vector_data 
    use vector_data 
    implicit none 
    real :: r_t=2.5
    real, dimension(5)::ar_1 
    real, dimension(8)::ar_2
    type(vector)::vec_1, vec_2
    ar_1 = (/1.5,2.5, 3.6,7.8,9.4/)
    ar_2 = (/2.6,3.6,4.2,7.6,2.5,4.2,3.6,4.7/)
    vec_1 = create_vec(ar_1,5)
    vec_2 = create_vec(ar_2,5)
    ! Caluclation
    print*, "No. of components in Vec-1 = ", vector_size(vec_1)
    print*, "No. of components in Vec-2 = ", vector_size(vec_2)
    print*, "Components of vec_1:", vec_array(vec_1)
    print*, "Components of vec_2:", vec_array(vec_2)
    print*, "Dot product of vec_1 and vec_2:", vec_1.dot.vec_2 
    print*, "Scalar product of vec_1 and vec_2:", scalar_product(vec_1, vec_2)
    print*, "Magnitude of vec_1:", magnitude(vec_1)
    print*, "Magnitude of vec_2:", magnitude(vec_2)
    vec_2 = r_times_vec(2.5,vec_1)
    print*, "2.5*vec_1 = ", vec_array(vec_2)
    print*, "Test of overload operator"
    vec_2 = r_t*vec_1
    print*, "2.5*vec_1 = ", vec_array(vec_2)
end program test_vector_data
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Example program 19.2: Use of module to simulate stack
module stack_structure
    implicit none 
    integer, parameter:: max_size=100
    real, dimension(1:max_size)::stack 
    integer::stack_top=0
    logical::stack_full, stack_empty
    contains
    subroutine push(pushed_no)
        real, intent(in)::pushed_no
        logical::stack_full, stack_empty
        if(stack_full) then 
            print*, "Stack full, cant Push number"
            return
        end if
        stack_top = stack_top +1
        stack(stack_top) =pushed_no 
        stack_empty = .false. 
        if (stack_top == max_size) then
            stack_full = .true.
        end if 
    end subroutine push
    subroutine pop(pop_no)
        real, intent(out)::pop_no
        if (stack_empty) then
            print*, "Stack empty, cant pop number!"
            return 
        end if 
        pop_no = stack(stack_top)
        stack_top = stack_top-1
        stack_full = .false. 
        if (stack_top<0) then
            stack_empty = .true. 
        end if 
    end subroutine pop
end module stack_structure
