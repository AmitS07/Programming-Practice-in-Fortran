! This is excercises for Computer programming in Fortran-90/95 by V Rajaraman, ISBN;9788120311817, PHI Leaning, Year-1999
! Chapter-16: Additional Features in Procedures
! Date: 26th September 2025
! Author: Amit(amitdravid@outlook.com), The University of Alabama, AL, USA
! Note: To run and compile an example program-code, comment other program-codes
! ==============================================================================================================
! ! Example Program 16.2, Compute factorial
recursive subroutine factorial (n, fact_n, error_flag)
    implicit none
    integer, intent(in):: n 
    integer, intent(out):: fact_n
    logical, intent(out):: error_flag
    error_flag = .false.
    if (n<0) then
        error_flag = .true.
        return
    else if (n==0) then 
        fact_n = 1
    else 
        call factorial(n-1, fact_n, error_flag)
        fact_n = n*fact_n
    endif
end subroutine factorial

program facto 
    implicit none 
    integer:: m, fact_m
    logical:: error 
    print*, "Enter the number (for factorial):"
    read*, m 
    call factorial(m, fact_m, error)
    if (error) then
        print*,"opps!!", m, "not legal to compute!"
    else 
        print*, "The factorial of", m, ":", fact_m 
    end if 
end program facto
! ==============================================================================================================
! ! Example program# 16.6: Generic procedure with "Module"
module rotate_ele
    interface rotate
        module procedure rotate_int 
        module procedure rotate_char
    end interface   
    contains 
    subroutine rotate_int(a,b,c)
        implicit none 
        integer, intent(inout):: a,b,c
        integer:: temp
        temp = c; c = b;b = a; a = temp
    end subroutine rotate_int
    subroutine rotate_char(a,b,c)
        implicit none 
        character, intent(inout):: a,b,c
        character:: temp
        temp = c; c = b;b = a; a = temp
    end subroutine rotate_char
end module rotate_ele

program circ_elements
    use rotate_ele
    implicit none 
    integer:: x,y,z 
    character:: p,q,r 
    ! ineteger elements, and rotation
    print*, "Enter (3) integer elements :"
    read*, x,y,z 
    print*, "Ele#1:", x, "Ele#2:", y, "Ele#3:", z
    call rotate(x,y,z)
    print*,"After rotation:", "Ele#1:", x, "Ele#2:", y, "Ele#3:", z
    ! Character elements, and rotation
    print*, "Enter (3) Character elements :"
    read*, p,q,r 
    print*, "Char#1:", p, "Char#2:", q, "Char#3:", r
    call rotate(p,q,r)
    print*,"After rotation:", "Char#1:", p, "Char#2:", q, "Char#3:", r
end program circ_elements

! ==============================================================================================================
! Example program#16.8: Use of unary operator .rotate.
module rotate_vec
    interface operator (.rotate.)
    module procedure rot
    end interface
    ! begin function/subrotuine
    contains
    function rot(a) result(rot_a)
        implicit none 
        integer, dimension(:), intent(in):: a
        integer, dimension(size(a)):: rot_a
        integer:: i, n ! local variable
        n = size(a)
        rot_a(1) = a(n)
        do i = 1, n-1
            rot_a(i+1) = a(i)
        end do
    end function rot
end module rotate_vec
! Main program
program array_rotate
    use rotate_vec
    implicit none 
    integer:: i, m 
    integer, dimension(:), allocatable:: b, b_out
    print*, "Enter the number of elements in array:"
    read*, m 
    print*, "Enter",m," elements of the array:"
    allocate(b(m))
    allocate(b_out(m))
    read*, (b(i), i=1,m)
    b_out = .rotate.b 
    print*, "Rotated array:", (b_out(i), i=1,m)
end program array_rotate

! ==============================================================================================================
! ! Example program#16.11: An array valued function
function mod_sum_arr(arr_x, arr_y)
    implicit none
    integer, dimension(:):: arr_x, arr_y
    integer, dimension(size(arr_x)):: mod_sum_arr
    mod_sum_arr = MOD((arr_x+arr_y),5)
end function mod_sum_arr

program mod_sum_array5
    ! Note: MOD is Fortran intrinsic function to calculate remainder of a divison
    implicit none 
    integer, dimension(5):: arr_p, arr_q, arr_r
    interface
        function mod_sum_arr(arr_x, arr_y)
            implicit none
            integer, dimension(:):: arr_x, arr_y
            integer, dimension(size(arr_x)):: mod_sum_arr
        end function mod_sum_arr
    end interface
    print*,'Input elements of for array_p.each elements<5'
    read*,arr_p
    print*,'Input elements of for array_q.each elements<5'
    read*,arr_q
    if(size(arr_p)/=size(arr_q)) then
        print*, "Opps! input arrays are different, cant add"
    else
        arr_r = mod_sum_arr(arr_p,arr_q)
        print*, arr_r
    endif
end program mod_sum_array5

!! NOTE: It could be noticed that we have mentioned a explicit "interface" section in calling program, it make sure that exact format of the 
!! defined variables in functions/subroutine to be called in same order and fashion. For example- when a function has array, keyward arguments etc. 

! ==============================================================================================================
! ! Example program#16.13: Scope of variables names, locally 
program variables_scope
    ! This program is to test and understand the role of local and dummy variables 
    ! Note: Examine carefully about the vaiables in main program, function and subroutine
    implicit none
    integer::x,y,z,a,b,p 
    x = 16;y = 9;a=2;b=4;
    z = fun_v(x,y)*b 
    call sub_v(x,y,p)
    print*,"Values in main program"
    print*,"x = ",x,"y = ",y, "a = ",a, "b = ", b 
    print*, "z = ",z,"p = ", p 
    contains
    function fun_v(p,q)
        integer, intent(in)::p,q 
        integer:: fun_v 
        integer::b
        b = p*q 
        fun_v = a*sqrt(real(p))+b*sqrt(real(q))
        print*, "Values printed in fun_v"
        print*,"p = ",p,"q = ",q, "b = ",b, "a = ", a 
        print*, "fun_v = ", fun_v,"x = ",x,"y = ",y
    end function fun_v
    subroutine sub_v(p,q,r)
        integer, intent(in)::p,q 
        integer, intent(out)::r 
        r = b*sqrt(real(p)) + q*q 
        print*, "Values printed in sub_v"
        print*, "p = ",p,"q = ",q, "r = ", r ,"b = ",b 
    end subroutine sub_v
end program variables_scope

! ==============================================================================================================
! ! Example program#16.15: Scope of variables names, locally 
program save_variable_value
    ! This program shows the how to declare and save the variables's value
    ! Note:: Pay attention to "save" in subroutine
    implicit none
    integer::serial_no, age
    print*,"Enter Serial_no., age"
    do
        read *, serial_no, age
        call sum_good_age(age)
        if (serial_no==0) exit 
    end do
end program save_variable_value
subroutine sum_good_age(age)
    implicit none 
    integer::avg_age
    integer, intent(in)::age
    integer, save::good_data = 0, sum_age = 0
    if ((age<25).and.(age>3)) then
        good_data = good_data +1 
        sum_age = sum_age + age
    end if
    if (age==0) then 
        avg_age = sum_age/good_data
        print*,"No. of good data = ",good_data
        print*, "Average age = ", avg_age
    end if
end subroutine sum_good_age
