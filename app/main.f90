! 
! FNote: Command-line notebook application
! Module: Main program, with helper procedures for command-line argument
!         handling.
! 

program main
    use fnote
    use note_mod
    implicit none
    
    integer, parameter :: NEWNOTE = 1
    integer, parameter :: LISTNOTES = 2
    integer, parameter :: REMOVENOTE = 3
    integer, parameter :: INVALID_CMD = 0
    
    integer :: arg_num
    character(len=:), allocatable :: arg_txt
    integer :: cmdcode
    type(note_t) :: note
    integer :: unum
    integer :: rstat
    integer :: note_num
    
    unum = open_file('r')
    call read_notes(unum)
    call close_file(unum)
    
    arg_num = command_argument_count() 
    
    ! Get command-line argument, result --> arg_txt.
    call get_arg(1, arg_txt)
    
    ! Read argument string for command.
    cmdcode = parse_cmd(arg_num, arg_txt)
    call dealloc_arg(arg_txt)
    
    ! Execute operation.
    select case (cmdcode)
    case (NEWNOTE)
        call get_arg(2, arg_txt)
        note = note_t(id_num=find_free_idnum(), msg=arg_txt)
        call dealloc_arg(arg_txt)
        
        unum = open_file('a')
        call add_note(unum, note)
        call close_file(unum)
        
    case (LISTNOTES)
        call list_notes()
        
    case (REMOVENOTE)
        call get_arg(2, arg_txt)
        read(unit=arg_txt, fmt='(i4)',iostat=rstat) note_num
        if (rstat == 0) then
            call remove_note(note_num)
        else
            call usage('invalid ID number: "' // arg_txt // '".')
        end if
        
    case default
        call say_hello()
        call usage()
    end select
    
    call dealloc_arg(arg_txt)
    
    call clean_notes()
    
contains
    
    !  
    !  name: parse_cmd
    !  desc: Helper function for parsing command-line argument.
    !  @param numargs: number of cmd-line arguments.
    !  @param str: cmd-line parameters as a character string. 
    !  @return: the command code as an integer number.
    !  
    function parse_cmd(numargs, str) result(cmdcode)
        integer, intent(in) :: numargs
        character(len=:), allocatable, intent(inout) :: str
        integer :: cmdcode
        
        cmdcode = INVALID_CMD
        if ( (numargs > 1) .and. &
             ((str == '--new') .or. (str == '-n')) ) then
            cmdcode = NEWNOTE
        end if
        
        if ( (str == '--list') .or. (str == '-l') ) then
            cmdcode = LISTNOTES
        end if
        
        if ( (numargs > 1) .and. &
             ((str == '--remove') .or. (str == '-r')) ) then
            cmdcode = REMOVENOTE
        end if
    end function
    
    
    !  
    !  name: get_arg
    !  desc: Reads the n'th cmd-line argument from the environment and 
    !        puts it into the "str" output argument.
    !  @param n: number of argument to read. Range 1..num of args.
    !  @param str: the corresponding argument is placed here.
    !  
    subroutine get_arg(n, str)
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: str
        integer :: arg_num
        integer :: arg_len
        
        arg_num = command_argument_count()
        
        if ( (n <= arg_num) .and. (n > 0) ) then
            call get_command_argument(number=n, length=arg_len)
            allocate( character(len=arg_len) :: str ) 
            call get_command_argument(number=n, value=str)
        end if
    end subroutine
    
    
    !  
    !  name: dealloc_arg
    !  desc: Helper subroutine for deallocating a character string.
    !  @param str: character string to de-allocate.
    !  
    subroutine dealloc_arg(str)
        character(len=:), allocatable, intent(inout) :: str
        if (allocated(str)) then
            deallocate(str)
        end if
    end subroutine
    
    
    !  
    !  name: usage
    !  desc: Helper subroutine for displaying a brief help message.
    !  @param str: optional text message to show to the user.
    !  
    subroutine usage(str)
        character(len=*), intent(in), optional :: str
        if ( present(str) ) then
            print '(2x,a)', str
        end if
        print *, 'Usage:'
        print *, '  fnote --new "my note": create a new note'
        print *, '  fnote --list: list all notes'
        print *, '  fnote --remove <id>: remove a note'
    end subroutine
end program main


































