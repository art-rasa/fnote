module fnote
    use note_mod
    implicit none
    
    !character(len=:), parameter :: FILENAME = 'fnote.json'
    character(len=*), parameter :: FILENAME = 'fnote.txt'
    
    integer :: num_notes
    type(note_t), dimension(:), allocatable :: notes
    
contains
    
    function find_free_idnum() result (available_id)
        integer :: available_id
        integer :: i, j, idnum
        integer :: lowest, highest, searchspace
        logical :: found
        
        available_id = 1
        found = .false.
        lowest = 9999
        highest = 1
        
        if (.not. allocated(notes)) then
            return
        end if
        
        ! Find lowest & highest ID
        call find_min_max_idnums(lowest, highest)
        
        searchspace = highest - lowest
        
!~         print *, 'lowest:', lowest
!~         print *, 'highest:', highest
!~         print *, 'searchspace', searchspace
        
        ! Easy case; use lowest-1 as ID:
        if (lowest > 1) then
            available_id = lowest - 1
        
        ! Find the lowest unoccupied ID between low & high:
        else if (searchspace > 1) then
            do idnum = lowest + 1, highest - 1
                if ( is_idnum_free(idnum) ) then
                    found = .true.
                    available_id = idnum
                    exit
                end if
            end do
            
            if (.not. found) then
                available_id = highest + 1
            end if
            
        ! If no IDs were available:
        else
            available_id = highest + 1
            
        end if
        
!~         print *, 'available_id:', available_id
        
    end function
    
    subroutine find_min_max_idnums(idmin, idmax)
        integer, intent(out) :: idmin
        integer, intent(out) :: idmax
        integer :: i, idnum, highest, lowest
        
        lowest = 9999
        highest = 1
        
        do i = 1, size(notes)
            idnum = notes(i)%id_num
            
            if (idnum > highest) then
                highest = idnum
            end if
            
            if (idnum < lowest) then
                lowest = idnum
            end if
        end do
        idmin = lowest
        idmax = highest
    end subroutine
    
    function is_idnum_free(n) result(is_free)
        integer, intent(in) :: n
        logical :: is_free
        integer :: i
        
        is_free = .true.
        
        if ( .not. note_validate_idnum(n) ) then
            is_free = .false.
            return
        end if
        
        do i = 1, size(notes) 
            if ( n == (notes(i)%id_num) ) then
                is_free = .false.
            end if
        end do
        
    end function
    
    function file_exists(fname) result(fexists)
        character(len=*), intent(in) :: fname
        logical :: fexists
        fexists = .false.
        !if ( allocated(fname) ) then
            inquire(file=fname, exist=fexists)
        !end if
    end function
    
    function get_num_notes() result(n)
        integer :: n
        n = num_notes
    end function
    
    subroutine say_hello
        print *, "Welcome to fnote!"
    end subroutine say_hello
    
    function open_file(mode) result(unum)
        character, intent(in) :: mode
        integer :: unum
        integer :: fstat
        character(len=3) :: fmode
        logical :: fexists
        
        fstat = 0
        fexists = file_exists(FILENAME)
        
        select case (mode)
        case ('w')
            open(newunit=unum, file=FILENAME, status='replace', iostat=fstat)
            
        case ('r')
            if ( fexists ) then
                open(newunit=unum, file=FILENAME, status='old', iostat=fstat)
            else
                open(newunit=unum, file=FILENAME, status='new', iostat=fstat)
            end if
            
        case ('a')
            if (fexists) then
                open(newunit=unum, file=FILENAME, status='old', iostat=fstat, position='append')
            else
                open(newunit=unum, file=FILENAME, status='new', iostat=fstat)
            end if
            
        case default
!~             print *, 'open_file(): erroneous mode:', mode
            stop 1
        end select
        
!~         print *, 'open_file(): unum:', unum, ' fstat:', fstat
        
    end function
    
    subroutine close_file(unum) !result(successful)
        integer, intent(in) :: unum
        integer :: fstat
        logical :: successful
        
        close(unit=unum, iostat=fstat)
!~         print *, 'close_file(): unum:', unum, ' fstat:', fstat
        successful = (fstat == 0)
    end subroutine
    
    subroutine add_note(unum, note)
        integer, intent(in) :: unum
        type(note_t), intent(in) :: note
        integer :: fstat
        character(len=:), allocatable :: msg
        
        if ( allocated(note%msg) .and. (note%id_num > 0) ) then
            msg = note % serialize()
            write(unit=unum, fmt='(a)', iostat=fstat) msg
!~             print *, 'fstat:',fstat
            if (fstat == 0) then
                num_notes = num_notes + 1
            end if
        end if
    end subroutine
    
    subroutine read_notes(unum)
        integer, intent(in) :: unum
        integer :: i
        character(len=1024) :: buffer
        integer :: fstat
        integer :: fileunit
        type(note_t) :: note
        
        num_notes = 0
        do
            read(unit=unum, fmt='(a)', iostat=fstat) buffer
            
            if (fstat /= 0) then
                exit
            end if
            
            note = note_from_text(buffer)
            
!~             print *, 'buffer:', trim(buffer)
            if (note%validate()) then
!~                 print *, 'valid: ', trim(buffer)
                num_notes = num_notes + 1
            else
!~                 print *, 'invalid: ', trim(buffer)
            end if
            call note%dealloc()
        end do
        
!~         print *, 'num_notes:', num_notes
        
        if (num_notes > 0) then
            allocate( notes(1:num_notes) )
            rewind(unit=unum)
            
            !do i = 1, num_notes
            i = 1
            do
                if (i > num_notes) then
                    exit
                end if
                
                read(unit=unum, fmt='(a)', iostat=fstat) buffer
!~                 print *, 'buffer:', trim(buffer)
                note = note_from_text(buffer)
!~                 print *, 'note: ', note%serialize()
                if (note%validate()) then
                    notes(i) = note
                    i = i + 1
                end if
                call note%dealloc()
            end do
        end if
        
    end subroutine
    
    subroutine list_notes()
        character(len=*), parameter :: header='Notes:'
        character(len=*), parameter :: line=repeat('-', 5)
        character(len=*), parameter :: empty='(none). Hint: use "fnote &
            &--new <note>" to create a new note.)'
        integer :: i
        integer :: num_chars
        
        print '(a)', header
        print '(a)', line
        if (num_notes > 0) then
            num_chars = 0
            do i = 1, num_notes
                print '(i4,2x,a)', notes(i)%id_num, notes(i)%msg
                num_chars = num_chars + len(notes(i)%msg)
            end do
            print '(a)', line
            print '(a,x,i0,x,a,x,i0,x,a)', 'Total', num_chars, &
                &'characters in', num_notes, 'notes.'
        else
            print '(4x,a)', empty
        end if
    end subroutine
    
    subroutine clean_notes()
        integer :: i
!~         print *, 'clean_notes() called'
        
        if ( allocated(notes) ) then
            do i = 1, size(notes)
                call notes(i) % dealloc()
            end do
            deallocate(notes)
        end if
    end subroutine
    
    subroutine remove_note(id_to_remove) 
        integer, intent(in)  :: id_to_remove
        logical :: is_valid, note_exists, is_alloc, nonempty
        integer :: i
        integer :: unum_file
        type(note_t) :: note
        
        is_valid = note_validate_idnum(id_to_remove)
        note_exists = .not. is_idnum_free(id_to_remove)
        is_alloc = allocated(notes)
        nonempty = .false.
        if (is_alloc) then
            nonempty = (size(notes) > 0)
        end if
        
        if ( is_valid .and. note_exists &
             .and. is_alloc .and. nonempty ) &
        then
            ! Empty the file
            unum_file = open_file('w')
            call close_file(unum_file)
            
            unum_file = open_file('a')
            do i = 1, size(notes)
                if ( notes(i)%id_num /= id_to_remove ) then
                    call add_note(note=notes(i), unum=unum_file)
                end if
            end do
            call close_file(unum_file)
        end if
    end subroutine
    
end module fnote



















