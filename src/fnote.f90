! 
! FNote: Command-line notebook application
! Module: Support procedures for handling the notes and the note 
!         storage file.
! 

module fnote
    use note_mod
    implicit none
    
    ! Filename, where to store notes.
    character(len=*), parameter :: FILENAME = 'fnote.txt'
    
    ! Note storage & helper variable for number of notes.
    integer :: num_notes
    type(note_t), dimension(:), allocatable :: notes
    
contains
    
    !  
    !  name: find_free_idnum
    !  desc: Scans through all notes and returns an unoccupied ID 
    !        number.
    !  @param: (none)
    !  @return: first available ID number as a number in range 1..9999.
    !  
    function find_free_idnum() result (available_id)
        integer :: available_id
        integer :: idnum
        integer :: lowest, highest, searchspace
        logical :: found
        
        available_id = 1
        found = .false.
        lowest = 9999
        highest = 1
        
        ! Error in note storage.
        if (.not. allocated(notes)) then
            available_id = -1
            return
        end if
        
        ! Find lowest & highest ID, it is the ID search space.
        call find_min_max_idnums(lowest, highest)
        searchspace = highest - lowest
        
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
            
            ! Have to get a new ID.
            if (.not. found) then
                available_id = highest + 1
            end if
            
        ! If no IDs were available:
        else
            available_id = highest + 1
        end if
        
    end function
    
    
    !  
    !  name: find_min_max_idnums
    !  desc: Scans all notes and returns the smallest and biggest ID 
    !        numbers found as output parameters.
    !  @param idmin: output parameter, smallest ID found.
    !  @param idmax: output parameter, largest ID found.
    !  
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
    
    
    !  
    !  name: is_idnum_free
    !  desc: Helper function for determining if a given ID number is 
    !        available or not.
    !  @param n: ID number as an integer.
    !  @return: logical, .true. if the ID is available, .false.if the ID
    !           is occupied.
    !  
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
    
    
    !  
    !  name: file_exists
    !  desc: Helper function for determining if a given file exists.
    !  @param fname: file name.
    !  @return: logical, .true. if the file exists and .false. if not.
    !  
    function file_exists(fname) result(fexists)
        character(len=*), intent(in) :: fname
        logical :: fexists
        fexists = .false.
        inquire(file=fname, exist=fexists)
    end function
    
    
    !  
    !  name: get_num_notes
    !  desc: Returns the current number of notes.
    !  @return: integer, number of notes.
    !  
    function get_num_notes() result(n)
        integer :: n
        n = num_notes
    end function
    
    
    !  
    !  name: say_hello
    !  desc: Helper subroutine that prints a greeting for the user.
    !  
    subroutine say_hello()
        print *, "Welcome to fnote!"
    end subroutine say_hello
    
    
    !  
    !  name: open_file
    !  desc: Opens an existing file and returns the unit number.
    !  @param mode: character that determines the mode to use.
    !               'w': opens a file and deletes any existing file.
    !               'r': opens a file and places the file pointer in
    !                    the beginning of the file.
    !               'a': opens a file and places the file pointer at the
    !                    end of the file (append mode).
    !  @return: unit number of the file.
    !  
    function open_file(mode) result(unum)
        character, intent(in) :: mode
        integer :: unum
        integer :: fstat
        logical :: fexists
        
        fstat = 0
        fexists = file_exists(FILENAME)
        
        select case (mode)
        case ('w')
            open(newunit=unum, file=FILENAME, status='replace', iostat=fstat)
            
        case ('r')
            if (fexists) then
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
            print *, 'open_file(): erroneous mode: ', mode
            stop 1
        end select
    end function
    
    
    !  
    !  name: close_file
    !  desc: Helper subroutine for closing a file.
    !  @param unum: File unit number.
    !  
    subroutine close_file(unum)
        integer, intent(in) :: unum
        integer :: fstat
        logical :: successful
        
        close(unit=unum, iostat=fstat)
        successful = (fstat == 0)
    end subroutine
    
    
    !  
    !  name: add_note
    !  desc: Reads, serializes and stores a note record into the note
    !        storage file.
    !  @param unum: File unit number.
    !  @param note: Note record to store.
    !  
    subroutine add_note(unum, note)
        integer, intent(in) :: unum
        type(note_t), intent(in) :: note
        integer :: fstat
        character(len=:), allocatable :: msg
        
        if ( allocated(note%msg) .and. (note%id_num > 0) ) then
            ! Call a type-bound procedure.
            msg = note % serialize()
            
            write(unit=unum, fmt='(a)', iostat=fstat) msg
            
            if (fstat == 0) then
                num_notes = num_notes + 1
            end if
        end if
    end subroutine
    
    
    !  
    !  name: read_notes
    !  desc: Reads all notes from the note storage file into memory.
    !  @param unum: File unit number.
    !  
    subroutine read_notes(unum)
        integer, intent(in) :: unum
        integer :: i
        character(len=1024) :: buffer
        integer :: fstat
        type(note_t) :: note
        
        ! Step 1: counts the total number of notes in the file.
        num_notes = 0
        do
            read(unit=unum, fmt='(a)', iostat=fstat) buffer
            
            if (fstat /= 0) then
                exit
            end if
            
            note = note_from_text(buffer)
            
            if (note%validate()) then
                num_notes = num_notes + 1
            end if
            
            call note%dealloc()
        end do
        
        ! Step 2: Reads all notes from file into memory.
        if (num_notes > 0) then
            allocate( notes(1:num_notes) )
            rewind(unit=unum)
            
            i = 1
            do
                if (i > num_notes) then
                    exit
                end if
                
                read(unit=unum, fmt='(a)', iostat=fstat) buffer
                note = note_from_text(buffer)
                
                if (note%validate()) then
                    notes(i) = note
                    i = i + 1
                end if
                call note%dealloc()
            end do
        end if
        
    end subroutine
    
    
    !  
    !  name: list_notes
    !  desc: Subroutine that prints all notes on screen as a table.
    !        Prints an informative message if there are no notes yet.
    !  
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
    
    
    !  
    !  name: clean_notes
    !  desc: Housekeeping subroutine for cleaning up the note memory 
    !        storage.
    !  
    subroutine clean_notes()
        integer :: i
        
        if ( allocated(notes) ) then
            do i = 1, size(notes)
                call notes(i) % dealloc()
            end do
            deallocate(notes)
        end if
    end subroutine
    
    
    !  
    !  name: remove_note
    !  desc: Subroutine that removes a note from note storage file.
    !  @param id_to_remove: ID number of the note to be removed.
    !  
    subroutine remove_note(id_to_remove) 
        integer, intent(in)  :: id_to_remove
        logical :: is_valid, note_exists, is_alloc, nonempty
        integer :: i
        integer :: unum_file
        
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
            ! Step 1: Note storage file is emptied.
            unum_file = open_file('w')
            call close_file(unum_file)
            
            ! Step 2: Write notes back into the file.
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



















