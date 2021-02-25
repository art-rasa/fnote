! 
! FNote: Command-line notebook application
! Module: Defines a derived data type for a note and several procedures
!         for handling its functionalities.
! 

module note_mod
    implicit none
    
    type :: note_t
        integer :: id_num
        character(len=:), allocatable :: msg
    contains
        ! Type-bound procedures
        procedure :: serialize => note_serialize
        procedure :: dealloc => note_dealloc
        procedure :: validate => note_validate
    end type
    
contains
    
    !  
    !  name: note_validate
    !  desc: Type-bound function for note validation.
    !  @return: logical, returns .true. if note record is valid, .false.
    !           otherwise.
    !  
    function note_validate(this) result (is_valid)
        class(note_t) :: this
        logical :: is_valid, valid_alloc, valid_len, valid_idnum
        
        valid_len = .false.
        valid_idnum = note_validate_idnum(this%id_num)
        valid_alloc = allocated(this%msg)
        if (valid_alloc) then
            valid_len = note_validate_msg(this%msg)
        end if
        
        is_valid = (valid_len .and. valid_idnum .and. valid_alloc)
    end function
    
    
    !  
    !  name: note_dealloc
    !  desc: Type-bound subroutine for deallocating a note record from 
    !        memory.
    !  
    subroutine note_dealloc(this)
        class(note_t) :: this
        if ( allocated(this % msg) ) then
            deallocate(this % msg)
            this % id_num = 0
        end if
    end subroutine
    
    
    !  
    !  name: note_serialize
    !  desc: Type-bound function, reads a note record and serializes it 
    !        into a character string for file storage.
    !  @return: the note record serialized into a character string.
    !  
    function note_serialize(this) result(rs)
        class(note_t) :: this
        character(len=10) :: tmp
        character(len=:), allocatable :: rs
        integer :: str_len
        
        write(tmp, '(i0)') this % id_num
        
        if ( allocated(this % msg) ) then
            str_len = len_trim(tmp) + len_trim(this % msg) + 1
            allocate( character(len=str_len) :: rs )
            rs = trim(tmp) // '|' // this%msg
        else
            rs = '-1|(invalid string)'
        end if
    end function
    
    
    !  
    !  name: note_validate_idnum
    !  desc: Checks if a note ID number is in an allowed range.
    !  @param n: ID number to check.
    !  @return: logical, .true. if ID number is valid, .false. if not.
    !  
    function note_validate_idnum(n) result(valid)
        integer, intent(in) :: n
        logical :: valid
        valid = ( (n > 0) .and. (n <= 9999) )
    end function
    
    
    !  
    !  name: note_validate_msg
    !  desc: Checks if a note message is valid.
    !  @param m: note text message string.
    !  @return: logical, .true. if note text is valid, .false. if not.
    !  
    function note_validate_msg(m) result(valid)
        character(len=:), allocatable, intent(in) :: m
        logical :: valid
        valid = .false.
        if ( allocated(m) ) then
            valid = (len(m) > 0)
        end if
    end function
    
    
    !  
    !  name: note_from_text
    !  desc: Takes a serialized note record, checks it and de-serializes
    !        it into a note record.
    !  @param src: serialized note record as a character string.
    !  @return: de-serialized note record.
    !  
    function note_from_text(src) result(res)
        character(len=*) :: src
        type(note_t) :: res
        integer :: src_len 
        integer :: sep_pos
        character(len=:), allocatable :: tmp_a
        character(len=:), allocatable :: tmp_b
        integer :: cstat
        integer :: idnum
        
        src_len = len_trim(src)
        sep_pos = index(src, '|')
        
        if ( (sep_pos > 0) .and. (sep_pos < src_len) ) then
            allocate( character(len=sep_pos) :: tmp_a )
            allocate( character(len=src_len - sep_pos) :: tmp_b )
            tmp_a = src(1:(sep_pos-1))
            tmp_b = src((sep_pos+1):src_len)
            
            read(tmp_a, fmt=*, iostat=cstat) idnum
            
            if ( (cstat == 0) .and. (len_trim(tmp_b) > 0) ) then
                res = note_t(idnum, tmp_b)
            end if
        end if
        
    end function
    
    
end module



























