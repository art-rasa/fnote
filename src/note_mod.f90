module note_mod
    implicit none
    
    type :: note_t
        integer :: id_num
        character(len=:), allocatable :: msg
    contains
        procedure :: serialize => note_serialize
        !procedure :: from_text => note_from_text
        procedure :: dealloc => note_dealloc
        procedure :: validate => note_validate
    end type
    
contains
    
    function note_validate(this) result (is_valid)
        class(note_t) :: this
        logical :: is_valid, valid_alloc, valid_len, valid_idnum
        
        valid_len = .false.
        valid_idnum = note_validate_idnum(this%id_num)
        valid_alloc = allocated(this%msg)
        if (valid_alloc) then
            valid_len = note_validate_msg(this%msg)
        end if
        
        is_valid = ( valid_len .and. valid_idnum .and. valid_alloc )
    end function
    
    function note_validate_idnum(n) result(valid)
        integer, intent(in) :: n
        logical :: valid
        valid = ( (n > 0) .and. (n <= 9999) )
    end function
    
    function note_validate_msg(m) result(valid)
        character(len=:), allocatable, intent(in) :: m
        logical :: valid
        valid = .false.
        if ( allocated(m) ) then
            valid = (len(m) > 0)
        end if
    end function
    
    subroutine note_dealloc(this)
        class(note_t) :: this
        if ( allocated(this % msg) ) then
            deallocate(this % msg)
            this % id_num = 0
        end if
    end subroutine
    
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
            !print *, 'tmp_a:', tmp_a
            !print *, 'tmp_b:', tmp_b
            read(tmp_a, fmt=*, iostat=cstat) idnum
            
            if ( (cstat == 0) .and. (len_trim(tmp_b) > 0) ) then
                res = note_t(idnum, tmp_b)
            end if
        end if
        
    end function
    
    
end module



























