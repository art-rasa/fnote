! 
! FNote: Command-line notebook application
! Module: Miscellaneous "helper" procedures for character string 
! operations.
! 

module str_utils
    implicit none
    
contains
    
    !  
    !  name: replace_chars
    !  desc: Replaces characters in a character string. The string is
    !        modified in-place.
    !  @param str: allocatable character string. Will be modified!
    !  @param to_replace: character that will be replaced.
    !  @param replace_with: replacement character.
    !  
    subroutine replace_chars(str, to_replace, replace_with)
        character(len=:), allocatable, intent(inout) :: str
        character, intent(in) :: to_replace
        character, intent(in) :: replace_with
        integer :: c_pos
        
        ! TODO: allocation check
        do
            c_pos = index(str, to_replace)
            
            if (c_pos == 0) then
                exit
            end if
            
            str(c_pos:c_pos) = replace_with
        end do
    end subroutine
    
end module
