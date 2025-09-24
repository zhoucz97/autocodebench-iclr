
" Display the ASCII information for a given character.
" Parameters:
" - character: The input character for which ASCII information is to be displayed.
" Returns:
" - ASCII code associated with the input character
" Example:
" >>> display_ascii_info('A')
"     65

function DisplayAsciiInfo(character)
    " Check if the input is a single character
    if len(a:character) != 1
        echoerr "Input must be a single character"
        return -1
    endif

    " Get the ASCII code of the character
    let ascii_code = char2nr(a:character)

    " Return the ASCII code
    return ascii_code
endfunction

function! TestDisplayAsciiInfo()
    " Replace `| cq |` with `| endif` as `cq` will quit Vim with an error code which is not the desired behavior in a test function.
    " Remove exit(0) as it's not needed or appropriate in the context of Vimscript.
    " The tests should not end Vim execution but should simply report success or failure.
    if assert_equal(DisplayAsciiInfo('A'), 65) | cq |endif
    if assert_equal(DisplayAsciiInfo('B'), 66) | cq |endif
    if assert_equal(DisplayAsciiInfo('0'), 48) | cq |endif
    if assert_equal(DisplayAsciiInfo(' '), 32) | cq |endif
    " echo "All tests passed."
endfunction

call TestDisplayAsciiInfo()
exit(0)