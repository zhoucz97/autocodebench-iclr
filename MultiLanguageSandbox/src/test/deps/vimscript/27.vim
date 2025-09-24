
" Transforms the case of a given letter.
" Parameters:
" - letter (char): The input letter to be transformed.
" Returns:
" - char: The letter with its case reversed. If the input is lowercase, it returns the uppercase version,
" and if the input is uppercase, it returns the lowercase version.
" Example:
" >>> transform_letter_case('b')
"     'Z'

function TransformLetterCase(letter)
    if a:letter =~# '[a-z]'
        return toupper(a:letter)
    elseif a:letter =~# '[A-Z]'
        return tolower(a:letter)
    else
        return a:letter  " Return as-is if not a letter
    endif
endfunction

function! TestTransformLetterCase()
    if assert_equal(TransformLetterCase('b'), 'B') | cq | endif
    if assert_equal(TransformLetterCase('B'), 'b') | cq | endif
    if assert_equal(TransformLetterCase('z'), 'Z') | cq | endif
    if assert_equal(TransformLetterCase('Z'), 'z') | cq | endif
    if assert_equal(TransformLetterCase('m'), 'M') | cq | endif
    if assert_equal(TransformLetterCase('M'), 'm') | cq | endif
    if assert_equal(TransformLetterCase('1'), '1') | cq | endif
    if assert_equal(TransformLetterCase('!'), '!') | cq | endif
endfunction

call TestTransformLetterCase()
exit(0)