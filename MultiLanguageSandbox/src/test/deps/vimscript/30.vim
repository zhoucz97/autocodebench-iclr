
" Checks if a three-character string has exactly two characters that are the same.
"
" Args:
"     s (String): The three-character string to be checked.
"
" Returns:
"     String: Returns 'Yes' if the input has exactly two equal characters, otherwise 'No'.
"     Examples:
"     >>> CheckTwoEqualDigits('112')
"     Yes

function CheckTwoEqualDigits(s)
    " Check if the string length is exactly 3
    if len(a:s) != 3
        return 'No'
    endif

    " Compare each pair of characters
    if a:s[0] == a:s[1] && a:s[0] != a:s[2]
        return 'Yes'
    elseif a:s[0] == a:s[2] && a:s[0] != a:s[1]
        return 'Yes'
    elseif a:s[1] == a:s[2] && a:s[1] != a:s[0]
        return 'Yes'
    else
        return 'No'
    endif
endfunction

function! TestCheckTwoEqualDigits()
    if assert_equal(CheckTwoEqualDigits('112'), 'Yes') | cq | endif
    if assert_equal(CheckTwoEqualDigits('123'), 'No') | cq | endif
    if assert_equal(CheckTwoEqualDigits('232'), 'Yes') | cq | endif
    if assert_equal(CheckTwoEqualDigits('444'), 'No') | cq | endif       " All three characters are equal, not two
    if assert_equal(CheckTwoEqualDigits('121'), 'Yes') | cq | endif

    " Additional test cases to cover more scenarios
    if assert_equal(CheckTwoEqualDigits('787'), 'Yes') | cq | endif
    if assert_equal(CheckTwoEqualDigits('999'), 'No') | cq | endif       " All three characters are equal
    if assert_equal(CheckTwoEqualDigits('890'), 'No') | cq | endif
    if assert_equal(CheckTwoEqualDigits('556'), 'Yes') | cq | endif
    if assert_equal(CheckTwoEqualDigits('353'), 'Yes') | cq | endif      " No two characters are equal
endfunction

call TestCheckTwoEqualDigits()
exit(0)