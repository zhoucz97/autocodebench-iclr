
" Find the factorial of N and take the modulo 10007 of the result.
" 
" Parameters:
" - N (int): An integer representing the input value (N <= 10000).
" 
" Returns:
" int: The result after taking the modulo 10007 of the factorial of N.
" 
" Example:
" >>> call ProcessRequest(1)
" 1

function ProcessRequest(n)
    if a:n < 0
        return 0 " Factorial is not defined for negative numbers
    elseif a:n == 0 || a:n == 1
        return 1
    endif

    let result = 1
    for i in range(2, a:n)
        let result = (result * i) % 10007
    endfor

    return result
endfunction

function! TestProcessRequest()
    if assert_equal(1, ProcessRequest(0)) | cq | endif
    if assert_equal(1, ProcessRequest(1)) | cq | endif
    if assert_equal(2, ProcessRequest(2)) | cq | endif
    if assert_equal(6, ProcessRequest(3)) | cq | endif
    if assert_equal(24, ProcessRequest(4)) | cq | endif
    if assert_equal(6266, ProcessRequest(10)) | cq | endif
    if assert_equal(6991, ProcessRequest(10000)) | cq | endif
endfunction

call TestProcessRequest()
exit(0)