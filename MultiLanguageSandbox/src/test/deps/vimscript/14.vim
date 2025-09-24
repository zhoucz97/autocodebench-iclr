
" Decode a series of numbers to reveal the pattern and understand the actual values 
" each digit represents.
" 0000=4 8888=8 1234=1 5678=3 9012=2 1357=0 2468=4
"
" Parameters:
" - data_str: A string representing a series of numbers. Length does not exceed 100.
"
" Returns:
" int: The result of each data string corresponding to the right-hand side of the equation.
"
" Example usage:
" assert_equal(decode_numbers("0000"), 4)

function DecodeNumbers(data_str)
    let loop_counts = {
        \ '0': 1,
        \ '1': 0,
        \ '2': 0,
        \ '3': 0,
        \ '4': 1,
        \ '5': 0,
        \ '6': 1,
        \ '7': 0,
        \ '8': 2,
        \ '9': 1
        \ }
    
    let total = 0
    for char in split(a:data_str, '\zs')
        let total += loop_counts[char]
    endfor
    
    return total
endfunction

function! TestDecodeNumbers()
    if assert_equal(DecodeNumbers("0000"), 4) | cq | endif
    if assert_equal(DecodeNumbers("8888"), 8) | cq | endif
    if assert_equal(DecodeNumbers("1234"), 1) | cq | endif
    if assert_equal(DecodeNumbers("5678"), 3) | cq | endif
    if assert_equal(DecodeNumbers("9012"), 2) | cq | endif
    if assert_equal(DecodeNumbers("1357"), 0) | cq | endif
    if assert_equal(DecodeNumbers("2468"), 4) | cq | endif
    " Additional test samples
    if assert_equal(DecodeNumbers("9999"), 4) | cq | endif
    if assert_equal(DecodeNumbers("1111"), 0) | cq | endif
    if assert_equal(DecodeNumbers("2222"), 0) | cq | endif
    if assert_equal(DecodeNumbers("3333"), 0) | cq | endif
    if assert_equal(DecodeNumbers("4444"), 4) | cq | endif
    if assert_equal(DecodeNumbers("5555"), 0) | cq | endif
    if assert_equal(DecodeNumbers("6666"), 4) | cq | endif
    if assert_equal(DecodeNumbers("7777"), 0) | cq | endif
    if assert_equal(DecodeNumbers("0001"), 3) | cq | endif
    if assert_equal(DecodeNumbers("2301"), 1) | cq | endif
endfunction

call TestDecodeNumbers()
exit(0)