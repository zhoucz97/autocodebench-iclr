
" Find the length of the longest consecutive sequence of 1s in the binary
" representation of a non-negative integer.
" 
" Parameters:
" - n (number): A non-negative integer (0 ≤ n ≤ 2^64 - 1).
"
" Returns:
" int: The length of the longest consecutive sequence of 1s in the binary
" representation of the given integer.
"
" Examples:
" >>> FindLongestConsecutiveOnesLength(7)
"     3

function FindLongestConsecutiveOnesLength(n)
    let max_length = 0
    let current_length = 0

    while a:n > 0
        if and(a:n, 1) == 1
            let current_length += 1
            if current_length > max_length
                let max_length = current_length
            endif
        else
            let current_length = 0
        endif
        let a:n = a:n / 2
    endwhile

    return max_length
endfunction

function! TestFindLongestConsecutiveOnesLength()
    if assert_equal(FindLongestConsecutiveOnesLength(7), 3) | cq | endif
    if assert_equal(FindLongestConsecutiveOnesLength(13), 2) | cq | endif
    if assert_equal(FindLongestConsecutiveOnesLength(12345), 3) | cq | endif
    if assert_equal(FindLongestConsecutiveOnesLength(0xF0F0F0F000), 4) | cq | endif
    if assert_equal(FindLongestConsecutiveOnesLength(0xFFFFFFFF), 32) | cq | endif
    if assert_equal(FindLongestConsecutiveOnesLength(0), 0) | cq | endif
endfunction

call TestFindLongestConsecutiveOnesLength()
exit(0)