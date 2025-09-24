
" Calculates the Hamming distance between two integers in binary representation.
" Parameters:
" - a: int, The first positive integer (a <= 1,000,000,000).
" - b: int, The second positive integer (b <= 1,000,000,000).
" Returns:
" - int: The Hamming distance between a and b, i.e., the number of bits that
"        need to be flipped to transform a into b in binary.
" Example:
" >>> echo HammingDistance(1,2)
" 2

function HammingDistance(x, y)
    " Calculate the XOR of x and y to find differing bits
    let xor = a:x ^ a:y
    
    " Count the number of set bits in the XOR result
    let distance = 0
    while xor > 0
        " Increment distance if the least significant bit is set
        let distance += and(xor, 1)
        " Right shift to check the next bit
        let xor = xor / 2
    endwhile
    
    return distance
endfunction

function! TestHammingDistance()
    if assert_equal(HammingDistance(1, 2), 2) | cq | endif " 01 and 10 have 2 different bits
    if assert_equal(HammingDistance(4, 7), 2) | cq | endif " 100 and 111 have 2 different bits
    if assert_equal(HammingDistance(25, 30), 3) | cq | endif " Additional test: 11001 and 11110 have 3 different bits
    if assert_equal(HammingDistance(0, 0), 0) | cq | endif " Additional test: Same numbers have 0 different bits
    if assert_equal(HammingDistance(0xFFFFFFF, 0x0000000), 28) | cq | endif " Additional test: Max unsigned int and 0 have 32 different bits
endfunction

call TestHammingDistance()
exit(0)