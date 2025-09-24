
" Counts the number of different permutation schemes for a binary string of length n,
" where the number of '1's is m and the number of '0's is n - m.
" Parameters:
" - n (int): Length of the binary string.
" - m (int): Number of '1's in the binary string.
" Returns:
" int: The total number of different permutation schemes.

function CountPermutationsOfBinaryString(n, m) abort
    if a:m < 0 || a:m > a:n
        return 0
    endif

    let result = 1
    " Calculate C(n, m) = n / (m * (n - m)!)
    " To avoid large intermediate values, compute the product incrementally
    let k = min([a:m, a:n - a:m])  " Take the smaller of m and n-m for efficiency
    for i in range(1, k)
        let result = result * (a:n - k + i) / i
    endfor

    return result
endfunction

function! TestCountPermutationsOfBinaryString()
    if assert_equal(CountPermutationsOfBinaryString(2, 0), 2) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(2, 1), 0) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(3, 0), 0) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(3, 1), 3) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(3, 2), 0) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(30, 2), 145422675) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(4, 2), 4) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(5, 5), 1) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(33, 17), 13884156) | cq | endif
    if assert_equal(CountPermutationsOfBinaryString(1000, 1000), 1) | cq | endif
    " Add more test cases if necessary
endfunction

call TestCountPermutationsOfBinaryString()
exit(0)