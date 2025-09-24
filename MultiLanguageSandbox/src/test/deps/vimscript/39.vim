
"
" Given an array A of integers, the task is to calculate the sum of the XOR
" of all subarrays. A subarray is defined by a pair of indices (L, R) such
" that 1 <= L <= R <= n, where n is the size of the array. The XOR sum of
" a subarray is the result of XORing all elements from L to R. The final
" result is the sum of the XOR sums for all possible subarrays.
"
" Example cases:
"     >>> SumOfXorSubarrays([1, 2, 3, 4, 5], 5)
"     39

function SumOfXorSubarrays(A, n)
    let total = 0
    for L in range(a:n)
        let current_xor = 0
        for R in range(L, a:n - 1)
            let current_xor = current_xor ^ a:A[R]
            let total += current_xor
        endfor
    endfor
    return total
endfunction

function! TestSumOfXorSubarrays()
    if assert_equal(SumOfXorSubarrays([1, 2, 3, 4, 5], 5), 39) | cq | endif
    if assert_equal(SumOfXorSubarrays([1, 1, 1], 3), 4) | cq | endif
    if assert_equal(SumOfXorSubarrays([2, 3, 1], 3), 9) | cq | endif
    if assert_equal(SumOfXorSubarrays([4, 5, 7, 9], 4), 74) | cq | endif
    if assert_equal(SumOfXorSubarrays([0, 0, 0, 0], 4), 0) | cq | endif
    if assert_equal(SumOfXorSubarrays([8, 8, 8, 8, 8], 5), 72) | cq | endif
    if assert_equal(SumOfXorSubarrays([3, 6, 9, 12, 15], 5), 125) | cq | endif
    if assert_equal(SumOfXorSubarrays([10, 20, 30, 40, 50], 5), 390) | cq | endif
    if assert_equal(SumOfXorSubarrays([16, 16, 16, 16, 16, 16], 6), 192) | cq | endif
    if assert_equal(SumOfXorSubarrays([1, 3, 5, 7, 9, 11, 13], 7), 192) | cq | endif
    if assert_equal(SumOfXorSubarrays([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], 10), 218) | cq | endif
endfunction

call TestSumOfXorSubarrays()
exit 0