
" Given an array of n distinct integers representing the heights of Kira's friends,
" find the number of ways to choose a triplet (a, b, c) such that the greatest common
" divisor (GCD) of the maximum and minimum values of the triplet is 1.
" Example
" >>> countTriplets([1, 5, 7])
"     1

" Function to compute the GCD of two numbers
function GCD(a, b)
    let l:a = a:a
    let l:b = a:b
    while l:b != 0
        let l:temp = l:b
        let l:b = l:a % l:b
        let l:a = l:temp
    endwhile
    return l:a
endfunction

" Main function to count triplets with GCD condition
function CountTriplets(heights, n)
    let l:count = 0
    for i in range(a:n)
        for j in range(i + 1, a:n - 1)
            for k in range(j + 1, a:n - 1)
                " Get the three elements
                let l:a = a:heights[i]
                let l:b = a:heights[j]
                let l:c = a:heights[k]
                
                " Find the max and min of the triplet
                let l:max = max([l:a, l:b, l:c])
                let l:min = min([l:a, l:b, l:c])
                
                " Check if GCD of max and min is 1
                if GCD(l:max, l:min) == 1
                    let l:count += 1
                endif
            endfor
        endfor
    endfor
    return l:count
endfunction

" Example usage:
echo CountTriplets([1, 5, 7], 3)  " Output should be 1

function! TestCountTriplets()
    if assert_equal(CountTriplets([1, 5, 7], 3), 1) | cq | endif
    if assert_equal(CountTriplets([1, 6, 2, 3], 4), 3) | cq | endif
    if assert_equal(CountTriplets([16, 4, 8, 2], 4), 0) | cq | endif
    if assert_equal(CountTriplets([10, 1, 6, 7, 9, 8, 4, 3, 5, 2], 10), 77) | cq | endif
    if assert_equal(CountTriplets([4, 5, 9, 11, 14], 5), 7) | cq | endif
    if assert_equal(CountTriplets([15, 10, 6, 9, 7, 3, 12, 8, 11, 1, 2], 11), 104) | cq | endif
    if assert_equal(CountTriplets([3, 7, 11, 13], 4), 4) | cq | endif
    if assert_equal(CountTriplets([5, 12, 13, 17, 19], 5), 10) | cq | endif
    if assert_equal(CountTriplets([2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], 11), 87) | cq | endif
    if assert_equal(CountTriplets([1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17], 11), 122) | cq | endif
endfunction

call TestCountTriplets()
exit(0)