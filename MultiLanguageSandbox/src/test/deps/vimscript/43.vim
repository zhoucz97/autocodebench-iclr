
" Given a permutation q of n elements and an integer k, find the number of permutations p of n elements
" such that f(p) = q, where f(p) is the lexicographically smallest permutation that can be obtained by
" dividing p into exactly k non-empty contiguous segments and sorting each segment (that is, choose k-1
" breakpoints 1 <= x_1 < x_2 < ... < x_{k-1} < n, and divide it into [1, x_1], (x_1, x_2], ..., (x_{k-1}, n]).
" The result should be modulo 998244353.
" Example
" >>> countPermutations(2, 1, [1, 2])
"     2

function CountPermutations(n, k, qq)
    let mod = 998244353
    " Precompute factorials up to n modulo mod
    let fact = [1]
    for i in range(1, a:n)
        call add(fact, (fact[i-1] * i) % mod)
    endfor

    " Check if q can be split into k increasing segments
    let segments = []
    let current_segment = [a:qq[0]]
    for i in range(1, a:n - 1)
        if a:qq[i] > a:qq[i-1]
            call add(current_segment, a:qq[i])
        else
            call add(segments, current_segment)
            let current_segment = [a:qq[i]]
        endif
    endfor
    call add(segments, current_segment)

    if len(segments) != a:k
        return 0
    endif

    " Calculate the product of factorials of segment lengths
    let result = 1
    for seg in segments
        let len_seg = len(seg)
        let result = (result * fact[len_seg]) % mod
    endfor

    return result
endfunction

function! TestCountPermutations()
    if assert_equal(CountPermutations(2, 1, [1, 2]), 2) | cq | endif
    if assert_equal(CountPermutations(3, 3, [3, 1, 2]), 1) | cq | endif
    if assert_equal(CountPermutations(6, 3, [1, 2, 3, 6, 5, 4]), 13) | cq | endif
    if assert_equal(CountPermutations(6, 1, [1, 2, 3, 4, 5, 6]), 720) | cq | endif
    if assert_equal(CountPermutations(6, 3, [1, 2, 5, 3, 4, 5]), 0) | cq | endif
    if assert_equal(CountPermutations(9, 9, [1, 2, 3, 4, 5, 6, 7, 8, 9]), 1) | cq | endif
    if assert_equal(CountPermutations(9, 2, [1, 2, 3, 4, 5, 6, 7, 9, 8]), 29093) | cq | endif
endfunction

call TestCountPermutations()
exit(0)