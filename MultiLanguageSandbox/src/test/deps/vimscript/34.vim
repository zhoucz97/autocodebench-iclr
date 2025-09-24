
" You are given an array a of n integers. You can perform at most one operation where you select three
" integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j] with x, at a cost
" of (j - i + 1). Find the minimum cost to make all elements in the array equal.
" Example
" >>> makeEqualAgain(3, [1, 2, 1])
"     1

function MakeEqualAgain(n, a)
    let n = a:n
    let a = a:a

    " Check if all elements are the same
    let first = a[0]
    let all_same = 1
    for num in a
        if num != first
            let all_same = 0
            break
        endif
    endfor
    if all_same
        return 0
    endif

    " Find the longest prefix of the same value
    let prefix_len = 1
    while prefix_len < n && a[prefix_len] == a[0]
        let prefix_len += 1
    endwhile

    " Find the longest suffix of the same value
    let suffix_len = 1
    while suffix_len < n && a[n - suffix_len - 1] == a[-1]
        let suffix_len += 1
    endwhile

    " Check if the first and last elements are the same (so prefix and suffix can be merged)
    if a[0] == a[-1]
        let total_possible = prefix_len + suffix_len
        if total_possible > n
            let total_possible = n
        endif
        let min_cost = n - total_possible
    else
        let min_cost = min([n - prefix_len, n - suffix_len])
    endif

    return min_cost
endfunction

function! TestMakeEqualAgain() abort
    if assert_equal(MakeEqualAgain(3, [1, 2, 1]), 1) | cq | endif
    if assert_equal(MakeEqualAgain(5, [5, 5, 1, 5, 5]), 1) | cq | endif
    if assert_equal(MakeEqualAgain(4, [1, 1, 1, 1]), 0) | cq | endif
    if assert_equal(MakeEqualAgain(6, [2, 2, 2, 3, 2, 2]), 1) | cq | endif
    if assert_equal(MakeEqualAgain(1, [1]), 0) | cq | endif
    if assert_equal(MakeEqualAgain(2, [1, 2]), 1) | cq | endif
    if assert_equal(MakeEqualAgain(4, [1, 2, 2, 1]), 2) | cq | endif
    if assert_equal(MakeEqualAgain(7, [4, 4, 4, 3, 3, 4, 4]), 2) | cq | endif
    if assert_equal(MakeEqualAgain(6, [5, 4, 4, 4, 5, 5]), 3) | cq | endif
    if assert_equal(MakeEqualAgain(7, [1, 2, 1, 2, 1, 2, 1]), 5) | cq | endif
    if assert_equal(MakeEqualAgain(6, [1,2,3,4,5,1]), 4) | cq | endif
    if assert_equal(MakeEqualAgain(7, [1,1,1,1,1,1,1]), 0) | cq | endif
    if assert_equal(MakeEqualAgain(8, [8,8,8,1,2,8,8,8]), 2) | cq | endif
    if assert_equal(MakeEqualAgain(3, [1,2,3]), 2) | cq | endif
    if assert_equal(MakeEqualAgain(7, [4,3,2,7,1,1,3]), 6) | cq | endif
    if assert_equal(MakeEqualAgain(9, [9,9,2,9,2,5,5,5,3]), 7) | cq | endif
endfunction

call TestMakeEqualAgain()
exit(0)