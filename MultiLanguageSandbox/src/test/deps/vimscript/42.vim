
" Given a rectangle paper of size n x m placed on a plane Cartesian coordinate system with its
" bottom-left corner at (0,0) and top-right corner at (n,m), you need to calculate the expected
" number of operations to cut the paper such that the remaining area is less than k.
" In each operation, a line is randomly chosen that is parallel to the axes, passes through points
" with integer coordinates, and cuts through (not just touching the edge) the paper. The bottom or
" right part of the paper along this line is then discarded.
" The answer should be modulo 10^9+7.
"
" For example:
"     >>> expectedCuts(3, 3, 4)
"     3

function ExpectedCuts(n, m, k) abort
    let MOD = 1000000007
    let dp = repeat([repeat([0], a:m + 1)], a:n + 1)

    for i in range(1, a:n)
        for j in range(1, a:m)
            if i * j < a:k
                let dp[i][j] = 0
            else
                let total = 0
                let count = 0

                " Vertical cuts
                for x in range(1, i - 1)
                    let total += dp[x][j] + 1
                    let count += 1
                endfor

                " Horizontal cuts
                for y in range(1, j - 1)
                    let total += dp[i][y] + 1
                    let count += 1
                endfor

                if count > 0
                    let dp[i][j] = (total / count) % MOD
                else
                    let dp[i][j] = 0
                endif
            endif
        endfor
    endfor

    return dp[a:n][a:m]
endfunction

function! TestExpectedCuts() abort
    if assert_equal(ExpectedCuts(2, 4, 10), 0) | cq | endif
    if assert_equal(ExpectedCuts(2, 4, 8), 1) | cq | endif
    if assert_equal(ExpectedCuts(2, 4, 2), 833333342) | cq | endif
    if assert_equal(ExpectedCuts(2, 4, 6), 250000003) | cq | endif
    if assert_equal(ExpectedCuts(3, 3, 4), 666666673) | cq | endif
    if assert_equal(ExpectedCuts(5, 5, 12), 666666673) | cq | endif
    if assert_equal(ExpectedCuts(6, 7, 20), 722222229) | cq | endif
    if assert_equal(ExpectedCuts(8, 8, 30), 72727275) | cq | endif
    if assert_equal(ExpectedCuts(10, 10, 50), 714285721) | cq | endif
    if assert_equal(ExpectedCuts(1, 10, 5), 945634929) | cq | endif
    if assert_equal(ExpectedCuts(10, 1, 5), 945634929) | cq | endif
endfunction

call TestExpectedCuts()
exit(0)