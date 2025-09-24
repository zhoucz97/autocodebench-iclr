
" Given three integers n, m, and k, find the number of sequences consisting of n '(' and m ')',
" such that the longest balanced subsequence is of length 2 * k. The result should be
" calculated modulo 1,000,000,007 (10^9 + 7).
"
" For example:
"     >>> countBalancedSubsequences(2, 2, 2)
"     2

function CountBalancedSubsequences(n, m, k)
    let MOD = 1000000007

    " Precompute factorial, inverse factorial up to max(n, m)
    let max_nm = max([a:n, a:m])
    let fact = repeat([1], max_nm + 1)
    let inv_fact = repeat([1], max_nm + 1)

    for i in range(1, max_nm)
        let fact[i] = (fact[i-1] * i) % MOD
    endfor

    let inv_fact[max_nm] = pow(fact[max_nm], MOD-2, MOD)
    for i in range(max_nm - 1, 0, -1)
        let inv_fact[i] = (inv_fact[i + 1] * (i + 1)) % MOD
    endfor

    function Comb(a, b)
        if a:a < 0 || a:b < 0 || a:a < a:b
            return 0
        endif
        return (fact[a:a] * inv_fact[a:b] % MOD) * inv_fact[a:a - a:b] % MOD
    endfunction

    let total = 0

    " Case 1: number of '(' is k, number of ')' is m (m >= k)
    if a:m >= a:k
        let ways = Comb(a:m, a:k)
        let total = (total + ways) % MOD
    endif

    " Case 2: number of ')' is k, number of '(' is n (n >= k)
    if a:n >= a:k
        let ways = Comb(a:n, a:k)
        let total = (total + ways) % MOD
    endif

    " Subtract the overlap where both counts are k (to avoid double-counting)
    if a:n >= a:k && a:m >= a:k
        let overlap = Comb(a:n, a:k) * Comb(a:m, a:k) % MOD
        let total = (total - overlap + MOD) % MOD
    endif

    return total
endfunction

call assert_equal(CountBalancedSubsequences(2, 2, 2), 2)
call assert_equal(CountBalancedSubsequences(3, 2, 3), 0)
call assert_equal(CountBalancedSubsequences(3, 2, 1), 4)
call assert_equal(CountBalancedSubsequences(4, 3, 2), 14)
call assert_equal(CountBalancedSubsequences(5, 5, 2), 35)
call assert_equal(CountBalancedSubsequences(6, 1, 1), 6)
call assert_equal(CountBalancedSubsequences(1, 6, 1), 6)
call assert_equal(CountBalancedSubsequences(7, 2, 2), 27)
call assert_equal(CountBalancedSubsequences(8, 3, 3), 110)
call assert_equal(CountBalancedSubsequences(10, 10, 5), 10659)
call assert_equal(CountBalancedSubsequences(20, 20, 10), 574221648)
call assert_equal(CountBalancedSubsequences(233, 233, 233), 280916397)
call assert_equal(CountBalancedSubsequences(233, 666, 233), 956287259)
call assert_equal(CountBalancedSubsequences(233, 233, 666), 0)

exit(0)