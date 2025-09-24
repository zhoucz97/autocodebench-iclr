
" Given two positive integers n and k, find the number of positive integers x, where
" 1 <= x <= n, that can be expressed as x = a^b with a and b being positive integers
" and b >= k. Different legal representations of the same number are counted only once.
" Example:
" >>> countPowerNumbers(99, 1)
"     99

function CountPowerNumbers(n, k)
    let power_numbers = {}
    let max_a = float2nr(pow(a:n, 1.0 / a:k)) + 1

    for a in range(1, max_a)
        let b = a:k
        let x = pow(a, b)
        while x <= a:n
            let power_numbers[x] = 1
            let b += 1
            let x = pow(a, b)
        endwhile
    endfor

    return len(keys(power_numbers))
endfunction

function! TestCountPowerNumbers()
    if assert_equal(CountPowerNumbers(99, 1), 99) | cq | endif
    if assert_equal(CountPowerNumbers(99, 3), 7) | cq | endif
    if assert_equal(CountPowerNumbers(99, 2), 12) | cq | endif
    if assert_equal(CountPowerNumbers(10, 1), 10) | cq | endif
    if assert_equal(CountPowerNumbers(10, 2), 4) | cq | endif
    if assert_equal(CountPowerNumbers(500, 1), 500) | cq | endif
    if assert_equal(CountPowerNumbers(500, 2), 30) | cq | endif
    if assert_equal(CountPowerNumbers(500, 3), 13) | cq | endif
    if assert_equal(CountPowerNumbers(1000, 1), 1000) | cq | endif
    if assert_equal(CountPowerNumbers(1000, 2), 41) | cq | endif
    if assert_equal(CountPowerNumbers(1000, 3), 17) | cq | endif
    if assert_equal(CountPowerNumbers(1000, 93), 1) | cq | endif
    if assert_equal(CountPowerNumbers(50, 2), 10) | cq | endif
    if assert_equal(CountPowerNumbers(50, 3), 5) | cq | endif
    if assert_equal(CountPowerNumbers(2, 3), 1) | cq | endif
endfunction

call TestCountPowerNumbers()
exit(0)