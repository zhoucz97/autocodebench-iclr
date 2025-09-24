
" Count the number of valid coin toss sequences with no consecutive heads in n tosses.
" Parameters:
" - n (int): The number of coin tosses.
" Returns:
" The count of valid sequences.
" Example:
" >>> count_valid_coin_toss_sequences(1)
"     2

function CountValidCoinTossSequences(n)
    if a:n == 0
        return 0
    elseif a:n == 1
        return 2
    elseif a:n == 2
        return 3
    endif

    let prev_prev = 2  " dp[1]
    let prev = 3       " dp[2]
    let current = 0

    for i in range(3, a:n)
        let current = prev + prev_prev
        let prev_prev = prev
        let prev = current
    endfor

    return current
endfunction

function! TestCountValidCoinTossSequences()
    if assert_equal(CountValidCoinTossSequences(1), 2) | cq | endif
    if assert_equal(CountValidCoinTossSequences(2), 3) | cq | endif
    if assert_equal(CountValidCoinTossSequences(3), 5) | cq | endif
    if assert_equal(CountValidCoinTossSequences(4), 8) | cq | endif
    if assert_equal(CountValidCoinTossSequences(5), 13) | cq | endif
    " Additional tests
    if assert_equal(CountValidCoinTossSequences(40), 267914296) | cq | endif
    if assert_equal(CountValidCoinTossSequences(39), 165580141) | cq | endif
    if assert_equal(CountValidCoinTossSequences(38), 102334155) | cq | endif
endfunction

call TestCountValidCoinTossSequences()
exit(0)