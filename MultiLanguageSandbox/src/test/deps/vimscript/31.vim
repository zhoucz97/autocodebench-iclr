
" Given a string consisting of lowercase English letters, in each round you can change one of the characters to another character. The question is: what is the minimum number of rounds needed to make the string composed of the same character?
" For example:
" >>> minRoundsToSameChar("aab")
"     1

function MinRoundsToSameChar(s)
    if len(a:s) == 0
        return 0
    endif

    " Count the frequency of each character
    let counts = {}
    for char in split(a:s, '\zs')
        if has_key(counts, char)
            let counts[char] += 1
        else
            let counts[char] = 1
        endif
    endfor

    " Find the maximum frequency
    let max_count = 0
    for [char, count] in items(counts)
        if count > max_count
            let max_count = count
        endif
    endfor

    " The minimum rounds is total length minus the maximum frequency
    return len(a:s) - max_count
endfunction

function! TestMinRoundsToSameChar()
    if assert_equal(MinRoundsToSameChar("aab"), 1) | cq | endif
    if assert_equal(MinRoundsToSameChar("abc"), 2) | cq | endif
    if assert_equal(MinRoundsToSameChar("aaa"), 0) | cq | endif
    if assert_equal(MinRoundsToSameChar("abab"), 1) | cq | endif
    if assert_equal(MinRoundsToSameChar("zzzzz"), 0) | cq | endif
endfunction

call TestMinRoundsToSameChar()
exit(0)