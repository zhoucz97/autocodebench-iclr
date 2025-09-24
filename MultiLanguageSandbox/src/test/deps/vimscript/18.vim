
" Creates an ID by combining two words in a specific manner.
" Parameters:
" - word1 (char*): The first word to be used in the ID.
" - word2 (char*): The second word to be used in the ID.
"
" Returns:
" char*: An ID formed by reversing every other character of the second
"        word and combining it with the first word.
"
" Example:
" >>> create_id('fish', 'cat')
"     'ftiasch'

function CreateID(word1, word2) abort
    " Reverse every other character of word2
    let reversed_word2 = ''
    let i = 0
    while i < len(a:word2)
        if i % 2 == 0
            " Keep the character as is for even indices (0-based)
            let reversed_word2 .= a:word2[i]
        else
            " Reverse the character for odd indices
            let reversed_word2 .= nr2char(char2nr(a:word2[i]) - 1)
        endif
        let i += 1
    endwhile

    " Combine word1 and the modified word2
    return a:word1 . reversed_word2
endfunction

function! TestCreateID() abort
    if assert_equal(CreateID('fish', 'cat'), 'ftiasch') | cq | endif
    if assert_equal(CreateID('icpc', 'acm'), 'imccpac') | cq | endif
    if assert_equal(CreateID('oo', 'w'), 'owo') | cq | endif
    if assert_equal(CreateID('hello', 'world'), 'hdellrloow') | cq | endif
    if assert_equal(CreateID('abc', 'def'), 'afbecd') | cq | endif
    if assert_equal(CreateID('buaanb', 'nbbuaa'), 'bauaauabnbbn') | cq | endif
    if assert_equal(CreateID('xtuisgood', 'ilovextu'), 'xuttuxiesvgooloid') | cq | endif
endfunction

call TestCreateID()
exit(0)