
" Determine if a given string is a palindrome.
" A palindrome is a word, phrase, number, or other sequence of characters that
" reads the same forward and backward (ignoring spaces, punctuation, and
" capitalization).
" Examples
" >>> isPalindrome("racecar")
"   true

function IsPalindrome(str)
    " Convert to lowercase
    let l:str = tolower(a:str)
    
    " Remove all non-alphanumeric characters
    let l:str = substitute(l:str, '[^a-z0-9]', '', 'g')
    
    " Compare the string with its reverse
    return l:str ==# join(reverse(split(l:str, '\zs')), '')
endfunction

function! TestIsPalindrome()
    if assert_equal(IsPalindrome("A man a plan a canal Panama"), v:true) | cq | endif
    if assert_equal(IsPalindrome("No lemon, no melon"), v:true) | cq | endif
    if assert_equal(IsPalindrome("Was it a car or a cat I saw"), v:true) | cq | endif
    if assert_equal(IsPalindrome("Madam, in Eden, I'm Adam"), v:true) | cq | endif
    if assert_equal(IsPalindrome("Never odd or even"), v:true) | cq | endif
    if assert_equal(IsPalindrome("Eva, can I see bees in a cave"), v:true) | cq | endif
    if assert_equal(IsPalindrome("hello"), v:false) | cq | endif
    if assert_equal(IsPalindrome("GitHub"), v:false) | cq | endif
    if assert_equal(IsPalindrome("programming"), v:false) | cq | endif
endfunction

call TestIsPalindrome()

exit(0)