
" Shifts all characters by 5 positions in alphabetical order. Only letters are replaced, and all letters are in uppercase.
" Parameters:
" text (str): The input text to be processed.
"
" Returns:
" str: The transformed text with characters shifted by 5 positions.
" Example
" >>> shift_characters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
"     "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"

function ShiftCharacters(text)
    let result = ''
    for char in split(a:text, '\zs')
        if char =~# '[A-Z]'
            " Calculate new character position (wrapping around if needed)
            let new_char = nr2char((char2nr(char) - char2nr('A') + 5) % 26 + char2nr('A'))
            let result .= new_char
        else
            let result .= char
        endif
    endfor
    return result
endfunction

function! TestShiftCharacters()
    let test1 = "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX"
    let test2 = "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ"
    let test3 = "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ"

    if assert_equal(ShiftCharacters(test1), "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES") | cq | endif
    if assert_equal(ShiftCharacters(test2), "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME") | cq | endif
    if assert_equal(ShiftCharacters(test3), "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE") | cq | endif
    " echo "All tests passed!"
endfunction

call TestShiftCharacters()

exit(0)