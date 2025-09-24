
--[[
Shifts all characters by 5 positions in alphabetical order. Only letters are replaced, and all letters are in uppercase.

Parameters:
text (str): The input text to be processed.

Returns:
str: The transformed text with characters shifted by 5 positions.

Examples:
shift_characters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
-- Expected: "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"
]]--

function shift_characters(text)
    local result = ""
    for i = 1, #text do
        local c = text:sub(i, i)
        if c >= 'A' and c <= 'Z' then
            -- Calculate the new character after shifting 5 positions
            local new_char_code = string.byte(c) - string.byte('A')
            new_char_code = (new_char_code - 5) % 26
            if new_char_code < 0 then
                new_char_code = new_char_code + 26
            end
            local new_char = string.char(new_char_code + string.byte('A'))
            result = result .. new_char
        else
            result = result .. c
        end
    end
    return result
end
function testShiftCharacters()
  local test1 = "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX"
  local test2 = "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ"
  local test3 = "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ"
  assert(shift_characters(test1) == "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES")
  assert(shift_characters(test2) == "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME")
  assert(shift_characters(test3) == "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE")
end

testShiftCharacters()