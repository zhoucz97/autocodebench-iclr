

# Decrypts a message encrypted with Caesar's cipher.
# The cipher shifts each letter in the message 5 positions to the right in the alphabet.
# Non-letter characters are left unchanged. All letters are in uppercase.
#
# Example:
# >>> decrypt_caesar_cipher('NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX')
# 'IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES'
# >>> decrypt_caesar_cipher('N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ')
# 'I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME'

def decrypt_caesar_cipher(message)
  message.chars.map do |char|
    if char.match?(/[A-Z]/)
      # Calculate the new character by shifting back 5 positions
      # Using modulo 26 to handle wrap-around from 'A' to 'Z'
      ((char.ord - 'A'.ord - 5) % 26 + 'A'.ord).chr
    else
      char
    end
  end.join
end

raise 'Test failed' unless decrypt_caesar_cipher('NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX') == 'IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES'
raise 'Test failed' unless decrypt_caesar_cipher('N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ') == 'I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME'
raise 'Test failed' unless decrypt_caesar_cipher('IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ') == 'DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE'
raise 'Test failed' unless decrypt_caesar_cipher('ABCDEF') == 'VWXYZA'
raise 'Test failed' unless decrypt_caesar_cipher('XYZ') == 'STU'

  


puts 'All tests passed!'