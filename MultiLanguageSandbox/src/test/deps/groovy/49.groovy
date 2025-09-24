
class StringCompressor {
    static int compress(ArrayList chars) {
        """
        Compress a given character array by replacing consecutive repeating characters with the character followed by the count of the consecutive characters.
        If the count of consecutive characters is 1, the character is left as is.
        If the count is 10 or more, the count is split into individual digits.
        The compressed array is stored in the input array itself.
        The function returns the new length of the array.
        >>> compress(['a', 'a', 'b', 'b', 'c', 'c', 'c'])
        6
        >>> compress(['a'])
        1
        >>> compress(['a', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b'])
        4
        """

    if (chars.isEmpty()) return 0
    
    int writeIndex = 0
    int readIndex = 0
    int n = chars.size()
    
    while (readIndex < n) {
        char currentChar = chars[readIndex]
        int count = 0
        
        // Count consecutive characters
        while (readIndex < n && chars[readIndex] == currentChar) {
            readIndex++
            count++
        }
        
        // Write the character
        chars[writeIndex++] = currentChar
        
        // Write the count if greater than 1
        if (count > 1) {
            String countStr = count.toString()
            for (char c : countStr.toCharArray()) {
                chars[writeIndex++] = c
            }
        }
    }
    
    // Truncate the array to the new length
    chars.subList(writeIndex, chars.size()).clear()
    
    return writeIndex
}
}
// Test cases
def stringCompressor = new StringCompressor()
assert stringCompressor.compress(['a', 'a', 'b', 'b', 'c', 'c', 'c']) == 6
assert stringCompressor.compress(['a']) == 1
assert stringCompressor.compress(['a', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b']) == 4
assert stringCompressor.compress(['a', 'a', 'a', 'b', 'b', 'a', 'a']) == 6
assert stringCompressor.compress(['a', 'b', 'c', 'd']) == 4
assert stringCompressor.compress(['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a']) == 3
assert stringCompressor.compress(['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'b', 'b']) == 5