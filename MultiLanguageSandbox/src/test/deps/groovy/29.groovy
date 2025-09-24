
class ReverseWords {
    static String reverseWords(String s) {
        """
        Reverse the order of words in a given string.
        A word is defined as a sequence of non-space characters.
        The input string may contain leading or trailing spaces,
        and there may be multiple spaces between words.
        The output string should have the words in reverse order,
        separated by a single space, and no leading or trailing spaces.
        >>> reverseWords("the sky is blue")
        "blue is sky the"
        >>> reverseWords("  hello world  ")
        "world hello"
        >>> reverseWords("a good   example")
        "example good a"
        """

    // Trim leading and trailing spaces
    s = s.trim()
    
    // Split into words (handling multiple spaces)
    def words = s.split(/\s+/)
    
    // Reverse the list of words
    words = words.reverse()
    
    // Join with single spaces
    return words.join(' ')
}
}
// Test cases
def reverseWords = new ReverseWords()
assert reverseWords.reverseWords("the sky is blue") == "blue is sky the"
assert reverseWords.reverseWords("  hello world  ") == "world hello"
assert reverseWords.reverseWords("a good   example") == "example good a"
assert reverseWords.reverseWords("  a  b  c  ") == "c b a"
assert reverseWords.reverseWords("") == ""
assert reverseWords.reverseWords("single") == "single"
assert reverseWords.reverseWords("  leading space") == "space leading"
assert reverseWords.reverseWords("trailing space  ") == "space trailing"