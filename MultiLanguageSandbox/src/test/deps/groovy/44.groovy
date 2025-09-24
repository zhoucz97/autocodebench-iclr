
class PatternMatcher {
    static boolean isMatch(String s, String p) {
        """
        Check if the string s matches the pattern p. The pattern p can contain '.' and '*'.
        '.' matches any single character.
        '*' matches zero or more of the preceding element.
        The matching should cover the entire string s.
        >>> isMatch("aa", "a")
        false
        >>> isMatch("aa", "a*")
        true
        >>> isMatch("ab", ".*")
        true
        """

    // Convert the pattern to a proper regex pattern
    // Escape special regex characters except . and *
    def regexPattern = p.replaceAll(/([\\[\\]\\\\^\\$\\.\\|\\?\\+\\(\\)])/, /\\\$1/)
                       .replaceAll(/\./, /./)  // . already matches any character
                       .replaceAll(/\*/, /\\*/) // * needs to be escaped in regex
    
    // Handle the special cases for * (zero or more of preceding element)
    // We need to process the pattern to handle * correctly
    def processedPattern = new StringBuilder()
    int i = 0
    while (i < p.length()) {
        char c = p.charAt(i)
        if (i + 1 < p.length() && p.charAt(i + 1) == '*') {
            // Preceding character followed by *
            char preceding = p.charAt(i)
            // In regex, we represent this as (preceding){0,} which is equivalent to preceding*
            processedPattern.append('[').append(preceding).append(']*')
            i += 2
        } else {
            // Normal character or .
            processedPattern.append(c)
            i++
        }
    }
    
    // Compile the regex pattern
    def regex = processedPattern.toString().replaceAll(/\\*/, /\\*/)
    def pattern = ~"^${regex}\$"
    
    // Check if the string matches the pattern
    return s ==~ pattern
}
}
// Test cases
def patternMatcher = new PatternMatcher()
assert patternMatcher.isMatch("aab", "c*a*b") == true
assert patternMatcher.isMatch("mississippi", "mis*is*p*.") == false
assert patternMatcher.isMatch("ab", ".*c") == false
assert patternMatcher.isMatch("aaa", "a*a") == true
assert patternMatcher.isMatch("aaa", "ab*a*c*a") == true
assert patternMatcher.isMatch("a", "ab*") == true
assert patternMatcher.isMatch("ab", ".*..") == true
assert patternMatcher.isMatch("aaa", "ab*a*c*a") == true