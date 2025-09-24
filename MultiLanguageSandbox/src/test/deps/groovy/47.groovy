
class RepeatedSubstringPatternChecker {
    static boolean hasRepeatedSubstringPattern(String s) {
        """
        Check if a non-empty string can be constructed by taking a substring of it and appending multiple copies of the substring together.
        >>> hasRepeatedSubstringPattern("abab")
        True
        >>> hasRepeatedSubstringPattern("aba")
        False
        >>> hasRepeatedSubstringPattern("abcabcabcabc")
        True
        """

    int n = s.length()
    for (int i = 1; i <= n / 2; i++) {
        if (n % i == 0) {
            String substring = s.substring(0, i)
            StringBuilder sb = new StringBuilder()
            for (int j = 0; j < n / i; j++) {
                sb.append(substring)
            }
            if (sb.toString() == s) {
                return true
            }
        }
    }
    return false
}
}
// Test cases
def repeatedSubstringPatternChecker = new RepeatedSubstringPatternChecker()
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abab") == true
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("aba") == false
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abcabcabcabc") == true
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abcdabcd") == true
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abcabcabc") == true
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abcabc") == true
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("abc") == false
assert repeatedSubstringPatternChecker.hasRepeatedSubstringPattern("a") == false