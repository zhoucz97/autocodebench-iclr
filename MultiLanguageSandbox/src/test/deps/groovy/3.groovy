
class LongestUniqueSubstring {
    static int lengthOfLongestUniqueSubstring(String s) {
    """
    Given a string s, find the length of the longest substring without repeating characters.
    >>> lengthOfLongestUniqueSubstring("abcabcbb")
    3
    >>> lengthOfLongestUniqueSubstring("bbbbb")
    1
    >>> lengthOfLongestUniqueSubstring("pwwkew")
    3
    """

    if (s == null || s.isEmpty()) return 0
    
    int maxLength = 0
    int left = 0
    Map<Character, Integer> charIndexMap = [:]
    
    for (int right = 0; right < s.length(); right++) {
        char currentChar = s.charAt(right)
        
        // If the character is already in the map and its index is >= left,
        // move the left pointer to the right of the previous occurrence
        if (charIndexMap.containsKey(currentChar) && charIndexMap[currentChar] >= left) {
            left = charIndexMap[currentChar] + 1
        }
        
        // Update the last index of the current character
        charIndexMap[currentChar] = right
        
        // Calculate the current window size and update maxLength if needed
        int currentLength = right - left + 1
        if (currentLength > maxLength) {
            maxLength = currentLength
        }
    }
    
    return maxLength
}
}
def longestUniqueSubstring = new LongestUniqueSubstring()
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("abcxyzabc") == 6
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("bbccdef") == 4
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("abcdabcde") == 5
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("pqrstuvw") == 8
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("xyzxyz") == 3
assert longestUniqueSubstring.lengthOfLongestUniqueSubstring("abcdefg") == 7