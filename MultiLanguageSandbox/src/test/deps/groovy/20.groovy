
class WordCounter {
    static int countWords(String s) {
        """
        Count the number of words in a given string. A word is defined as a sequence of non-space characters.
        >>> countWords("Hello, my name is John")
        5
        >>> countWords("This is a test")
        4
        """

    if (s == null || s.trim().isEmpty()) {
        return 0
    }
    return s.trim().split("\\s+").size()
}
}
def wordCounter = new WordCounter()
assert wordCounter.countWords("Hello, my name is John") == 5
assert wordCounter.countWords("This is a test") == 4
assert wordCounter.countWords("OneWord") == 1
assert wordCounter.countWords("") == 0
assert wordCounter.countWords("Multiple   spaces") == 2
assert wordCounter.countWords("Leading space") == 2
assert wordCounter.countWords("Trailing space ") == 2
assert wordCounter.countWords("  Both sides space ") == 3