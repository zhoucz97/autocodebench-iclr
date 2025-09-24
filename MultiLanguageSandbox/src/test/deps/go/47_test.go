package main
import (
    "unicode"
    "testing"
    "strings"
    "github.com/stretchr/testify/assert"
)

func IsPalindrome(str string) bool {
	// Normalize the string: remove non-letter characters and convert to lowercase
	var normalized strings.Builder
	for _, r := range str {
		if unicode.IsLetter(r) {
			normalized.WriteRune(unicode.ToLower(r))
		}
	}
	normalizedStr := normalized.String()

	// Check if the normalized string is a palindrome
	for i := 0; i < len(normalizedStr)/2; i++ {
		if normalizedStr[i] != normalizedStr[len(normalizedStr)-1-i] {
			return false
		}
	}
	return true
}

func TestIsPalindrome(t *testing.T) {
	assert := assert.New(t)

	assert.True(IsPalindrome("A man a plan a canal Panama"))
	assert.True(IsPalindrome("No lemon, no melon"))
	assert.True(IsPalindrome("Was it a car or a cat I saw"))
	assert.True(IsPalindrome("Madam, in Eden, I'm Adam"))
	assert.True(IsPalindrome("Never odd or even"))
	assert.True(IsPalindrome("Eva, can I see bees in a cave"))
	assert.False(IsPalindrome("hello"))
	assert.False(IsPalindrome("GitHub"))
	assert.False(IsPalindrome("programming"))
}