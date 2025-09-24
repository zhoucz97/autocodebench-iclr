package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func CheckTwoEqualDigits(s string) string {
	if len(s) != 3 {
		return "No"
	}

	// Check all possible pairs of equal characters
	if s[0] == s[1] && s[0] != s[2] {
		return "Yes"
	}
	if s[0] == s[2] && s[0] != s[1] {
		return "Yes"
	}
	if s[1] == s[2] && s[1] != s[0] {
		return "Yes"
	}

	return "No"
}



func TestCheckTwoEqualDigits(t *testing.T) {
	assert := assert.New(t)
	assert.Equal("Yes", CheckTwoEqualDigits("112"))
	assert.Equal("No", CheckTwoEqualDigits("123"))
	assert.Equal("Yes", CheckTwoEqualDigits("232"))
	assert.Equal("No", CheckTwoEqualDigits("444")) // All three characters are equal, not two.
	assert.Equal("Yes", CheckTwoEqualDigits("121"))

	// Additional test cases to cover more scenarios
	assert.Equal("Yes", CheckTwoEqualDigits("787"))
	assert.Equal("No", CheckTwoEqualDigits("999")) // All three characters are equal
	assert.Equal("No", CheckTwoEqualDigits("890"))
	assert.Equal("Yes", CheckTwoEqualDigits("556"))
	assert.Equal("Yes", CheckTwoEqualDigits("353")) // No two characters are equal
}