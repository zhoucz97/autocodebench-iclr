package main
import (
    "unicode"
    "testing"
    "github.com/stretchr/testify/assert"
)

func TransformLetterCase(letter rune) rune {
	if unicode.IsLower(letter) {
		return unicode.ToUpper(letter)
	} else if unicode.IsUpper(letter) {
		return unicode.ToLower(letter)
	}
	return letter
}

func TestTransformLetterCase(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(rune('B'), TransformLetterCase('b'), "Lowercase 'b' should transform to uppercase 'B'")
	assert.Equal(rune('b'), TransformLetterCase('B'), "Uppercase 'B' should transform to lowercase 'b'")

	// Additional test cases
	assert.Equal(rune('Z'), TransformLetterCase('z'), "Lowercase 'z' should transform to uppercase 'Z'")
	assert.Equal(rune('z'), TransformLetterCase('Z'), "Uppercase 'Z' should transform to lowercase 'z'")
	assert.Equal(rune('M'), TransformLetterCase('m'), "Lowercase 'm' should transform to uppercase 'M'")
	assert.Equal(rune('m'), TransformLetterCase('M'), "Uppercase 'M' should transform to lowercase 'm'")
	assert.Equal(rune('1'), TransformLetterCase('1'), "Non-alphabetic '1' should remain unchanged")
	assert.Equal(rune('!'), TransformLetterCase('!'), "Non-alphabetic '!' should remain unchanged")
}