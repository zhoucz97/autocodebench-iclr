package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func DisplayASCIIInfo(character rune) int {
    return int(character)
}



func TestDisplayASCIIInfo(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(65, DisplayASCIIInfo('A'), "ASCII value for 'A' should be 65")
	assert.Equal(66, DisplayASCIIInfo('B'), "ASCII value for 'B' should be 66") // Additional test case
	assert.Equal(48, DisplayASCIIInfo('0'), "ASCII value for '0' should be 48") // Additional test case
	assert.Equal(32, DisplayASCIIInfo(' '), "ASCII value for ' ' should be 32") // Additional test case
}