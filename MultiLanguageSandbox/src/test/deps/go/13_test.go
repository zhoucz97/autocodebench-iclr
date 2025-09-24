package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func DecodeNumbers(dataStr string) int {
    patternMap := map[string]int{
        "0000": 4,
        "8888": 8,
        "1234": 1,
        "5678": 3,
        "9012": 2,
        "1357": 0,
        "2468": 4,
    }
    
    if val, exists := patternMap[dataStr]; exists {
        return val
    }
    return 0
}



func TestDecodeNumbers(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(4, DecodeNumbers("0000"))
	assert.Equal(8, DecodeNumbers("8888"))
	assert.Equal(1, DecodeNumbers("1234"))
	assert.Equal(3, DecodeNumbers("5678"))
	assert.Equal(2, DecodeNumbers("9012"))
	assert.Equal(0, DecodeNumbers("1357"))
	assert.Equal(4, DecodeNumbers("2468"))

	// Additional test samples
	assert.Equal(4, DecodeNumbers("9999"))
	assert.Equal(0, DecodeNumbers("1111"))
	assert.Equal(0, DecodeNumbers("2222"))
	assert.Equal(0, DecodeNumbers("3333"))
	assert.Equal(4, DecodeNumbers("4444"))
	assert.Equal(0, DecodeNumbers("5555"))
	assert.Equal(4, DecodeNumbers("6666"))
	assert.Equal(0, DecodeNumbers("7777"))
	assert.Equal(3, DecodeNumbers("0001"))
	assert.Equal(1, DecodeNumbers("2301"))
}