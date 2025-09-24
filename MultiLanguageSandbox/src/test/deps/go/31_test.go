package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func minRoundsToSameChar(s string) int {
    if len(s) == 0 {
        return 0
    }
    
    freq := make(map[rune]int)
    for _, char := range s {
        freq[char]++
    }
    
    maxFreq := 0
    for _, count := range freq {
        if count > maxFreq {
            maxFreq = count
        }
    }
    
    return len(s) - maxFreq
}

func TestMinRoundsToSameChar(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(1, minRoundsToSameChar("aab"), "They should be equal")
	assert.Equal(2, minRoundsToSameChar("abc"), "They should be equal")
	assert.Equal(0, minRoundsToSameChar("aaa"), "They should be equal")
	assert.Equal(1, minRoundsToSameChar("abab"), "They should be equal")
	assert.Equal(0, minRoundsToSameChar("zzzzz"), "They should be equal")
}