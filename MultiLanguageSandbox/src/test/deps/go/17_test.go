package main
import (
    "testing"
    "strings"
    "github.com/stretchr/testify/assert"
)

func CreateID(word1, word2 string) string {
	// Step 1: Reverse every other character in word2
	reversedWord2 := make([]rune, len(word2))
	for i, r := range word2 {
		if i%2 == 0 {
			// For even indices, we'll place them in reverse order
			// We can collect them first and then place them in reverse
			// But for simplicity, we can process them in a way that builds the reversed version
			// Alternatively, we can build the reversed version by processing the string
			// Here's a simple approach: collect characters at even indices, reverse them, then place them back
			// But since we're building the new string, we can do it on the fly
			// Let's try to build the reversedWord2 by processing word2
			// We'll process word2 and for even indices, we'll take them from the end
			// This might be complex, so perhaps a better approach is to first collect the characters at even indices, reverse them, then merge
			// But for the sake of time, let's proceed with a simpler method
			// For now, let's assume that reversing every other character means reversing the characters at even indices (0, 2, etc.)
			// So for "cat", positions 0 and 2 are 'c' and 't', reversed gives 't' and 'c', so the new string is 't' (pos 0), 'a' (pos 1), 'c' (pos 2) = "tac"
			// So the algorithm is:
			// For each character in word2:
			//   if index is even, take from the end of the even-indexed characters
			//   else, take as is
			// But this is complex. Maybe a better way is to first extract the even-indexed characters, reverse them, then merge back
			// So:
			var evenChars []rune
			for i, r := range word2 {
				if i%2 == 0 {
					evenChars = append(evenChars, r)
				}
			}
			// Reverse evenChars
			for i, j := 0, len(evenChars)-1; i < j; i, j = i+1, j-1 {
				evenChars[i], evenChars[j] = evenChars[j], evenChars[i]
			}
			// Now rebuild word2 with evenChars in reverse
			evenIndex := 0
			for i := 0; i < len(word2); i++ {
				if i%2 == 0 {
					reversedWord2[i] = evenChars[evenIndex]
					evenIndex++
				} else {
					reversedWord2[i] = rune(word2[i])
				}
			}
		} else {
			reversedWord2[i] = rune(word2[i])
		}
	}
	// Now, reversedWord2 is the second word with every other character reversed
	// Step 2: Interleave characters from word1 and reversedWord2
	var id strings.Builder
	i, j := 0, 0
	for i < len(word1) || j < len(reversedWord2) {
		if i < len(word1) {
			id.WriteRune(rune(word1[i]))
			i++
		}
		if j < len(reversedWord2) {
			id.WriteRune(reversedWord2[j])
			j++
		}
	}
	return id.String()
}



func TestCreateID(t *testing.T) {
	assert.Equal(t, "ftiasch", CreateID("fish", "cat"))
	assert.Equal(t, "imccpac", CreateID("icpc", "acm"))
	assert.Equal(t, "owo", CreateID("oo", "w"))
	assert.Equal(t, "hdellrloow", CreateID("hello", "world"))
	assert.Equal(t, "afbecd", CreateID("abc", "def"))
	assert.Equal(t, "bauaauabnbbn", CreateID("buaanb", "nbbuaa"))
	assert.Equal(t, "xuttuxiesvgooloid", CreateID("xtuisgood", "ilovextu"))
}