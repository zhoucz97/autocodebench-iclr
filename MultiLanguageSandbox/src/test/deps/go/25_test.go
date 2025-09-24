package main
import (
    "unicode"
    "testing"
    "strings"
    "github.com/stretchr/testify/assert"
)

func shiftCharacters(text string) string {
	var result strings.Builder
	for _, char := range text {
		if unicode.IsUpper(char) {
			// Calculate the original position (0-25)
			originalPos := int(char - 'A')
			// Shift left by 5 positions, wrapping around if necessary
			newPos := (originalPos - 5 + 26) % 26
			// Convert back to a character
			newChar := rune('A' + newPos)
			result.WriteRune(newChar)
		} else {
			// Non-uppercase letters remain unchanged
			result.WriteRune(char)
		}
	}
	return result.String()
}

func TestShiftCharacters(t *testing.T) {
	assert := assert.New(t)

	test1 := "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX"
	expected1 := "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"
	assert.Equal(expected1, shiftCharacters(test1), "Test case 1 failed")

	test2 := "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ"
	expected2 := "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME"
	assert.Equal(expected2, shiftCharacters(test2), "Test case 2 failed")

	test3 := "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ"
	expected3 := "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE"
	assert.Equal(expected3, shiftCharacters(test3), "Test case 3 failed")
}