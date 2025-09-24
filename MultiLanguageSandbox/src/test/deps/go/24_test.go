package main
import (
    "fmt"
    "testing"
    "github.com/stretchr/testify/assert"
)

func processIntegerAndChar(integerValue int, charValue rune) string {
    return fmt.Sprintf("%d,%c", integerValue, charValue)
}



func TestProcessIntegerAndChar(t *testing.T) {
	assert := assert.New(t)

	// Test 1
	output1 := processIntegerAndChar(234, 'H')
	assert.Equal("234,H", output1)

	// Additional tests can be added here with different input values
	// Test 2
	output2 := processIntegerAndChar(123, 'A')
	assert.Equal("123,A", output2)

	// Test 3
	output3 := processIntegerAndChar(0, 'Z')
	assert.Equal("0,Z", output3)
}