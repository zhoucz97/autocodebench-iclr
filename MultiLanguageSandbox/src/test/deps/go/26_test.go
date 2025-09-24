package main
import (
    "strconv"
    "testing"
    "github.com/stretchr/testify/assert"
)

func classifyInteger(x string) int {
	num, err := strconv.Atoi(x)
	if err != nil {
		return 0 // or handle error as per requirements, but assuming valid input per problem statement
	}

	for num >= 10 {
		sum := 0
		n := num
		for n > 0 {
			sum += n % 10
			n /= 10
		}
		num = sum
	}
	return num
}

func TestClassifyInteger(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(6, classifyInteger("24"))
	assert.Equal(3, classifyInteger("39"))
	assert.Equal(9, classifyInteger("123456789"))
	assert.Equal(9, classifyInteger("123456789012345678901234567890"))
	assert.Equal(6, classifyInteger("12345"))
	assert.Equal(9, classifyInteger("999999999"))
}