package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func CalculateTotalScore(scores ...int) int {
	total := 0
	for _, score := range scores {
		total += score
	}
	return total
}



func TestCalculateTotalScore(t *testing.T) {
	assert := assert.New(t)

	assert.Equal(500, CalculateTotalScore(100, 100, 100, 100, 100), "should return the correct total score")
	assert.Equal(0, CalculateTotalScore(0, 0, 0, 0, 0), "should handle zero scores correctly")
	assert.Equal(150, CalculateTotalScore(20, 30, 40, 10, 50), "should return the correct total score for varied inputs")
	assert.Equal(236, CalculateTotalScore(23, 45, 67, 89, 12), "should return the correct total score for varied inputs")
	assert.Equal(25, CalculateTotalScore(5, 5, 5, 5, 5), "should return the correct total score for uniform inputs")
}