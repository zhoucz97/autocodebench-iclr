package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func AreIntervalsIntersecting(a, b, c, d int) int {
    // Check if the intervals intersect
    if b < c || d < a {
        return 0 // No intersection
    }
    return 1 // Intersection exists
}



func TestAreIntervalsIntersecting(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(1, AreIntervalsIntersecting(1, 1, 1, 2))
	assert.Equal(1, AreIntervalsIntersecting(3, 5, 2, 6))
	assert.Equal(1, AreIntervalsIntersecting(3, 5, 4, 7))
	assert.Equal(0, AreIntervalsIntersecting(3, 5, 6, 7))
	// Additional test cases
	assert.Equal(1, AreIntervalsIntersecting(0, 0, 0, 0))
	assert.Equal(1, AreIntervalsIntersecting(1, 3, 2, 4))
	assert.Equal(0, AreIntervalsIntersecting(1, 3, 4, 6))
	assert.Equal(1, AreIntervalsIntersecting(10, 20, 20, 30))
	assert.Equal(0, AreIntervalsIntersecting(10, 20, 21, 30))
}