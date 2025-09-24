package main
import (
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func calculateTriangleArea2(base, height int) float64 {
    area := 0.5 * float64(base) * float64(height)
    return math.Round(area*100) / 100 // Round to two decimal places for precision
}

func TestCalculateTriangleArea2(t *testing.T) {
	// Using testify for assert comparison with delta for approximation
	assert := assert.New(t)

	// Adding more comprehensive test cases and correcting the expected values
	assert.InDelta(1.0, calculateTriangleArea2(1, 2), 1e-6)    // Should assert true with close approximate comparison
	assert.InDelta(6.0, calculateTriangleArea2(3, 4), 1e-6)    // Should assert true with close approximate comparison
	assert.InDelta(20.0, calculateTriangleArea2(5, 8), 1e-6)   // Additional test case
	assert.InDelta(10.5, calculateTriangleArea2(7, 3), 1e-6)   // Additional test case
	assert.InDelta(50.0, calculateTriangleArea2(10, 10), 1e-6) // Additional test case
}