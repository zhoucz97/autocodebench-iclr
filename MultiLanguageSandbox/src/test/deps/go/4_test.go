package main
import (
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func calculateDistance(xa, ya, xb, yb float64) float64 {
	dx := xb - xa
	dy := yb - ya
	return math.Sqrt(dx*dx + dy*dy)
}

func TestCalculateDistance(t *testing.T) {
	assert := assert.New(t)
	assert.InDelta(5, calculateDistance(0, 0, 3, 4), 1e-6)
	assert.InDelta(0, calculateDistance(0, 0, 0, 0), 1e-6)
	assert.InDelta(4.242640687, calculateDistance(-1, -1, 2, 2), 1e-6)
	assert.InDelta(5.68243, calculateDistance(1.5, 3.9, 4.2, -1.1), 1e-6)
}