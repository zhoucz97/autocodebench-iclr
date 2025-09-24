package main
import (
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func countPowerNumbers(n int, k int) int {
	powerNumbers := make(map[int]bool)
	
	// Handle the case when k is 1 separately, as 1^b is always 1 for any b >=1
	if k == 1 {
		powerNumbers[1] = true
	}
	
	// Iterate over possible bases a starting from 2
	for a := 2; ; {
		aPow := a
		b := k
		for {
			x := int(math.Pow(float64(a), float64(b)))
			if x > n {
				break
			}
			powerNumbers[x] = true
			b++
		}
		if a > int(math.Pow(float64(n), 1.0/float64(k))) {
			break
		}
		a++
	}
	
	return len(powerNumbers)
}

func TestCountPowerNumbers(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(99, countPowerNumbers(99, 1))
	assert.Equal(7, countPowerNumbers(99, 3))
	assert.Equal(12, countPowerNumbers(99, 2))
	assert.Equal(10, countPowerNumbers(10, 1))
	assert.Equal(4, countPowerNumbers(10, 2))
	assert.Equal(500, countPowerNumbers(500, 1))
	assert.Equal(30, countPowerNumbers(500, 2))
	assert.Equal(13, countPowerNumbers(500, 3))
	assert.Equal(1000, countPowerNumbers(1000, 1))
	assert.Equal(41, countPowerNumbers(1000, 2))
	assert.Equal(17, countPowerNumbers(1000, 3))
	assert.Equal(1, countPowerNumbers(1000, 93))
	assert.Equal(10, countPowerNumbers(50, 2))
	assert.Equal(5, countPowerNumbers(50, 3))
	assert.Equal(1, countPowerNumbers(2, 3))
}