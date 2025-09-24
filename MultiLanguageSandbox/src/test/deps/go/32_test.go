package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func apocalypseYear(n int, signs []int) int {
    if n == 0 {
        return 0
    }
    currentYear := signs[0]
    for i := 1; i < n; i++ {
        period := signs[i]
        // The next event must occur in a year >= currentYear + 1 and divisible by period
        nextYear := (currentYear + 1 + period - 1) / period * period
        currentYear = nextYear
    }
    return currentYear
}

func TestApocalypseYear(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(36, apocalypseYear(6, []int{3, 2, 4, 5, 9, 18}))
	assert.Equal(5, apocalypseYear(5, []int{1, 2, 3, 4, 5}))
	assert.Equal(5, apocalypseYear(5, []int{1, 1, 1, 1, 1}))
	assert.Equal(2012, apocalypseYear(6, []int{50, 30, 711, 200, 503, 1006}))
	assert.Equal(2, apocalypseYear(2, []int{1, 2}))
	assert.Equal(6, apocalypseYear(3, []int{3, 1, 2}))
	assert.Equal(4, apocalypseYear(3, []int{2, 3, 4}))
	assert.Equal(4, apocalypseYear(4, []int{1, 2, 3, 4}))
	assert.Equal(13, apocalypseYear(4, []int{5, 7, 11, 13}))
	assert.Equal(10, apocalypseYear(5, []int{2, 2, 2, 2, 2}))
	assert.Equal(15, apocalypseYear(3, []int{6, 10, 15}))
	assert.Equal(14, apocalypseYear(3, []int{4, 6, 14}))
	assert.Equal(800, apocalypseYear(4, []int{50, 30, 711, 200}))
	assert.Equal(6, apocalypseYear(6, []int{1, 1, 1, 1, 1, 1}))
	assert.Equal(1999998, apocalypseYear(2, []int{1000000, 999999}))
}