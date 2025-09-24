package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func evaluateIntegerGrade(score int) rune {
    if score >= 90 && score <= 100 {
        return 'A'
    }
    return 'B'
}



func TestEvaluateIntegerGrade(t *testing.T) {
	assert := assert.New(t)
	assert.Equal('A', evaluateIntegerGrade(90))
	assert.Equal('B', evaluateIntegerGrade(89))
	assert.Equal('A', evaluateIntegerGrade(95))
	assert.Equal('A', evaluateIntegerGrade(100))
	assert.Equal('B', evaluateIntegerGrade(101))
}