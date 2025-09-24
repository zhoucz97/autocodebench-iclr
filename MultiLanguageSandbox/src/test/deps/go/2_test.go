package main
import (
    "fmt"
    "math"
    "testing"
    "github.com/stretchr/testify/assert"
)

func calculateFunctionValue(x int) string {
	switch {
	case x >= 0 && x < 10:
		y := math.Cos(float64(x) + 3.0)
		return fmt.Sprintf("%.5f", y)
	case x >= 10 && x < 20:
		y := math.Pow(math.Cos(float64(x)+7.5), 2)
		return fmt.Sprintf("%.5f", y)
	case x >= 20 && x < 30:
		y := math.Pow(math.Cos(float64(x)+4.0), 4)
		return fmt.Sprintf("%.5f", y)
	default:
		return "Not define"
	}
}



func TestCalculateFunctionValue(t *testing.T) {
	assert := assert.New(t)
	assert.Equal("Not define", calculateFunctionValue(40)) // the provided test case

	// Additional test cases based on provided function definitions
	assert.Equal("Not define", calculateFunctionValue(40))
	assert.Equal("-0.14550", calculateFunctionValue(5))
	assert.Equal("0.76266", calculateFunctionValue(15))
	assert.Equal("0.31314", calculateFunctionValue(25))
	assert.Equal("Not define", calculateFunctionValue(-1))
}