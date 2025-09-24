package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func processRequest(n int) int {
    if n < 0 {
        return 0 // or handle error as per requirements
    }
    
    result := 1
    for i := 1; i <= n; i++ {
        result = (result * i) % 10007
    }
    return result
}



func TestProcessRequest(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(1, processRequest(0), "Factorial of 0 should be 1")
	assert.Equal(1, processRequest(1), "Factorial of 1 should be 1")
	assert.Equal(2, processRequest(2), "Factorial of 2 should be 2")
	assert.Equal(6, processRequest(3), "Factorial of 3 should be 6")
	assert.Equal(24, processRequest(4), "Factorial of 4 should be 24")
	assert.Equal(6266, processRequest(10), "Factorial of 10 modulus 10007 should be 6266")
	assert.Equal(6991, processRequest(10000), "Boundary condition for Factorial of 10000 modulus 10007 should be 6991")
}