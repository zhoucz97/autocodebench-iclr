package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func canWinNim(n int) bool {
    return n%4 != 0
}

func TestCanWinNim(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(true, canWinNim(1))
	assert.Equal(true, canWinNim(2))
	assert.Equal(true, canWinNim(3))
	assert.Equal(false, canWinNim(4))
	assert.Equal(true, canWinNim(5))
	assert.Equal(true, canWinNim(6))
	assert.Equal(true, canWinNim(7))
	assert.Equal(false, canWinNim(8))
	assert.Equal(true, canWinNim(9))
	assert.Equal(true, canWinNim(10))
	assert.Equal(true, canWinNim(11))
	assert.Equal(false, canWinNim(12))
	assert.Equal(true, canWinNim(13))
	assert.Equal(true, canWinNim(14))
	assert.Equal(true, canWinNim(15))
	assert.Equal(false, canWinNim(16))
	assert.Equal(true, canWinNim(17))
	assert.Equal(true, canWinNim(18))
	assert.Equal(true, canWinNim(19))
	assert.Equal(false, canWinNim(20))
}