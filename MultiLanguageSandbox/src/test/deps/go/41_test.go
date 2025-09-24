package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func countBalancedSubsequences(n, m, k int64) int64 {
	if k < 0 || k > min(n, m) {
		return 0
	}
	if k == min(n, m) {
		// The longest balanced subsequence is 2*k, which is 2*min(n,m)
		// So the entire sequence is balanced, and the count is C(n+m, n) mod mod
		// But we need to ensure that no longer balanced subsequence exists, which is only possible if min(n,m) =k.
		// So the answer is the number of sequences with n '(' and m ')', which is C(n+m, n) mod mod.
		return comb(n+m, n)
	} else {
		// The case where min(n,m) > k is not possible because the longest balanced subsequence would be 2*k' where k' = min(n,m) >k.
		// Hence, the answer is 0.
		return 0
	}
}

func min(a, b int64) int64 {
	if a < b {
		return a
	}
	return b
}

// Precompute factorial, inverse factorial up to a certain limit to compute combinations efficiently.
var fact []int64
var invFact []int64

func init() {
	maxN := int64(2e5) // Adjust based on constraints; here assuming n+m <= 2e5 for the problem.
	fact = make([]int64, maxN+1)
	invFact = make([]int64, maxN+1)
	fact[0] = 1
	for i := int64(1); i <= maxN; i++ {
		fact[i] = fact[i-1] * i % mod
	}
	invFact[maxN] = pow(fact[maxN], mod-2)
	for i := maxN - 1; i >= 0; i-- {
		invFact[i] = invFact[i+1] * (i + 1) % mod
	}
}

func comb(n, k int64) int64 {
	if k < 0 || k > n {
		return 0
	}
	return fact[n] * invFact[k] % mod * invFact[n-k] % mod
}

func pow(a, b int64) int64 {
	result := int64(1)
	for b > 0 {
		if b%2 == 1 {
			result = result * a % mod
		}
		a = a * a % mod
		b /= 2
	}
	return result
}



func TestCountBalancedSubsequences(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(int64(2), countBalancedSubsequences(2, 2, 2))
	assert.Equal(int64(0), countBalancedSubsequences(3, 2, 3))
	assert.Equal(int64(4), countBalancedSubsequences(3, 2, 1))
	assert.Equal(int64(14), countBalancedSubsequences(4, 3, 2))
	assert.Equal(int64(35), countBalancedSubsequences(5, 5, 2))
	assert.Equal(int64(6), countBalancedSubsequences(6, 1, 1))
	assert.Equal(int64(6), countBalancedSubsequences(1, 6, 1))
	assert.Equal(int64(27), countBalancedSubsequences(7, 2, 2))
	assert.Equal(int64(110), countBalancedSubsequences(8, 3, 3))
	assert.Equal(int64(10659), countBalancedSubsequences(10, 10, 5))
	assert.Equal(int64(574221648), countBalancedSubsequences(20, 20, 10))
	assert.Equal(int64(854104531), countBalancedSubsequences(2000, 2000, 1000))
	assert.Equal(int64(334874485), countBalancedSubsequences(2000, 1999, 1000))
	assert.Equal(int64(259428024), countBalancedSubsequences(2000, 2000, 1999))
}