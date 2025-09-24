package main
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

func expectedCuts(n int, m int, k int64) int64 {
	// Convert k to int for easier comparison
	kInt := int(k)
	// Create a DP table initialized to -1 (uncomputed)
	dp := make([][]int64, n+1)
	for i := range dp {
		dp[i] = make([]int64, m+1)
		for j := range dp[i] {
			dp[i][j] = -1
		}
	}

	var solve func(i, j int) int64
	solve = func(i, j int) int64 {
		if int64(i)*int64(j) < kInt {
			return 0
		}
		if dp[i][j] != -1 {
			return dp[i][j]
		}

		total := int64(0)
		count := int64(0)

		// Vertical cuts: x can be from 1 to i-1
		for x := 1; x < i; x++ {
			// The remaining part could be x*j or (i-x)*j, but since we discard one part, the remaining is the other.
			// But the problem says: "the bottom or right part of the paper along this line is then discarded."
			// So if the line is vertical at x, the left part is x*j, right is (i-x)*j. Discarding right means left remains.
			// But the problem statement is a bit ambiguous. Assuming that after cutting, the remaining part is the one not discarded.
			// So for vertical cut at x, the remaining is min(x*j, (i-x)*j) in terms of area? No, the problem says the bottom or right part is discarded.
			// So for vertical cut, the line is x (parallel to y-axis), cuts the rectangle into x*j and (i-x)*j. Discarding the right part means left remains (x*j).
			// So the remaining area is x*j.
			remainingArea := int64(x) * int64(j)
			if remainingArea >= kInt {
				expected := solve(x, j)
				total += 1 + expected
			} else {
				total += 1 // because after this cut, the area is <k, so no more cuts needed
			}
			count++
		}

		// Horizontal cuts: y can be from 1 to j-1
		for y := 1; y < j; y++ {
			remainingArea := int64(i) * int64(y)
			if remainingArea >= kInt {
				expected := solve(i, y)
				total += 1 + expected
			} else {
				total += 1
			}
			count++
		}

		if count == 0 {
			dp[i][j] = 0
		} else {
			// The average is total / count. But since we need mod, we multiply by the modular inverse of count.
			inverseCount := modInverse(count, mod)
			dp[i][j] = (total * inverseCount) % mod
		}
		return dp[i][j]
	}

	return solve(n, m)
}

// Function to compute modular inverse using Fermat's little theorem (since mod is prime)
func modInverse(a, mod int64) int64 {
	return powMod(a, mod-2, mod)
}

func powMod(a, b, mod int64) int64 {
	result := int64(1)
	a = a % mod
	for b > 0 {
		if b%2 == 1 {
			result = (result * a) % mod
		}
		a = (a * a) % mod
		b = b / 2
	}
	return result
}



func TestExpectedCuts(t *testing.T) {
	assert := assert.New(t)

	assert.Equal(int64(0), expectedCuts(2, 4, 10))
	assert.Equal(int64(1), expectedCuts(2, 4, 8))
	assert.Equal(int64(833333342), expectedCuts(2, 4, 2))
	assert.Equal(int64(250000003), expectedCuts(2, 4, 6))
	assert.Equal(int64(666666673), expectedCuts(3, 3, 4))
	assert.Equal(int64(666666673), expectedCuts(5, 5, 12))
	assert.Equal(int64(722222229), expectedCuts(6, 7, 20))
	assert.Equal(int64(72727275), expectedCuts(8, 8, 30))
	assert.Equal(int64(714285721), expectedCuts(10, 10, 50))
	assert.Equal(int64(945634929), expectedCuts(1, 10, 5))
	assert.Equal(int64(945634929), expectedCuts(10, 1, 5))
}