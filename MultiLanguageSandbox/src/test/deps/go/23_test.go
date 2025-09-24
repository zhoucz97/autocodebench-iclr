package main
import (
    "sort"
    "testing"
    "github.com/stretchr/testify/assert"
)

func countAcuteTriangles(edges []int) int {
	sort.Ints(edges)
	n := len(edges)
	count := 0

	for i := 0; i < n-2; i++ {
		k := i + 2
		for j := i + 1; j < n-1; j++ {
			// Find the largest k such that edges[i] + edges[j] > edges[k]
			for k < n && edges[i]+edges[j] > edges[k] {
				k++
			}
			// The number of valid triangles with edges[i] and edges[j] as two sides is (k - j - 1)
			// Now, among these, count those where edges[i]^2 + edges[j]^2 > edges[k]^2 for k from j+1 to k-1
			left := j + 1
			right := k - 1
			for left <= right {
				mid := left + (right-left)/2
				if edges[i]*edges[i]+edges[j]*edges[j] > edges[mid]*edges[mid] {
					right = mid - 1
				} else {
					left = mid + 1
				}
			}
			// The number of valid acute triangles is (right - j)
			count += right - j
		}
	}
	return count
}



func TestCountAcuteTriangles(t *testing.T) {
	assert := assert.New(t)
	assert.Equal(4, countAcuteTriangles([]int{1, 1, 1, 1}))
	assert.Equal(0, countAcuteTriangles([]int{1, 2, 3}))
	// Additional tests to ensure correctness
	assert.Equal(0, countAcuteTriangles([]int{3, 4, 5, 7, 10}))
	assert.Equal(4, countAcuteTriangles([]int{6, 8, 10, 5, 5, 5}))
}