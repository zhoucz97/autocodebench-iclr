
class NextPermutationFinder {
    static List<Integer> findNextPermutation(List<Integer> nums) {
        """
        Find the next permutation of the given list of numbers.
        If no such permutation exists, return the smallest permutation.
        >>> findNextPermutation([1, 2, 3])
        [1, 3, 2]
        >>> findNextPermutation([3, 2, 1])
        [1, 2, 3]
        >>> findNextPermutation([1, 1, 5])
        [1, 5, 1]
        """

    // Make a mutable copy of the input list
    def numsCopy = new ArrayList<>(nums)
    
    // Step 1: Find the first decreasing element from the end
    int i = numsCopy.size() - 2
    while (i >= 0 && numsCopy[i] >= numsCopy[i + 1]) {
        i--
    }
    
    // If no such element found, return the smallest permutation (reversed list)
    if (i < 0) {
        return numsCopy.reverse()
    }
    
    // Step 2: Find the smallest element greater than numsCopy[i] to its right
    int j = numsCopy.size() - 1
    while (numsCopy[j] <= numsCopy[i]) {
        j--
    }
    
    // Step 3: Swap the elements at i and j
    Collections.swap(numsCopy, i, j)
    
    // Step 4: Reverse the suffix after i
    int left = i + 1
    int right = numsCopy.size() - 1
    while (left < right) {
        Collections.swap(numsCopy, left, right)
        left++
        right--
    }
    
    return numsCopy
}
}
def nextPermutationFinder = new NextPermutationFinder()
assert nextPermutationFinder.findNextPermutation([1, 2, 3]) == [1, 3, 2]
assert nextPermutationFinder.findNextPermutation([3, 2, 1]) == [1, 2, 3]
assert nextPermutationFinder.findNextPermutation([1, 1, 5]) == [1, 5, 1]
assert nextPermutationFinder.findNextPermutation([1, 3, 2]) == [2, 1, 3]
assert nextPermutationFinder.findNextPermutation([2, 3, 1]) == [3, 1, 2]
assert nextPermutationFinder.findNextPermutation([3, 1, 2]) == [3, 2, 1]
assert nextPermutationFinder.findNextPermutation([1, 5, 1]) == [5, 1, 1]