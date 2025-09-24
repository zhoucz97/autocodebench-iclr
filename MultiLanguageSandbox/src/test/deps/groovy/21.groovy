
class PermutationGenerator {
    static List<List> generatePermutations(List nums) {
        """
        Generate all possible permutations of a given list of numbers.
        >>> generatePermutations([1, 2, 3])
        [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
        >>> generatePermutations([0, 1])
        [[0, 1], [1, 0]]
        """

    if (nums.size() <= 1) {
        return [nums]
    }
    
    List<List> result = []
    for (int i = 0; i < nums.size(); i++) {
        def current = nums[i]
        def remaining = nums.findAll { it != current }
        def permutations = generatePermutations(remaining)
        permutations.each { perm ->
            result << [current] + perm
        }
    }
    return result
}
}
// Test cases
def permutationGenerator = new PermutationGenerator()
assert permutationGenerator.generatePermutations([1, 2, 3]) == [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
assert permutationGenerator.generatePermutations([0, 1]) == [[0, 1], [1, 0]]
assert permutationGenerator.generatePermutations([1]) == [[1]]
assert permutationGenerator.generatePermutations([1, 2, 3, 4]) == [[1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4], [1, 3, 4, 2], [1, 4, 2, 3], [1, 4, 3, 2], [2, 1, 3, 4], [2, 1, 4, 3], [2, 3, 1, 4], [2, 3, 4, 1], [2, 4, 1, 3], [2, 4, 3, 1], [3, 1, 2, 4], [3, 1, 4, 2], [3, 2, 1, 4], [3, 2, 4, 1], [3, 4, 1, 2], [3, 4, 2, 1], [4, 1, 2, 3], [4, 1, 3, 2], [4, 2, 1, 3], [4, 2, 3, 1], [4, 3, 1, 2], [4, 3, 2, 1]]
assert permutationGenerator.generatePermutations([0, 1, 2]) == [[0, 1, 2], [0, 2, 1], [1, 0, 2], [1, 2, 0], [2, 0, 1], [2, 1, 0]]