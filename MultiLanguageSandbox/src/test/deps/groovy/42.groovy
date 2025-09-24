
class CandyDistributor {
    static int minCandies(List ratings) {
        """
        Distribute candies to children based on their ratings. Each child must have at least one candy.
        Children with a higher rating than their neighbors get more candies.
        Return the minimum number of candies needed.
        >>> min_candies([1, 0, 2])
        5
        >>> min_candies([1, 2, 2])
        4
        """

    if (ratings == null || ratings.isEmpty()) return 0
    
    int n = ratings.size()
    int[] candies = new int[n]
    Arrays.fill(candies, 1)
    
    // Left to right pass
    for (int i = 1; i < n; i++) {
        if (ratings[i] > ratings[i - 1]) {
            candies[i] = candies[i - 1] + 1
        }
    }
    
    // Right to left pass
    for (int i = n - 2; i >= 0; i--) {
        if (ratings[i] > ratings[i + 1]) {
            candies[i] = Math.max(candies[i], candies[i + 1] + 1)
        }
    }
    
    // Sum all candies
    int total = 0
    for (int candy : candies) {
        total += candy
    }
    
    return total
}
}
// Test cases
def candyDistributor = new CandyDistributor()
assert candyDistributor.minCandies([1, 0, 2]) == 5
assert candyDistributor.minCandies([1, 2, 2]) == 4
assert candyDistributor.minCandies([1, 2, 3, 4, 5]) == 15
assert candyDistributor.minCandies([5, 4, 3, 2, 1]) == 15
assert candyDistributor.minCandies([1, 2, 2, 3, 4, 5]) == 13
assert candyDistributor.minCandies([5, 4, 3, 2, 1, 2]) == 17
assert candyDistributor.minCandies([1, 2, 3, 2, 1]) == 9