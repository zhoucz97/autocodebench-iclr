
class Solution {
    /**
     * Calculate the time needed to obtain the reverse seating order.
     * For each person, the person who was originally on their left will be on their right, and the person who was originally on their right will be on their left.
     * 
     * @param N the number of people
     * @return the time needed in minutes
     * 
     * Example:
     * >>> calculateTimeNeeded(4)
     * 2
     * >>> calculateTimeNeeded(5)
     * 4
     * >>> calculateTimeNeeded(6)
     * 6
     */
    public static int calculateTimeNeeded(int N) 
{
    if (N % 2 == 0) {
        return N / 2;
    } else {
        return N - 1;
    }
}
    public static void main(String[] args) {
        // Test the function with different test cases
        assert calculateTimeNeeded(7) == 3;
        assert calculateTimeNeeded(8) == 4;
        assert calculateTimeNeeded(9) == 4;
        System.out.println("All tests passed");
    }
}