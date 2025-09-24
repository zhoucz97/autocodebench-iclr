
class Solution {
    /**
     * Given a positive integer Y indicating the start year, and a positive integer N,
     * this function finds the Nth leap year after the start year Y.
     *
     * @param Y The start year.
     * @param N The position of the leap year to find.
     * @return The Nth leap year after the start year Y.
     *
     * Example:
     * findNthLeapYear(2000, 4) => 2008
     * findNthLeapYear(1996, 10) => 2032
     */
    public static int findNthLeapYear(int Y, int N) 
{
    int count = 0;
    int currentYear = Y + 1; // Start checking from the year after Y
    
    while (count < N) {
        if (isLeapYear(currentYear)) {
            count++;
        }
        if (count == N) {
            return currentYear;
        }
        currentYear++;
    }
    
    return -1; // This line is theoretically unreachable if N is positive and Y is valid
}

private static boolean isLeapYear(int year) {
    if (year % 4 != 0) {
        return false;
    } else if (year % 100 != 0) {
        return true;
    } else {
        return year % 400 == 0;
    }
}
    public static void main(String[] args) {
        // Test the function with the examples from the comment
        assert findNthLeapYear(2000, 4) == 2012;
        assert findNthLeapYear(1996, 10) == 2032;
        assert findNthLeapYear(2000, 1) == 2000;
        System.out.println("All tests passed");
    }
}