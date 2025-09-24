using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
   /*
    Calculates the maximum total value of herbs that can be collected within a given time.
    The function takes the total time available for collecting herbs, the number of different herbs,
    an array of time costs for each herb, and an array of values for each herb.

    Parameters:
    totalMinutes - the total time available for collecting herbs.
    herbCount - the number of different herbs available.
    timeCosts - an array where timeCosts[i] represents the time required to collect the i-th herb.
    values - an array where values[i] represents the value of the i-th herb.

    Returns:
    The maximum total value of the herbs that can be collected within the given time.

    Examples:
    >>> MaxHerbValue(10, 3, new int[] {3, 4, 5}, new int[] {200, 300, 350})
    550
    >>> MaxHerbValue(5, 2, new int[] {1, 4}, new int[] {150, 300})
    450
*/
   static int MaxHerbValue(int totalMinutes, int herbCount, int[] timeCosts, int[] values)
{
        // Create a DP table initialized to 0
        int[,] dp = new int[herbCount + 1, totalMinutes + 1];
        
        for (int i = 1; i <= herbCount; i++)
        {
            for (int j = 0; j <= totalMinutes; j++)
            {
                if (timeCosts[i - 1] <= j)
                {
                    // Option 1: Include the current herb
                    int includeValue = values[i - 1] + dp[i - 1, j - timeCosts[i - 1]];
                    // Option 2: Exclude the current herb
                    int excludeValue = dp[i - 1, j];
                    // Choose the maximum of the two options
                    dp[i, j] = Math.Max(includeValue, excludeValue);
                }
                else
                {
                    // Cannot include the current herb, carry forward the previous value
                    dp[i, j] = dp[i - 1, j];
                }
            }
        }
        
        return dp[herbCount, totalMinutes];
    }
static void Main()
    {
        Debug.Assert(MaxHerbValue(10, 3, new int[] {3, 4, 5}, new int[] {200, 300, 350}) == 650);
        Debug.Assert(MaxHerbValue(5, 2, new int[] {1, 4}, new int[] {150, 300}) == 450);
        Debug.Assert(MaxHerbValue(7, 3, new int[] {2, 3, 5}, new int[] {100, 200, 300}) == 400);
        Debug.Assert(MaxHerbValue(12, 4, new int[] {2, 4, 6, 3}, new int[] {100, 200, 300, 150}) == 600);
        Debug.Assert(MaxHerbValue(8, 3, new int[] {1, 3, 4}, new int[] {150, 250, 350}) == 750);
        Debug.Assert(MaxHerbValue(15, 5, new int[] {3, 5, 7, 4, 2}, new int[] {120, 280, 350, 220, 180}) == 810);


    }
}