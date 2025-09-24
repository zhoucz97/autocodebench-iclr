
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
   /* Calculates the total number of cows after a given number of years, following the rule that each cow gives birth to another cow every year from its fourth year.
    Example cases:
    >>> TotalCows(1)
    1
    >>> TotalCows(4)
    2
    >>> TotalCows(5)
    3
    >>> TotalCows(6)
    4
*/
   static int TotalCows(int years)
{
    if (years == 0)
        return 0;
    
    int[] dp = new int[years + 1];
    dp[1] = 1;
    if (years >= 2)
        dp[2] = 1;
    if (years >= 3)
        dp[3] = 1;
    
    for (int i = 4; i <= years; i++)
    {
        dp[i] = dp[i - 1] + dp[i - 3];
    }
    
    return dp[years];
}
    static void Main()
    {
        Debug.Assert(TotalCows(1) == 1);
        Debug.Assert(TotalCows(2) == 1);
        Debug.Assert(TotalCows(3) == 1);
        Debug.Assert(TotalCows(4) == 2);
        Debug.Assert(TotalCows(5) == 3);
        Debug.Assert(TotalCows(6) == 4);
        Debug.Assert(TotalCows(7) == 6);
        Debug.Assert(TotalCows(8) == 9);
        Debug.Assert(TotalCows(10) == 19);
    }
}
