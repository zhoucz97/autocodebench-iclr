using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


/* Counts the number of ways a group of kids can queue up to buy tickets, ensuring the ticket seller can always give change.
   - totalKids: Total number of kids.
   - oneDollarKids: Number of kids with exactly 1 dollar.
   - twoDollarKids: Number of kids with exactly 2 dollars.
   The ticket seller has no change initially, and the ticket costs 1 dollar.
   Different permutations of kids with the same amount are considered distinct arrangements.
   Example:
   >>> CountQueueArrangements(4, 2, 2)
   8

*/
    static int CountQueueArrangements(int totalKids, int oneDollarKids, int twoDollarKids)
{
        if (oneDollarKids < twoDollarKids)
        {
            return 0;
        }
        
        // The problem reduces to counting the number of valid sequences where at any point,
        // the number of 1-dollar kids is >= the number of 2-dollar kids.
        // This is similar to the Catalan number problem but generalized.
        // The formula is ( (totalKids choose oneDollarKids) * (oneDollarKids + 1) ) / (twoDollarKids + 1)
        // But another way is to use the generalized ballot theorem: ( (n + 1 - k) / (n + 1) ) * (n choose k)
        // Here, n = totalKids, k = twoDollarKids (since oneDollarKids = n - k)
        // So the formula becomes ( ( (n - k) + 1 ) / (n + 1) ) * (n choose k)
        // Which simplifies to ( (n + 1 - k) / (n + 1) ) * (n choose k)
        
        // Compute the binomial coefficient (totalKids choose twoDollarKids)
        long binomial = BinomialCoefficient(totalKids, twoDollarKids);
        
        // Apply the generalized ballot theorem formula
        long numerator = (long)(oneDollarKids + 1) * binomial;
        long denominator = (long)(totalKids + 1);
        
        // Since numerator must be divisible by denominator for integer result
        if (numerator % denominator != 0)
        {
            return 0; // This case shouldn't happen for valid inputs where oneDollarKids >= twoDollarKids
        }
        
        return (int)(numerator / denominator);
    }
    
    private static long BinomialCoefficient(int n, int k)
    {
        if (k < 0 || k > n)
        {
            return 0;
        }
        if (k == 0 || k == n)
        {
            return 1;
        }
        k = Math.Min(k, n - k); // Take advantage of symmetry
        long result = 1;
        for (int i = 1; i <= k; i++)
        {
            result = result * (n - k + i) / i;
        }
        return result;
    }
    static void Main()
    {
        Debug.Assert(CountQueueArrangements(4, 2, 2) == 8);
        Debug.Assert(CountQueueArrangements(5, 3, 2) == 60); // Expected output: True
        Debug.Assert(CountQueueArrangements(6, 4, 2) == 432); // Expected output: True

    }
}
