using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
    /* 
    FindAmicableNumbers - Finds all amicable number pairs within a specified limit.
    Amicable numbers are two different numbers so related that the sum of the proper divisors of each is equal to the other number.

    Example usage:
    >>> FindAmicableNumbers(300)
    >>> (220, 284)
    */
static List<(int, int)> FindAmicableNumbers(int limit)
{
        List<(int, int)> amicablePairs = new List<(int, int)>();
        
        for (int a = 1; a < limit; a++)
        {
            int b = SumOfProperDivisors(a);
            
            // Check if b is greater than a to avoid duplicate pairs and within limit
            if (b > a && b < limit && SumOfProperDivisors(b) == a)
            {
                amicablePairs.Add((a, b));
            }
        }
        
        return amicablePairs;
    }
    
    private static int SumOfProperDivisors(int number)
    {
        if (number == 1) return 0;
        
        int sum = 1; // 1 is a proper divisor for any number > 1
        
        // Check divisors up to square root of number
        for (int i = 2; i * i <= number; i++)
        {
            if (number % i == 0)
            {
                if (i * i == number)
                {
                    sum += i; // perfect square, add the square root once
                }
                else
                {
                    sum += i + (number / i); // add both divisors
                }
            }
        }
        
        return sum;
    }
    static void Main()
    {
        Debug.Assert(FindAmicableNumbers(300).SequenceEqual(new List<(int, int)> { (220, 284) }));
        Debug.Assert(FindAmicableNumbers(2000).SequenceEqual(new List<(int, int)> { (220, 284), (1184, 1210) }));
        Debug.Assert(FindAmicableNumbers(500).Count == 1);
        Debug.Assert(FindAmicableNumbers(100).Count == 0);
        Debug.Assert(FindAmicableNumbers(3000).SequenceEqual(new List<(int, int)> { (220, 284), (1184, 1210) ,(2620,2924)}));

    }
}



