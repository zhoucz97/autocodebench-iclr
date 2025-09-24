using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Finds all automorphic numbers within a specified limit.
    An automorphic number is a number whose square ends in the number itself.
    Examples:
    >>> FindAutomorphicNumbers(100)
    [0, 1, 5, 6, 25, 76]
    >>> FindAutomorphicNumbers(500)
    [0, 1, 5, 6, 25, 76, 376]
*/

static List<int> FindAutomorphicNumbers(int limit)
{
        List<int> automorphicNumbers = new List<int>();
        
        for (int i = 0; i <= limit; i++)
        {
            long square = (long)i * i; // Use long to avoid overflow for larger numbers
            string numberStr = i.ToString();
            string squareStr = square.ToString();
            
            if (squareStr.EndsWith(numberStr))
            {
                automorphicNumbers.Add(i);
            }
        }
        
        return automorphicNumbers;
    }
    static void Main()
    {
     Debug.Assert(FindAutomorphicNumbers(100).SequenceEqual(new List<int> { 0, 1, 5, 6, 25, 76 }));
    Debug.Assert(FindAutomorphicNumbers(500).SequenceEqual(new List<int> { 0, 1, 5, 6, 25, 76, 376 }));
    Debug.Assert(FindAutomorphicNumbers(1000).SequenceEqual(new List<int> { 0, 1, 5, 6, 25, 76, 376, 625 }));
    Debug.Assert(FindAutomorphicNumbers(200000).SequenceEqual(new List<int> { 0, 1, 5, 6, 25, 76, 376, 625,9376, 90625, 109376 }));

    }
}