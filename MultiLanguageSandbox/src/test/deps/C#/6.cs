using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Sorts three given integers in ascending order and returns the sorted list.
    Example usage:
    >>> SortIntegersAscending(3, 1, 2)
    [1, 2, 3]
    >>> SortIntegersAscending(5, 7, 2)
    [2, 5, 7]
    >>> SortIntegersAscending(-1, 0, -3)
    [-3, -1, 0]
*/
static List<int> SortIntegersAscending(int num1, int num2, int num3)
{
        // Create a list with the three numbers
        List<int> numbers = new List<int> { num1, num2, num3 };
        
        // Sort the list in ascending order
        numbers.Sort();
        
        // Return the sorted list
        return numbers;
    }

    // Example usage
    static void Main()
    {
        Debug.Assert(SortIntegersAscending(3, 1, 2).SequenceEqual(new List<int> { 1, 2, 3 }));
        Debug.Assert(SortIntegersAscending(5, 7, 2).SequenceEqual(new List<int> { 2, 5, 7 }));
        Debug.Assert(SortIntegersAscending(-1, 0, -3).SequenceEqual(new List<int> { -3, -1, 0 }));
        Debug.Assert(SortIntegersAscending(10, 10, 10).SequenceEqual(new List<int> { 10, 10, 10 }));
        Debug.Assert(SortIntegersAscending(0, -1, 1).SequenceEqual(new List<int> { -1, 0, 1 }));
        Debug.Assert(SortIntegersAscending(100, 50, 75).SequenceEqual(new List<int> { 50, 75, 100 }));
        Debug.Assert(SortIntegersAscending(-5, -10, -7).SequenceEqual(new List<int> { -10, -7, -5 }));


    }
}