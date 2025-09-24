using System;
using System.Collections.Generic;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* 
    This function takes a list of integers and performs two operations. Firstly, it removes any duplicate numbers, ensuring each number is unique. Secondly, it sorts the remaining numbers in ascending order.
    Example usage:
    >>> UniqueAndSorted(new List<int> {4, 2, 2, 5, 1})
    [1, 2, 4, 5]
    >>> UniqueAndSorted(new List<int> {10, 9, 8, 7, 8, 9, 10})
    [7, 8, 9, 10]
*/
   static List<int> UniqueAndSorted(List<int> numbers)
{
        // Remove duplicates by converting to a HashSet (which automatically removes duplicates)
        // Then convert back to a List and sort it
        return new HashSet<int>(numbers).ToList().OrderBy(x => x).ToList();
    }
static void Main()
    {
        Debug.Assert(UniqueAndSorted(new List<int> { 3, 1, 2, 1 }) .SequenceEqual(new List<int> { 1, 2, 3 }));
        Debug.Assert(UniqueAndSorted(new List<int> { 5, 4, 5, 4, 5 }) .SequenceEqual(new List<int> { 4, 5 }));
        Debug.Assert(UniqueAndSorted(new List<int> { 8, 7, 9, 6, 10 }) .SequenceEqual(new List<int> { 6, 7, 8, 9, 10 }));
        Debug.Assert(UniqueAndSorted(new List<int> { 11, 11, 11 }) .SequenceEqual(new List<int> { 11 }));
        Debug.Assert(UniqueAndSorted(new List<int> { 15, 14, 13, 12, 11, 15, 14 }) .SequenceEqual(new List<int> { 11, 12, 13, 14, 15 }));

    }
}