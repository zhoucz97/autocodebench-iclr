using System;
using System.Collections.Generic;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


/* Sort a list of integers in descending order based on their absolute values.
    Examples:
    >>> SortByAbsoluteDescending(new List<int> { 3, -4, 2 })
    [-4, 3, 2]
    >>> SortByAbsoluteDescending(new List<int> { 0, 1, 2, -3 })
    [-3, 2, 1, 0]
*/
static List<int> SortByAbsoluteDescending(List<int> numbers)
{
        // Sort the list in descending order based on the absolute value of each element
        var sortedList = numbers.OrderByDescending(x => Math.Abs(x)).ToList();
        return sortedList;
    }
    static void Main()
    {
        Debug.Assert(SortByAbsoluteDescending(new List<int> { 3, -4, 2 }).SequenceEqual(new List<int> { -4, 3, 2 }));
        Debug.Assert(SortByAbsoluteDescending(new List<int> { 0, 1, 2, -3 }).SequenceEqual(new List<int> { -3, 2, 1, 0 }));


    }
}