using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* 
    Determine the minimum number of groups needed to distribute souvenirs where each group can have at most two souvenirs and the total price of souvenirs in a group cannot exceed a specified limit.
    Example:
    >>> MinimumGroups(new List<int> { 15, 20, 10, 30, 40 }, 50)
    3
    >>> MinimumGroups(new List<int> { 5, 20, 25, 10 }, 30)
    2
*/
   
static int MinimumGroups(List<int> souvenirPrices, int maxPriceSum)
{
        souvenirPrices.Sort();
        int left = 0;
        int right = souvenirPrices.Count - 1;
        int groups = 0;
        
        while (left <= right)
        {
            if (souvenirPrices[left] + souvenirPrices[right] <= maxPriceSum)
            {
                left++;
            }
            right--;
            groups++;
        }
        
        return groups;
    }
    static void Main()
    {
        Debug.Assert(MinimumGroups(new List<int> { 15, 20, 10, 30, 40 }, 50) == 3);
        Debug.Assert(MinimumGroups(new List<int> { 5, 20, 25, 10 }, 30) == 2);
        Debug.Assert(MinimumGroups(new List<int> { 10, 20, 30, 40, 50 }, 60) == 3);
        Debug.Assert(MinimumGroups(new List<int> { 25, 30, 35, 5, 15, 20 }, 40) == 4);
        Debug.Assert(MinimumGroups(new List<int> { 32, 42, 52, 12, 22 }, 70) == 3);
        Debug.Assert(MinimumGroups(new List<int> { 30, 35, 40, 10, 15, 20, 25}, 45) == 4);
        Debug.Assert(MinimumGroups(new List<int> { 5, 10, 15, 20, 25 }, 30) == 3);

    }
}
