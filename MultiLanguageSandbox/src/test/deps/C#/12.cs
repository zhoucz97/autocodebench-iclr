using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* This function counts how many apples are reachable.
    It takes a list of integers representing the heights of apples from the ground and an integer representing the maximum height that can be reached (with or without a stool).
    An apple is considered reachable if its height is less than or equal to the maximum reachable height.
    Example:
    >>> CountReachableApples(new List<int> {150, 200, 160, 310, 220}, 210)
    3
    >>> CountReachableApples(new List<int> {100, 180, 250, 300, 320}, 200)
    2
*/
static int CountReachableApples(List<int> appleHeights, int maxReachHeight)
{
        int count = 0;
        foreach (int height in appleHeights)
        {
            if (height <= maxReachHeight)
            {
                count++;
            }
        }
        return count;
    }

    // Example usage
    static void Main()
    {
        Debug.Assert(CountReachableApples(new List<int> {150, 190, 300, 210, 220}, 200) == 2);
        Debug.Assert(CountReachableApples(new List<int> {120, 180, 260, 310, 150}, 250) == 3);
        Debug.Assert(CountReachableApples(new List<int> {100, 200, 300, 400, 500}, 350) == 3);

    }
}