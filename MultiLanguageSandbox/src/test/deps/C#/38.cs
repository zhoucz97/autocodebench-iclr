using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines the minimum absolute difference between elements of two sorted arrays.
   Each array is assumed to be sorted in ascending order.
   Examples:
   >>> MinArrayDistance(new[] {1, 3, 5}, new[] {2, 4, 6})
   1
   >>> MinArrayDistance(new[] {10, 20, 30}, new[] {15, 25, 35})
   5
*/
static int MinArrayDistance(int[] array1, int[] array2)
{
        int i = 0;
        int j = 0;
        int minDiff = int.MaxValue;
        
        while (i < array1.Length && j < array2.Length)
        {
            int diff = Math.Abs(array1[i] - array2[j]);
            if (diff < minDiff)
            {
                minDiff = diff;
            }
            
            if (array1[i] < array2[j])
            {
                i++;
            }
            else
            {
                j++;
            }
        }
        
        return minDiff;
    }

    // Example usage
    static void Main()
    {

        Debug.Assert(MinArrayDistance(new[] {1, 3, 5}, new[] {2, 4, 6}) == 1);
        Debug.Assert(MinArrayDistance(new[] {10, 20, 30}, new[] {15, 25, 35}) == 5);

    }
}