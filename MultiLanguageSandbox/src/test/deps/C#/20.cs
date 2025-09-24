using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Calculates the number of unique postage values that can be created using a given number of 3-cent and 5-cent stamps.
        Parameters:
        threeCentStamps - the number of 3-cent stamps available
        fiveCentStamps - the number of 5-cent stamps available

        Returns:
        The number of unique postage values that can be created.

        Example:
        >>> CountUniquePostageCombinations(4, 3)
        19

        >>> CountUniquePostageCombinations(2, 2)
        8
    */
    static int CountUniquePostageCombinations(int threeCentStamps, int fiveCentStamps)
{
        HashSet<int> uniqueSums = new HashSet<int>();
        
        for (int i = 0; i <= threeCentStamps; i++)
        {
            for (int j = 0; j <= fiveCentStamps; j++)
            {
                int sum = 3 * i + 5 * j;
                uniqueSums.Add(sum);
            }
        }
        
        return uniqueSums.Count;
    }
    static void Main()
    {
        Debug.Assert(CountUniquePostageCombinations(4, 3) == 19); 
        Debug.Assert(CountUniquePostageCombinations(2, 2) == 8); 
        Debug.Assert(CountUniquePostageCombinations(3, 1) == 7);
        Debug.Assert(CountUniquePostageCombinations(1, 3) == 7); 

    }
}
