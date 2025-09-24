using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Finds M positive integers that sum up to N and have the maximum possible product. If multiple solutions exist, 
   returns the one with the lexicographically smallest sequence.
    >>> MaxProductPartition(6, 3)
    [2, 2, 2]

*/


    public static List<int> MaxProductPartition(int N, int M)
{
        List<int> result = new List<int>();
        int baseValue = N / M;
        int remainder = N % M;
        
        for (int i = 0; i < M; i++) {
            if (i < remainder) {
                result.Add(baseValue + 1);
            } else {
                result.Add(baseValue);
            }
        }
        
        // To ensure lexicographical order, we need the smallest numbers first.
        // The above approach already does this because we add the larger numbers first (if any), but then the loop starts from 0 to M-1,
        // so the larger numbers (baseValue +1) are added first if remainder >0. But we need them last for lex smallest.
        // So we need to reverse the logic: the first 'remainder' elements are baseValue+1, the rest are baseValue.
        // But to make it lex smallest, we should have the smallest numbers first. So the correct approach is to have the baseValue numbers first, followed by baseValue+1.
        // So the initial approach is incorrect for lex order. Let's fix it.
        
        // Correct approach: the lex smallest sequence has as many baseValue as possible first, then baseValue+1.
        result.Clear();
        for (int i = 0; i < M; i++) {
            if (i >= M - remainder) {
                result.Add(baseValue + 1);
            } else {
                result.Add(baseValue);
            }
        }
        
        return result;
    }
    static void Main()
    {

        Debug.Assert(MaxProductPartition(6, 3).SequenceEqual(new List<int> { 2, 2, 2 }));
        Debug.Assert(MaxProductPartition(8, 3).SequenceEqual(new List<int> { 2, 3, 3 }));
        Debug.Assert(MaxProductPartition(10, 2).SequenceEqual(new List<int> { 5, 5 }));
        Debug.Assert(MaxProductPartition(7, 3).SequenceEqual(new List<int> { 2, 2, 3 }));

    }
}
