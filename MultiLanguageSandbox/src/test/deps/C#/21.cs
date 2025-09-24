using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Determines if a sequence has "Jolly Jumps" where the absolute differences between adjacent elements, 
   when sorted, form a continuous sequence from 1 to (length of sequence - 1).
   A sequence with a single element is always considered jolly.
   
        Examples:
        >>> IsSequenceJolly(new int[] {1, 4, 2, 3})
        "Jolly"
        
        >>> IsSequenceJolly(new int[] {1, 3})
        "Not Jolly"

        >>> IsSequenceJolly(new int[] {5})
        "Jolly"
    */
    static string IsSequenceJolly(int[] sequence)
{
        if (sequence.Length <= 1)
        {
            return "Jolly";
        }

        // Calculate absolute differences between adjacent elements
        List<int> differences = new List<int>();
        for (int i = 1; i < sequence.Length; i++)
        {
            differences.Add(Math.Abs(sequence[i] - sequence[i - 1]));
        }

        // Check if the differences form a continuous sequence from 1 to (n-1)
        int n = sequence.Length;
        HashSet<int> uniqueDifferences = new HashSet<int>(differences);
        
        // The set should contain all numbers from 1 to n-1 exactly once
        for (int i = 1; i < n; i++)
        {
            if (!uniqueDifferences.Contains(i))
            {
                return "Not Jolly";
            }
        }

        return "Jolly";
    }

    // Test cases
    static void Main()
    {

        Debug.Assert(IsSequenceJolly(new int[] {1, 4, 2, 3}) == "Jolly");
        Debug.Assert(IsSequenceJolly(new int[] {1, 3}) == "Not Jolly");
        Debug.Assert(IsSequenceJolly(new int[] {5}) == "Jolly");
        Debug.Assert(IsSequenceJolly(new int[] {10, 7, 8, 9}) == "Not Jolly");

    }
}