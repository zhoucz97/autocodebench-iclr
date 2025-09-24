using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Finds the length of the longest consecutive elements sequence in a given list of integers.
    The sequence does not need to be in order but must consist of consecutive natural numbers.
    >>> LongestConsecutiveSequence(new List<int> {1, 5, 6, 2, 3, 4, 5, 6, 8, 9})
    6
    >>> LongestConsecutiveSequence(new List<int> {10, 4, 20, 1, 3, 2})
    4
*/

static int LongestConsecutiveSequence(List<int> sequence)
{
        if (sequence == null || sequence.Count == 0)
        {
            return 0;
        }

        HashSet<int> numSet = new HashSet<int>(sequence);
        int maxLength = 0;

        foreach (int num in numSet)
        {
            // Check if it's the start of a sequence
            if (!numSet.Contains(num - 1))
            {
                int currentNum = num;
                int currentLength = 1;

                while (numSet.Contains(currentNum + 1))
                {
                    currentNum++;
                    currentLength++;
                }

                maxLength = Math.Max(maxLength, currentLength);
            }
        }

        return maxLength;
    }
    static void Main()
    {
        Debug.Assert(LongestConsecutiveSequence(new List<int> {1, 5, 6, 2, 3, 4, 5, 6, 8, 9}) == 6);
        Debug.Assert(LongestConsecutiveSequence(new List<int> {10, 4, 20, 1, 3, 2}) == 4);
        Debug.Assert(LongestConsecutiveSequence(new List<int> {100, 4, 200, 1, 3, 2}) == 4);
        Debug.Assert(LongestConsecutiveSequence(new List<int> {1, 1}) == 1);

    }
}

