using System;
using System.Collections.Generic;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/*
    Finds the missing (broken) and duplicated ID from a series of ID sequences.
    IDs are supposed to be continuous but due to an error, one ID is missing and one is duplicated.
    The function returns a tuple with the first element being the missing ID and the second the duplicated ID.

    Args:
    ids: A list of lists, each sublist contains a series of ticket IDs.

    Returns:
    A tuple of two integers: the first is the missing ID (m) and the second is the duplicated ID (n).

    Examples:
    >>> FindErrorIds(new List<List<int>> { new List<int> {5, 6, 8, 11, 9}, new List<int> {10, 12, 9} })
    (7, 9)
    >>> FindErrorIds(new List<List<int>> { new List<int> {1, 2, 4}, new List<int> {3, 3} })
    (5, 3)
*/

    static (int, int) FindErrorIds(List<List<int>> ids)
{
        // Flatten all the lists into one list
        var flattened = ids.SelectMany(list => list).ToList();
        
        if (flattened.Count == 0)
        {
            return (0, 0); // though the problem likely expects non-empty input
        }
        
        int min = flattened.Min();
        int max = flattened.Max();
        
        // Create a frequency dictionary
        Dictionary<int, int> frequency = new Dictionary<int, int>();
        
        foreach (int id in flattened)
        {
            if (frequency.ContainsKey(id))
            {
                frequency[id]++;
            }
            else
            {
                frequency[id] = 1;
            }
        }
        
        int missing = 0;
        int duplicated = 0;
        
        for (int i = min; i <= max; i++)
        {
            if (!frequency.ContainsKey(i))
            {
                missing = i;
            }
            else if (frequency[i] > 1)
            {
                duplicated = i;
            }
        }
        
        return (missing, duplicated);
    }
static void Main()
    {

        Debug.Assert(FindErrorIds(new List<List<int>> { new List<int> {5, 6, 8, 11, 9}, new List<int> {10, 12, 9} }) == (7, 9));
        Debug.Assert(FindErrorIds(new List<List<int>> { new List<int> {1, 2, 4, 6}, new List<int> {3, 3} }) == (5, 3));
        Debug.Assert(FindErrorIds(new List<List<int>> { new List<int> { 110, 112, 108 }, new List<int> { 107, 109, 108 } }) == (111, 108));

    }
}