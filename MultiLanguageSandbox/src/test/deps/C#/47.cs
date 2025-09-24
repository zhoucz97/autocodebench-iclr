using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines the number of students who have enrolled in both Course A and Course B.
    Given two lists of student IDs for each course, this function counts how many IDs appear in both lists.
    - courseAStudents: List<int> representing student IDs enrolled in Course A.
    - courseBStudents: List<int> representing student IDs enrolled in Course B.
    
    Example usage:
    >>> CountCommonStudents(new List<int> {1, 2, 3, 4, 5}, new List<int> {1, 3, 4, 5, 6})
    4
    >>> CountCommonStudents(new List<int> {7, 8, 9}, new List<int> {10, 11, 12})
    0
*/


static int CountCommonStudents(List<int> courseAStudents, List<int> courseBStudents)
{
        // Convert the second list to a HashSet for O(1) lookups
        HashSet<int> courseBSet = new HashSet<int>(courseBStudents);
        
        int count = 0;
        
        // Iterate through the first list and check if each element exists in the HashSet
        foreach (int id in courseAStudents)
        {
            if (courseBSet.Contains(id))
            {
                count++;
            }
        }
        
        return count;
    }

    // Example usage
    static void Main()
    {
        Debug.Assert(CountCommonStudents(new List<int> { 1, 2, 3, 4, 5 }, new List<int> { 1, 3, 4, 5, 6 }) == 4);
        Debug.Assert(CountCommonStudents(new List<int> { 7, 8, 9 }, new List<int> { 10, 11, 12 }) == 0);
        Debug.Assert(CountCommonStudents(new List<int> { 1, 3, 5, 7, 9 }, new List<int> { 2, 4, 6, 8, 10 }) == 0);
        Debug.Assert(CountCommonStudents(new List<int> { 2, 4, 6, 8 }, new List<int> { 1, 3, 5, 7, 8 }) == 1);


    }
}