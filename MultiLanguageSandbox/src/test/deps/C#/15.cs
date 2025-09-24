using System;
using System.Collections.Generic;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* This function awards the top 5 students based on their total scores. Students are sorted first by their total score (Chinese, Mathematics, English),
   then by their Chinese score, and finally by their student ID if there is a tie. The function returns a list of the top 5 students with their IDs and total scores.
   Example:
   >>> AwardTopStudents([(1, 80, 90, 85), (2, 85, 80, 90), (3, 90, 85, 80)])
   [(3, 255), (2, 255), (1, 255)]
   >>> AwardTopStudents([(4, 88, 75, 95), (5, 95, 88, 75)])
   [(4, 258), (5, 258)]
*/
   static List<(int, int)> AwardTopStudents(List<(int, int, int, int)> students)
{
        // Calculate total score for each student and add it to the tuple
        var studentsWithTotal = students.Select(s => (s.Item1, s.Item2, s.Item3, s.Item4, TotalScore: s.Item2 + s.Item3 + s.Item4));
        
        // Sort the students by:
        // 1. Total score descending
        // 2. Chinese score descending
        // 3. Student ID ascending (to break ties consistently)
        var sortedStudents = studentsWithTotal
            .OrderByDescending(s => s.TotalScore)
            .ThenByDescending(s => s.Item2) // Chinese score
            .ThenBy(s => s.Item1) // Student ID
            .Take(5) // Take top 5
            .Select(s => (s.Item1, s.TotalScore)) // Select only ID and total score
            .ToList();
        
        return sortedStudents;
    };
        var result1 = AwardTopStudents(test1);
        Console.WriteLine("Test 1:");
        foreach (var item in result1)
        {
            Console.WriteLine($"({item.Item1}, {item.Item2})");
        }

        var test2 = new List<(int, int, int, int)>
        {
            (4, 88, 75, 95),
            (5, 95, 88, 75)
        };
        var result2 = AwardTopStudents(test2);
        Console.WriteLine("\nTest 2:");
        foreach (var item in result2)
        {
            Console.WriteLine($"({item.Item1}, {item.Item2})");
        }
    static void Main()
    {
        // Test Case 1
        Debug.Assert(AwardTopStudents(new List<(int, int, int, int)> { (1, 80, 90, 85), (2, 85, 80, 90), (3, 90, 85, 80) }).SequenceEqual(new List<(int, int)> { (3, 255), (2, 255), (1, 255) }));

        // Test Case 2
        Debug.Assert(AwardTopStudents(new List<(int, int, int, int)> { (4, 88, 75, 95), (5, 95, 88, 75) }).SequenceEqual(new List<(int, int)> { (5, 258), (4, 258) }));

        // Test Case 3
        Debug.Assert(AwardTopStudents(new List<(int, int, int, int)> { (6, 75, 85, 90), (7, 90, 75, 85) }).SequenceEqual(new List<(int, int)> { (7, 250), (6, 250) }));

        // Test Case 4
        Debug.Assert(AwardTopStudents(new List<(int, int, int, int)> { (8, 80, 90, 80), (9, 90, 80, 80) }).SequenceEqual(new List<(int, int)> { (9, 250), (8, 250) }));

        // Test Case 5
        Debug.Assert(AwardTopStudents(new List<(int, int, int, int)> { (10, 85, 85, 85), (11, 86, 85, 84), (12, 84, 86, 85) }).SequenceEqual(new List<(int, int)> { (11, 255), (10, 255), (12, 255) }));
        }
}
