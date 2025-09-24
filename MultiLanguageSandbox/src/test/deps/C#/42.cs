using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines the number of pairs of students who are considered "equal opponents". Two students are equal opponents if the difference in each of their subjects' scores (Chinese, Mathematics, English) does not exceed 5, and the total difference in their scores does not exceed 10.
    >>> FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(90, 90, 90), Tuple.Create(85, 95, 90), Tuple.Create(80, 100, 91) })
    2

*/


static int FindEqualOpponents(List<Tuple<int, int, int>> scores)
{
        int count = 0;
        int n = scores.Count;
        
        for (int i = 0; i < n; i++)
        {
            for (int j = i + 1; j < n; j++)
            {
                var student1 = scores[i];
                var student2 = scores[j];
                
                int chineseDiff = Math.Abs(student1.Item1 - student2.Item1);
                int mathDiff = Math.Abs(student1.Item2 - student2.Item2);
                int englishDiff = Math.Abs(student1.Item3 - student2.Item3);
                
                bool individualDiffsValid = (chineseDiff <= 5) && (mathDiff <= 5) && (englishDiff <= 5);
                int totalDiff = chineseDiff + mathDiff + englishDiff;
                bool totalDiffValid = (totalDiff <= 10);
                
                if (individualDiffsValid && totalDiffValid)
                {
                    count++;
                }
            }
        }
        
        return count;
    };
        
        Console.WriteLine(FindEqualOpponents(scores)); // Output should be 2
    static void Main()
    {
        Debug.Assert(FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(90, 90, 90), Tuple.Create(85, 95, 90), Tuple.Create(80, 100, 91) }) == 2);
        Debug.Assert(FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(100, 100, 100), Tuple.Create(92, 95, 98), Tuple.Create(85, 90, 95) }) == 0);
        Debug.Assert(FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(100, 95, 90), Tuple.Create(95, 90, 85), Tuple.Create(90, 85, 80) }) == 0);
        Debug.Assert(FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(100, 100, 100), Tuple.Create(80, 80, 80) }) == 0);
        Debug.Assert(FindEqualOpponents(new List<Tuple<int, int, int>> { Tuple.Create(100, 100, 100), Tuple.Create(100, 100, 99), Tuple.Create(101, 100, 99) }) == 3);

    }
}
