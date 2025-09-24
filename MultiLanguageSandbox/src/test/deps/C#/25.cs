using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Finds the first character in a string that appears only once.
   If all characters appear more than once or the string is empty, it returns 'no' as a character.
   Examples:
   - FirstUniqueChar("aabbcdde") should return 'c'.
   - FirstUniqueChar("aabbcc") should return 'n', representing "no".
   - FirstUniqueChar("xxyyzz") should return 'n'.
   - FirstUniqueChar("") should return 'n'.
*/
    static char FirstUniqueChar(string str)
{
        if (string.IsNullOrEmpty(str))
        {
            return 'n';
        }

        Dictionary<char, int> charCount = new Dictionary<char, int>();

        // Count the occurrences of each character
        foreach (char c in str)
        {
            if (charCount.ContainsKey(c))
            {
                charCount[c]++;
            }
            else
            {
                charCount[c] = 1;
            }
        }

        // Find the first character with count 1
        foreach (char c in str)
        {
            if (charCount[c] == 1)
            {
                return c;
            }
        }

        // If no unique character found
        return 'n';
    }
    static void Main()
    {
        Debug.Assert(FirstUniqueChar("aabbcdde") == 'c');
        Debug.Assert(FirstUniqueChar("aabbcc") == 'n');
        Debug.Assert(FirstUniqueChar("xxyyazz") == 'a');
        Debug.Assert(FirstUniqueChar("") == 'n');
        Debug.Assert(FirstUniqueChar("aabbcceeddf") == 'f');

    }
}