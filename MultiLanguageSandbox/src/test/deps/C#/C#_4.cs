using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


    /* Counts the number of digit, lowercase, and uppercase characters in a given string of length 8.
        >>> CountCharacters("1n2s0e1s")
        (4, 4, 0)
    */

    static (int, int, int) CountCharacters(string s)
{
        int digitCount = 0;
        int lowercaseCount = 0;
        int uppercaseCount = 0;

        // Check if the string length is 8
        if (s.Length != 8)
        {
            throw new ArgumentException("Input string must be of length 8.");
        }

        // Iterate through each character in the string
        foreach (char c in s)
        {
            // Count digits
            if (char.IsDigit(c))
            {
                digitCount++;
            }
            // Count lowercase letters
            else if (char.IsLower(c))
            {
                lowercaseCount++;
            }
            // Count uppercase letters
            else if (char.IsUpper(c))
            {
                uppercaseCount++;
            }
        }

        return (digitCount, lowercaseCount, uppercaseCount);
    }
    static void Main()
    {
        Debug.Assert(CountCharacters("yLAX2022") == (4, 1, 3));
        Debug.Assert(CountCharacters("MBKKOKOK") == (0, 0, 8));
        Debug.Assert(CountCharacters("1n2s0e1s") == (4, 4, 0));
        Debug.Assert(CountCharacters("1234ABCD") == (4, 0, 4));


    }
}