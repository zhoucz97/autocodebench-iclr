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
        if (s.Length != 8)
        {
            throw new ArgumentException("Input string must be of length 8");
        }

        int digitCount = 0;
        int lowerCount = 0;
        int upperCount = 0;

        foreach (char c in s)
        {
            if (char.IsDigit(c))
            {
                digitCount++;
            }
            else if (char.IsLower(c))
            {
                lowerCount++;
            }
            else if (char.IsUpper(c))
            {
                upperCount++;
            }
        }

        return (digitCount, lowerCount, upperCount);
    }
    static void Main()
    {
        Debug.Assert(CountCharacters("yLAX2022") == (4, 1, 3));
        Debug.Assert(CountCharacters("MBKKOKOK") == (0, 0, 8));
        Debug.Assert(CountCharacters("1n2s0e1s") == (4, 4, 0));
        Debug.Assert(CountCharacters("1234ABCD") == (4, 0, 4));


    }
}