using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Counts the number of characters in a given title string, excluding spaces and newline characters.
    >>> CountTitleCharacters("234")
    3
    >>> CountTitleCharacters("Ca 45")
    4
    >>> CountTitleCharacters("Hello World\n")
    10
    >>> CountTitleCharacters(" 123 ")
    3
*/

static int CountTitleCharacters(string title)
{
        int count = 0;
        foreach (char c in title)
        {
            if (c != ' ' && c != '\n')
            {
                count++;
            }
        }
        return count;
    }
    static void Main()
    {
        Debug.Assert(CountTitleCharacters("234") == 3);
        Debug.Assert(CountTitleCharacters("Ca 45") == 4);
        Debug.Assert(CountTitleCharacters("Hello World\n") == 10);
        Debug.Assert(CountTitleCharacters(" 123 ") == 3);
        Debug.Assert(CountTitleCharacters("\nNew\tLine") == 7);

    }
}