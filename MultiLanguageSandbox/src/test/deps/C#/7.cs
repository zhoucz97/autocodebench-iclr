using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Copies a substring from the specified start index in the input string to the end of the string.
   If the start index is greater than the string's length, returns an empty string.
   Examples:
   >>> CopySubstringFromIndex("Hello World", 6)
   "World"
   >>> CopySubstringFromIndex("Example", 3)
   "mple"
   >>> CopySubstringFromIndex("Short", 10)
   ""
*/   
   static string CopySubstringFromIndex(string input, int startIndex)
{
        if (startIndex >= input.Length)
        {
            return "";
        }
        return input.Substring(startIndex);
    }
static void Main()
    {
        Debug.Assert(CopySubstringFromIndex("Hello World", 6) == "World");
        Debug.Assert(CopySubstringFromIndex("Example", 3) == "mple");
        Debug.Assert(CopySubstringFromIndex("Short", 10) == "");
        Debug.Assert(CopySubstringFromIndex("AnotherExample", 0) == "AnotherExample");
        Debug.Assert(CopySubstringFromIndex("Test", 4) == "");
        Debug.Assert(CopySubstringFromIndex("", 0) == "");
        Debug.Assert(CopySubstringFromIndex("LastOne", 7) == "");

    }
}