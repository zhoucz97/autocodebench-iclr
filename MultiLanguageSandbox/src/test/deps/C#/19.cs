using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* This function takes a string, reverses it, and then concatenates it with the original string.
    Example usage:
    >>> ReverseAndConcatenate("hello")
    "helloolleh"
    >>> ReverseAndConcatenate("123")
    "123321"
*/
static string ReverseAndConcatenate(string input)
{
        // Reverse the input string
        char[] charArray = input.ToCharArray();
        Array.Reverse(charArray);
        string reversed = new string(charArray);
        
        // Concatenate the original and reversed strings
        return input + reversed;
    }
    static void Main()
    {
        Debug.Assert(ReverseAndConcatenate("abc") == "abccba");
        Debug.Assert(ReverseAndConcatenate("test") == "testtset");
        Debug.Assert(ReverseAndConcatenate("") == "");
        Debug.Assert(ReverseAndConcatenate("12345") == "1234554321");

    }
}