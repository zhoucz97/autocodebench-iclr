using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
    /* Determines which of the two provided strings is a substring of the other. 
    If neither string is a substring of the other, it returns "No substring".
    >>> CheckSubstring("hello", "world")
    "No substring"
    >>> CheckSubstring("code", "decode")
    "code is substring of decode"
    >>> CheckSubstring("book", "notebook")
    "book is substring of notebook"
    */

    static string CheckSubstring(string str1, string str2)
{
        if (str1.Contains(str2))
        {
            return $"{str2} is substring of {str1}";
        }
        else if (str2.Contains(str1))
        {
            return $"{str1} is substring of {str2}";
        }
        else
        {
            return "No substring";
        }
    }
    static void Main()
    {
        Debug.Assert(CheckSubstring("apple", "pineapple") == "\"apple\" is substring of \"pineapple\"");
        Debug.Assert(CheckSubstring("star", "astrophysics") == "No substring");
        Debug.Assert(CheckSubstring("sun", "sunset") == "\"sun\" is substring of \"sunset\"");
        Debug.Assert(CheckSubstring("moon", "moonlight") == "\"moon\" is substring of \"moonlight\"");


    }
}