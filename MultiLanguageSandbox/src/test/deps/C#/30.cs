using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Removes a specific suffix ('er', 'ly', or 'ing') from the given word if it ends with one of these suffixes. 
    The function ensures the remaining word is not empty.
    Examples:
        >>> RemoveSuffix("happily")
        "happi"
        >>> RemoveSuffix("dancing")
        "danc"
        >>> RemoveSuffix("flower")
        "flow"
*/
static string RemoveSuffix(string word)
{
        // Define the suffixes to check for
        string[] suffixes = { "er", "ly", "ing" };
        
        foreach (string suffix in suffixes)
        {
            if (word.EndsWith(suffix))
            {
                string result = word.Substring(0, word.Length - suffix.Length);
                if (!string.IsNullOrEmpty(result))
                {
                    return result;
                }
            }
        }
        
        // If no suffix was found or removing it would make the word empty, return the original word
        return word;
    }
    static void Main()
    {
        Debug.Assert(RemoveSuffix("happily") == "happi");
        Debug.Assert(RemoveSuffix("dancing") == "danc");
        Debug.Assert(RemoveSuffix("flower") == "flow");
        Debug.Assert(RemoveSuffix("computer") == "comput");
        Debug.Assert(RemoveSuffix("flying") == "fly");
        Debug.Assert(RemoveSuffix("ing") == "ing");
        Debug.Assert(RemoveSuffix("er") == "er");
    }
}