using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
   /*
    Encodes the given string by shifting each letter by a specified number of places in the alphabet.
    If the shift takes the letter beyond 'z', it wraps around to the beginning of the alphabet.
    
    Examples:
    >>> EncodeString("apple", 2)
    "crrng"
    
    >>> EncodeString("hello", 5)
    "mjqqt"
*/
   static string EncodeString(string input, int shift)
{
        if (string.IsNullOrEmpty(input))
            return input;
            
        // Normalize the shift to handle large values and negative shifts
        shift = shift % 26;
        if (shift < 0)
            shift += 26; // Ensure shift is positive
        
        char[] result = new char[input.Length];
        
        for (int i = 0; i < input.Length; i++)
        {
            char c = input[i];
            if (char.IsLetter(c))
            {
                char baseChar = char.IsUpper(c) ? 'A' : 'a';
                int originalPosition = c - baseChar;
                int newPosition = (originalPosition + shift) % 26;
                result[i] = (char)(baseChar + newPosition);
            }
            else
            {
                // Leave non-letter characters unchanged
                result[i] = c;
            }
        }
        
        return new string(result);
    }
    static void Main()
    {
        Debug.Assert(EncodeString("China", 4) == "Glmre");
        Debug.Assert(EncodeString("apple", 2) == "crrng");
        Debug.Assert(EncodeString("hello", 5) == "mjqqt");
        Debug.Assert(EncodeString("xyz", 3) == "abc");
        Debug.Assert(EncodeString("AbCdE", 1) == "BcDeF");
        Debug.Assert(EncodeString("Java", 10) == "Tkfk");
        Debug.Assert(EncodeString("OpenAI", 4) == "StirEM");
        Debug.Assert(EncodeString("", 5) == "");

    }
}
