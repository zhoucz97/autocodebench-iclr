using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/*
    Decrypts a cipher text that was encrypted using a Caesar cipher with a 5-letter shift.
    The function only shifts uppercase alphabetic characters and leaves other characters unchanged.
    Example:
    >>> DecryptCaesarCipher("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
    "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"

*/
    static string DecryptCaesarCipher(string cipherText)
{
        char[] result = new char[cipherText.Length];
        
        for (int i = 0; i < cipherText.Length; i++)
        {
            char c = cipherText[i];
            
            if (char.IsUpper(c))
            {
                // Shift back by 5 positions, wrapping around if necessary
                c = (char)(((c - 'A' - 5 + 26) % 26) + 'A');
            }
            
            result[i] = c;
        }
        
        return new string(result);
    }
static void Main()
    {
        Debug.Assert(DecryptCaesarCipher("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX") == "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES");
        Debug.Assert(DecryptCaesarCipher("F XYWNFYNTS YT XJHWJY") == "A STRIATION TO SECRET");
        Debug.Assert(DecryptCaesarCipher("YMJ VZNHP TS X HNUMJW") == "THE QUICK ON S CIPHER");
        Debug.Assert(DecryptCaesarCipher("JXU UQFSI QDT TZW YMZXW") == "ESP PLAND LYO OUR THUSR");
    }
}