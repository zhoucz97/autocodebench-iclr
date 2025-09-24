using System;
using System.Text.RegularExpressions;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines if a given password is secure based on specific criteria.
   A secure password must:
   1. Be at least 8 characters long and no more than 16 characters.
   2. Include characters from at least three of the following categories:
      - Uppercase letters (A-Z)
      - Lowercase letters (a-z)
      - Numbers (0-9)
      - Special symbols (~, !, @, #, $, %, ^)

   Examples:
   >>> IsPasswordSecure("Password123!")
   "YES"
   >>> IsPasswordSecure("short")
   "NO"
   >>> IsPasswordSecure("LongPasswordWithoutNumbersOrSymbols")
   "NO"
*/
static string IsPasswordSecure(string password)
{
        // Check length criteria (8-16 characters)
        if (password.Length < 8 || password.Length > 16)
        {
            return "NO";
        }

        // Initialize counters for each category
        bool hasUppercase = false;
        bool hasLowercase = false;
        bool hasNumber = false;
        bool hasSymbol = false;

        // Define special symbols
        string specialSymbols = "~@#$%^";

        foreach (char c in password)
        {
            if (char.IsUpper(c))
            {
                hasUppercase = true;
            }
            else if (char.IsLower(c))
            {
                hasLowercase = true;
            }
            else if (char.IsDigit(c))
            {
                hasNumber = true;
            }
            else if (specialSymbols.Contains(c))
            {
                hasSymbol = true;
            }
        }

        // Count how many categories are present
        int categoryCount = 0;
        if (hasUppercase) categoryCount++;
        if (hasLowercase) categoryCount++;
        if (hasNumber) categoryCount++;
        if (hasSymbol) categoryCount++;

        // Password must have at least 3 categories
        if (categoryCount >= 3)
        {
            return "YES";
        }
        else
        {
            return "NO";
        }
    }
    static void Main()
    {

        Debug.Assert(IsPasswordSecure("Password123!") == "YES");
        Debug.Assert(IsPasswordSecure("short") == "NO");
        Debug.Assert(IsPasswordSecure("LongPasswordWithoutNumbersOrSymbols") == "NO");
        Debug.Assert(IsPasswordSecure("Good@Pass1") == "YES");
        Debug.Assert(IsPasswordSecure("1234567890") == "NO");

    }
}