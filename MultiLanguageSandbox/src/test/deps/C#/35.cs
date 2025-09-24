using System;
using System.Text;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
class Program
{

/* Determines the minimum number of steps required to convert a number into a palindrome in a given base (from 2 to 16). 
   The function takes an integer representing the base and a string representing the number in that base. 
   A palindrome is a number that reads the same forwards and backwards in the same base.
   It calculates the number of steps required to reach a palindrome by repeatedly adding the number to its reverse in the given base.
   If a palindrome is not obtained within 30 steps, it returns "Impossible!".

   Example cases:
   >>> StepsToPalindrome(10, "56")  // In base 10, "56" becomes palindrome "121" in 1 step
   "STEP=1"
   >>> StepsToPalindrome(9, "87")   // In base 9, "87" becomes palindrome "4884" in 6 steps
   "STEP=6"
   >>> StepsToPalindrome(2, "1001") 
   "STEP=0"

*/
    public static string StepsToPalindrome(int baseNum, string num)
{
        if (baseNum < 2 || baseNum > 16)
        {
            throw new ArgumentException("Base must be between 2 and 16");
        }

        int steps = 0;
        string current = num;

        while (steps < 30)
        {
            if (IsPalindrome(current, baseNum))
            {
                return $"STEP={steps}";
            }

            string reversed = ReverseNumber(current, baseNum);
            string sum = AddNumbers(current, reversed, baseNum);
            current = sum;
            steps++;
        }

        return "Impossible!";
    }

    private static bool IsPalindrome(string num, int baseNum)
    {
        int left = 0;
        int right = num.Length - 1;

        while (left < right)
        {
            if (CharToInt(num[left], baseNum) != CharToInt(num[right], baseNum))
            {
                return false;
            }
            left++;
            right--;
        }
        return true;
    }

    private static string ReverseNumber(string num, int baseNum)
    {
        char[] chars = num.ToCharArray();
        Array.Reverse(chars);
        return new string(chars);
    }

    private static string AddNumbers(string num1, string num2, int baseNum)
    {
        StringBuilder result = new StringBuilder();
        int carry = 0;
        int i = num1.Length - 1;
        int j = num2.Length - 1;

        while (i >= 0 || j >= 0 || carry > 0)
        {
            int digit1 = (i >= 0) ? CharToInt(num1[i--], baseNum) : 0;
            int digit2 = (j >= 0) ? CharToInt(num2[j--], baseNum) : 0;

            int sum = digit1 + digit2 + carry;
            carry = sum / baseNum;
            int remainder = sum % baseNum;

            result.Insert(0, IntToChar(remainder, baseNum));
        }

        return result.ToString();
    }

    private static int CharToInt(char c, int baseNum)
    {
        if (char.IsDigit(c))
        {
            return c - '0';
        }
        else if (char.IsLetter(c))
        {
            return char.ToUpper(c) - 'A' + 10;
        }
        throw new ArgumentException($"Invalid character '{c}' for base {baseNum}");
    }

    private static char IntToChar(int value, int baseNum)
    {
        if (value < 0 || value >= baseNum)
        {
            throw new ArgumentException($"Value {value} is out of range for base {baseNum}");
        }

        if (value < 10)
        {
            return (char)('0' + value);
        }
        else
        {
            return (char)('A' + value - 10);
        }
    }
    static void Main()
    {

        Debug.Assert(StepsToPalindrome(10, "56") == "STEP=1");
        Debug.Assert(StepsToPalindrome(9, "87") == "STEP=6");
        Debug.Assert(StepsToPalindrome(10, "87") == "STEP=4");
        Debug.Assert(StepsToPalindrome(2, "1001") == "STEP=0"); // Already a palindrome
        Debug.Assert(StepsToPalindrome(16, "1A3") == "STEP=2");


    }
}