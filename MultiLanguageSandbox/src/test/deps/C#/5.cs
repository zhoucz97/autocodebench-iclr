using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* This function finds all narcissistic numbers within a given range.
   A narcissistic number is a number that is the sum of its own digits each raised to the power of the number of digits.
   For example, 153 is a narcissistic number because 153 = 1^3 + 5^3 + 3^3.
   
   Examples:
   >>> FindNarcissisticNumbers(100, 170)
   [153]
   >>> FindNarcissisticNumbers(100, 500)
   [153, 370, 371, 407]
*/
   static List<int> FindNarcissisticNumbers(int start, int end)
{
        List<int> narcissisticNumbers = new List<int>();
        
        for (int num = start; num <= end; num++)
        {
            if (IsNarcissistic(num))
            {
                narcissisticNumbers.Add(num);
            }
        }
        
        return narcissisticNumbers;
    }
    
    private static bool IsNarcissistic(int number)
    {
        string numStr = number.ToString();
        int length = numStr.Length;
        int sum = 0;
        
        foreach (char digitChar in numStr)
        {
            int digit = digitChar - '0'; // Convert char to int
            sum += (int)Math.Pow(digit, length);
        }
        
        return sum == number;
    }
    static void Main()
    {
 
        Debug.Assert(FindNarcissisticNumbers(100, 170).SequenceEqual(new List<int> { 153 }));
        Debug.Assert(FindNarcissisticNumbers(100, 500).SequenceEqual(new List<int> { 153, 370, 371, 407 }));
        Debug.Assert(FindNarcissisticNumbers(1, 100).SequenceEqual(new List<int> { }));
        Debug.Assert(FindNarcissisticNumbers(370, 380).SequenceEqual(new List<int> { 370, 371 }));
        Debug.Assert(FindNarcissisticNumbers(0, 999).SequenceEqual(new List<int> { 153, 370, 371, 407 }));
        Debug.Assert(FindNarcissisticNumbers(900, 1000).SequenceEqual(new List<int> { }));
        Debug.Assert(FindNarcissisticNumbers(10, 15).SequenceEqual(new List<int> { }));

    }
}