using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


/* Calculate the speed of a vehicle and the next palindromic number on the odometer.
     Given the current palindromic odometer reading and the time elapsed in hours, 
     find the constant speed of the vehicle and the next palindromic odometer reading.
     The speed is calculated per hour.
     Example:
     >>> CalculateSpeedAndNextPalindrome(12321, 2)
     (50, 12421)
     >>> CalculateSpeedAndNextPalindrome(23332, 4)
     (25, 23432)
*/
static (int, int) CalculateSpeedAndNextPalindrome(int currentPalindrome, int elapsedHours)
{
        int nextPalindrome = FindNextPalindrome(currentPalindrome);
        int distanceTraveled = nextPalindrome - currentPalindrome;
        int speed = distanceTraveled / elapsedHours;
        return (speed, nextPalindrome);
    }

    private static int FindNextPalindrome(int current)
    {
        int next = current + 1;
        while (true)
        {
            if (IsPalindrome(next))
            {
                return next;
            }
            next++;
        }
    }

    private static bool IsPalindrome(int number)
    {
        string numStr = number.ToString();
        int left = 0;
        int right = numStr.Length - 1;
        while (left < right)
        {
            if (numStr[left] != numStr[right])
            {
                return false;
            }
            left++;
            right--;
        }
        return true;
    }
    static void Main()
    {
        Debug.Assert(CalculateSpeedAndNextPalindrome(12321, 2) == (50, 12421));
        Debug.Assert(CalculateSpeedAndNextPalindrome(11111, 1) == (100, 11211));
        Debug.Assert(CalculateSpeedAndNextPalindrome(23332, 2) == (50, 23432));
        Debug.Assert(CalculateSpeedAndNextPalindrome(45654, 4) == (25, 45754));

    }
}