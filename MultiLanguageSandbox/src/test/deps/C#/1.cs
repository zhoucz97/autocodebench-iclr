using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;

class Program
{
    /* Check if in given list of numbers, any two numbers are closer to each other than
   the given threshold.
   >>> hasCloseElements([1.0, 2.0, 3.0], 0.5)
   false
   >>> hasCloseElements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
   true
    */
    static bool HasCloseElements(List<double> numbers, double threshold)
{
        // Sort the list to easily find the closest pairs
        numbers.Sort();
        
        // Iterate through the sorted list and check adjacent elements
        for (int i = 0; i < numbers.Count - 1; i++)
        {
            double difference = Math.Abs(numbers[i] - numbers[i + 1]);
            if (difference < threshold)
            {
                return true;
            }
        }
        
        return false;
    }
static void Main()
    {

        // Test cases
        Console.WriteLine(HasCloseElements(new List<double> { 1.0, 2.0, 3.9, 4.0, 5.0, 2.2 }, 0.3) == true);
        Console.WriteLine(HasCloseElements(new List<double> { 1.0, 2.0, 3.9, 4.0, 5.0, 2.2 }, 0.05) == false);
        Console.WriteLine(HasCloseElements(new List<double> { 1.0, 2.0, 5.9, 4.0, 5.0 }, 0.95) == true);
        Console.WriteLine(HasCloseElements(new List<double> { 1.0, 2.0, 5.9, 4.0, 5.0 }, 0.8) == false);
        Console.WriteLine(HasCloseElements(new List<double> { 1.0, 2.0, 3.0, 4.0, 5.0, 2.0 }, 0.1) == true);
        Console.WriteLine(HasCloseElements(new List<double> { 1.1, 2.2, 3.1, 4.1, 5.1 }, 1.0) == true);
        Console.WriteLine(HasCloseElements(new List<double> { 1.1, 2.2, 3.1, 4.1, 5.1 }, 0.5) == false);
    }
}