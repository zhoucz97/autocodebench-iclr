using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines whose geometric shape has a larger area: Alice's square with side length a, or Bob's rectangle with sides b and c.
    - If Alice's square has a larger area, return "Alice".
    - If Bob's rectangle has a larger area, return "Bob".
    
    Examples:
    >>> CompareArea(5, 4, 6)
    "Alice"
    
    >>> CompareArea(7, 5, 10)
    "Bob"
*/

static string CompareArea(int a, int b, int c)
{
        // Calculate the area of Alice's square
        int aliceArea = a * a;
        
        // Calculate the area of Bob's rectangle
        int bobArea = b * c;
        
        // Compare the areas and return the appropriate result
        if (aliceArea > bobArea)
        {
            return "Alice";
        }
        else
        {
            return "Bob";
        }
    }

    // Example usage
    static void Main()
    {
        Debug.Assert(CompareArea(5, 4, 6) == "Alice");
        Debug.Assert(CompareArea(7, 5, 10) == "Bob");
        Debug.Assert(CompareArea(2, 2, 8) == "Bob"); // Testing with a square of side 2 and a rectangle 2x8
        Debug.Assert(CompareArea(10, 5, 5) == "Alice"); // Testing with a square of side 10 and a rectangle 5x5


    }
}

