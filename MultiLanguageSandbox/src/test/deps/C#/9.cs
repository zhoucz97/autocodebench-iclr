using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;


class Program
{
/* Calculates the area of intersection between two rectangles.
   Each rectangle is defined by the coordinates of its top-left (x1, y1) and bottom-right (x2, y2) corners.
   
   Examples:
   >>> CalculateIntersectionArea(0, 0, 2, 2, 1, 1, 3, 3)
   1.00
   >>> CalculateIntersectionArea(1, 1, 3, 4, 2, 2, 5, 5)
   2.00
   >>> CalculateIntersectionArea(0, 0, 1, 1, 2, 2, 3, 3)
   0.00 (No Intersection)
*/
static double CalculateIntersectionArea(int x1Rect1, int y1Rect1, int x2Rect1, int y2Rect1, int x1Rect2, int y1Rect2, int x2Rect2, int y2Rect2)
{
        // Calculate the overlap in the x-axis
        int xOverlap = Math.Min(x2Rect1, x2Rect2) - Math.Max(x1Rect1, x1Rect2);
        
        // Calculate the overlap in the y-axis
        int yOverlap = Math.Min(y2Rect1, y2Rect2) - Math.Max(y1Rect1, y1Rect2);
        
        // If there is no overlap in either axis, the area is 0
        if (xOverlap <= 0 || yOverlap <= 0)
        {
            return 0.0;
        }
        
        // Otherwise, the area is the product of the overlaps
        return xOverlap * yOverlap;
    }

    // Example usage
    static void Main()
    {

        Debug.Assert(Math.Abs(CalculateIntersectionArea(0, 0, 2, 2, 1, 1, 3, 3) - 1.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(1, 1, 3, 4, 2, 2, 5, 5) - 2.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(0, 0, 1, 1, 2, 2, 3, 3) - 0.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(1, 1, 4, 4, 3, 3, 6, 6) - 1.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(0, 0, 3, 3, 1, 1, 2, 2) - 1.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(2, 2, 5, 5, 3, 3, 6, 6) - 4.00) < 0.01);
        Debug.Assert(Math.Abs(CalculateIntersectionArea(0, 0, 2, 2, 3, 3, 5, 5) - 0.00) < 0.01);

    }
}