
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Computes a value based on the input x according to specified linear functions.
   The function applies different formulas based on the value of x:
   - For x less than 1, returns x.
   - For x between 1 and 10, returns 2x - 1.
   - For x greater than or equal to 10, returns 3x - 11.
   Example cases:
   >>> ComputeValue(0.5)
   0.5
   >>> ComputeValue(5)
   9
   >>> ComputeValue(10)
   19
*/
static double ComputeValue(double x)
{
    if (x < 1)
    {
        return x;
    }
    else if (x >= 1 && x <= 10)
    {
        return 2 * x - 1;
    }
    else // x > 10
    {
        return 3 * x - 11;
    }
}
    static void Main()
    {
        Debug.Assert(ComputeValue(0.5) == 0.5);
        Debug.Assert(ComputeValue(2) == 3);
        Debug.Assert(ComputeValue(5) == 9);
        Debug.Assert(ComputeValue(10) == 19);
        Debug.Assert(ComputeValue(15) == 34);
        Debug.Assert(ComputeValue(0) == 0);
        Debug.Assert(ComputeValue(1) == 1);

    }
}