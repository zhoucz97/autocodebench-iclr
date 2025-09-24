using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/*
    Solves a linear equation of the form ax + b = 0.
    The function returns the solution as a double with two decimal places precision.
    Assumes that 'a' is not zero.

    Examples:
    >>> SolveLinearEquation(2, -4)
    2.00
    >>> SolveLinearEquation(5, 10)
    -2.00
*/
    static double SolveLinearEquation(double a, double b)
{
        // Calculate the solution x = -b / a
        double solution = -b / a;
        
        // Round the solution to two decimal places
        return Math.Round(solution, 2);
    }
    static void Main()
    {
        Debug.Assert(SolveLinearEquation(1, -3) == 3.00);
        Debug.Assert(SolveLinearEquation(4, 8) == -2.00);
        Debug.Assert(SolveLinearEquation(-2, 6) == 3.00);
        Debug.Assert(SolveLinearEquation(3, -9) == 3.00);
        Debug.Assert(SolveLinearEquation(0.5, -1.5) == 3.00);
        Debug.Assert(SolveLinearEquation(-5, 10) == 2.00);
        Debug.Assert(SolveLinearEquation(10, -20) == 2.00);

    }
}