
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{
/* Calculate the initial number of peaches a monkey had, given that each day it eats half of the peaches plus one more, and on the Nth day only one peach is left.
    Examples:
    >>> InitialPeachesCount(1)
    1
    >>> InitialPeachesCount(2)
    4
    >>> InitialPeachesCount(3)
    10
*/
   
   static int InitialPeachesCount(int days)
{
    int peaches = 1; // On the last day, only 1 peach is left
    for (int day = days; day > 1; day--)
    {
        peaches = (peaches + 1) * 2;
    }
    return peaches;
}
    static void Main()
    {
        Debug.Assert(InitialPeachesCount(1) == 1);
        Debug.Assert(InitialPeachesCount(2) == 4);
        Debug.Assert(InitialPeachesCount(3) == 10);
        Debug.Assert(InitialPeachesCount(4) == 22);


    }
}