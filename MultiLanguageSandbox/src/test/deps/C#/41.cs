
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Calculates the total number of cigarettes Peter can smoke given an initial number of cigarettes and a conversion rate of butts to new cigarettes.
    It is assumed that Peter can continue smoking and converting butts into new cigarettes as long as he has enough butts to do so.
    
    >>> TotalSmokedCigarettes(4, 3)
    5
    >>> TotalSmokedCigarettes(10, 3)
    14
*/

static int TotalSmokedCigarettes(int initialCigarettes, int buttConversionRate)
{
    int totalSmoked = 0;
    int butts = 0;
    
    while (initialCigarettes > 0)
    {
        // Smoke all available cigarettes
        totalSmoked += initialCigarettes;
        butts += initialCigarettes;
        initialCigarettes = 0;
        
        // Convert butts to new cigarettes
        if (butts >= buttConversionRate)
        {
            initialCigarettes = butts / buttConversionRate;
            butts = butts % buttConversionRate;
        }
    }
    
    return totalSmoked;
}
static void Main()
    {
        Debug.Assert(TotalSmokedCigarettes(4, 3) == 5);
        Debug.Assert(TotalSmokedCigarettes(10, 3) == 14);
        Debug.Assert(TotalSmokedCigarettes(1, 2) == 1); // No conversion possible, should return initial cigarettes
        Debug.Assert(TotalSmokedCigarettes(20, 4) ==26); // Additional case to test

    }
}