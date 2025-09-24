
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Calculates the number of whole apples remaining after eating apples for a given amount of time.
   Each apple takes a specific amount of time to eat, and the function returns how many whole apples are left uneaten.
   - totalApples: the total number of apples available
   - timePerApple: the time it takes to eat one apple (timePerApple>=0)
   - timeElapsed: the total time elapsed

   Examples:
   >>> RemainingApples(50, 10, 200)
   30

*/

static int RemainingApples(int totalApples, int timePerApple, int timeElapsed)
{
    if (timePerApple == 0)
    {
        // If it takes 0 time to eat an apple, all apples can be eaten instantly
        return 0;
    }

    // Calculate the number of apples that can be eaten in the given time
    int applesEaten = timeElapsed / timePerApple;

    // Ensure we don't eat more apples than available
    applesEaten = Math.Min(applesEaten, totalApples);

    // Return the remaining apples
    return totalApples - applesEaten;
}
    static void Main()
    {
        Debug.Assert(RemainingApples(50, 10, 200) == 30);
        Debug.Assert(RemainingApples(10, 2, 5) == 7);
        Debug.Assert(RemainingApples(5, 15, 10) == 4);
        Debug.Assert(RemainingApples(20, 0, 100) == 0); // Case to test division by zero handling
        Debug.Assert(RemainingApples(3, 10, 130) == 0); // Case where all apples are eaten

    }
}