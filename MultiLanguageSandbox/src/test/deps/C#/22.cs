using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

    /* Determines the winner in a game of Rock-Paper-Scissors played for a number of rounds.
    Players A and B have their own repeating patterns of moves.
    Rock (represented by 0) beats Scissors (2), Scissors beat Paper (5), and Paper beats Rock.
    If both players choose the same move, the round is a draw.

    Examples:
    >>> DetermineWinner(10, new List<int> {0, 2, 5}, new List<int> {0, 5, 0, 2})
    "A"

    >>> DetermineWinner(6, new List<int> {2, 5, 0}, new List<int> {5, 0, 2})
    "A"

    >>> DetermineWinner(7, new List<int> {5, 5, 2}, new List<int> {5, 5, 2})
    "draw"
    */

    static string DetermineWinner(int rounds, List<int> patternA, List<int> patternB)
{
        int winsA = 0;
        int winsB = 0;
        
        for (int round = 0; round < rounds; round++)
        {
            int moveA = patternA[round % patternA.Count];
            int moveB = patternB[round % patternB.Count];
            
            if (moveA == moveB)
            {
                // Draw
            }
            else if ((moveA == 0 && moveB == 2) || (moveA == 2 && moveB == 5) || (moveA == 5 && moveB == 0))
            {
                winsA++;
            }
            else
            {
                winsB++;
            }
        }
        
        if (winsA > winsB)
        {
            return "A";
        }
        else if (winsB > winsA)
        {
            return "B";
        }
        else
        {
            return "draw";
        }
    }

    // Example usage
    static void Main()
    {
        Debug.Assert(DetermineWinner(10, new List<int> {0, 2, 5}, new List<int> {0, 5, 0, 2}) == "A");
        Debug.Assert(DetermineWinner(6, new List<int> {2, 5, 0}, new List<int> {5, 0, 2}) == "A");
        Debug.Assert(DetermineWinner(5, new List<int> {0, 0, 0}, new List<int> {2, 2, 2}) == "A");
        Debug.Assert(DetermineWinner(7, new List<int> {5, 5, 2}, new List<int> {5, 5, 2}) == "draw");
    }
}