using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determine the last remaining person's number in a circular counting game.
   In this game, people are numbered from 1 to totalPeople and sit in a circle. 
   Starting with the first person, they count from 1 to countNumber. 
   The person who is at countNumber is removed from the circle, 
   and the count resumes from the next person. 
   This process continues until only one person remains.

   Example:
   >>> LastRemainingPerson(5, 2)
   3
   >>> LastRemainingPerson(6, 4)
   5
*/

static int LastRemainingPerson(int totalPeople, int countNumber)
{
        List<int> people = new List<int>();
        for (int i = 1; i <= totalPeople; i++) {
            people.Add(i);
        }
        
        int index = 0;
        while (people.Count > 1) {
            index = (index + countNumber - 1) % people.Count;
            people.RemoveAt(index);
        }
        
        return people[0];
    }
static void Main()
    {
        Debug.Assert(LastRemainingPerson(5, 2) == 3);
        Debug.Assert(LastRemainingPerson(6, 4) == 5);
        Debug.Assert(LastRemainingPerson(10, 3) == 4);
        Debug.Assert(LastRemainingPerson(7, 2) == 7);

    }
}