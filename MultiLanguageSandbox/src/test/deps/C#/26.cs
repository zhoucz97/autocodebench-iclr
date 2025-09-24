using System;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* Determines if two DNA sequences are related based on the similarity threshold.
   The function compares two DNA sequences of the same length, base pair by base pair.
   If the proportion of identical base pairs is greater than or equal to the given threshold,
   the sequences are considered related.
   
   Example:
   >>> AreDNAsRelated("ATCG", "ATCC", 0.75)
   true
*/

    static bool AreDNAsRelated(string dnaSequence1, string dnaSequence2, double similarityThreshold)
{
        // Check if the sequences are of the same length
        if (dnaSequence1.Length != dnaSequence2.Length)
        {
            throw new ArgumentException("DNA sequences must be of the same length");
        }

        int identicalPairs = 0;
        
        // Compare each base pair
        for (int i = 0; i < dnaSequence1.Length; i++)
        {
            if (dnaSequence1[i] == dnaSequence2[i])
            {
                identicalPairs++;
            }
        }
        
        // Calculate the proportion of identical base pairs
        double similarity = (double)identicalPairs / dnaSequence1.Length;
        
        // Return true if the similarity meets or exceeds the threshold
        return similarity >= similarityThreshold;
    }
    static void Main()
    {

        Debug.Assert(AreDNAsRelated("ATCGCCGTAAGTAACGGTTTTAAATAGGCC", "ATCGCCGGAAGTAACGGTCTTAAATAGGCC", 0.85) == true);
        Debug.Assert(AreDNAsRelated("GGCATGAC", "TGACATGC", 0.50) == false);
        Debug.Assert(AreDNAsRelated("AAAA", "AAAA", 1.00) == true);
        Debug.Assert(AreDNAsRelated("CCCC", "GGTT", 0.25) == false);

    }
}