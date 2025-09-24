using System;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{

/* 
    Finds the first longest and the first shortest word in a given sentence. 
    A word is defined as a continuous sequence of letters (A-Z, a-z), 
    and words are separated by spaces and commas. Quotation marks and other 
    punctuation are not part of the words.

    Examples:
    >>> FindLongestAndShortestWord("The quick, brown fox jumps over a lazy dog")
    ("quick", "a")

    >>> FindLongestAndShortestWord("Hello world, this is an example sentence")
    ("sentence", "is")
*/

    static (string, string) FindLongestAndShortestWord(string sentence)
{
        // Split the sentence into words based on spaces and commas
        var words = sentence.Split(new[] { ' ', ',' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Filter out non-letter characters from each word
        var cleanedWords = words.Select(word => 
            new string(word.Where(c => char.IsLetter(c)).ToArray()));
        
        // Find the first longest and first shortest words
        string longestWord = cleanedWords.OrderByDescending(w => w.Length).FirstOrDefault();
        string shortestWord = cleanedWords.OrderBy(w => w.Length).FirstOrDefault();
        
        return (longestWord, shortestWord);
    }
    static void Main()
    {
        Debug.Assert(FindLongestAndShortestWord("Fast cars, smooth drives") == ("smooth", "Fast"));
        Debug.Assert(FindLongestAndShortestWord("Sunshine brings happiness") == ("happiness", "brings"));
        Debug.Assert(FindLongestAndShortestWord("A") == ("A", "A")); // Edge case: Only one word
        Debug.Assert(FindLongestAndShortestWord("Every cloud has a silver lining") == ("silver", "a"));


    }
}