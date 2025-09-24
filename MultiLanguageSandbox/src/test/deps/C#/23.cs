using System;
using System.Collections.Generic;
using System;
using System.Collections.Generic;
using System.Diagnostics;

class Program
{


/* 
    Calculate the dot product of two n-dimensional integer vectors.
    The dot product is the sum of the products of the corresponding elements of the two vectors.

    Example 1:
    >>> CalculateDotProduct(new List<int> {1, 3, -5}, new List<int> {4, -2, -1})
    3

    Example 2:
    >>> CalculateDotProduct(new List<int> {2, 5, 7}, new List<int> {3, 6, 8})
    100
*/

static int CalculateDotProduct(List<int> vectorA, List<int> vectorB)
{
        if (vectorA == null || vectorB == null)
        {
            throw new ArgumentNullException("Vectors cannot be null.");
        }

        if (vectorA.Count != vectorB.Count)
        {
            throw new ArgumentException("Vectors must be of the same dimension.");
        }

        int dotProduct = 0;
        for (int i = 0; i < vectorA.Count; i++)
        {
            dotProduct += vectorA[i] * vectorB[i];
        }

        return dotProduct;
    }
    static void Main()
    {
        Debug.Assert(CalculateDotProduct(new List<int> {1, 3, -5}, new List<int> {4, -2, -1}) == 3);
        Debug.Assert(CalculateDotProduct(new List<int> {2, 5, 7}, new List<int> {3, 6, 8}) == 92);
        Debug.Assert(CalculateDotProduct(new List<int> {0, 0, 0}, new List<int> {0, 0, 0}) == 0);
        Debug.Assert(CalculateDotProduct(new List<int> {1, 2}, new List<int> {3, 4}) == 11);

    }
}