#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
/*
Count the number of acute-angled triangles that can be formed by selecting any 3 edges out of a given set of edges.
    
Args:
- edges_count: The number of edges passed.
- An arbitrary number of integer arguments representing the lengths of the edges.
    
Returns:
int: The count of distinct acute-angled triangles that can be formed.
    
Examples:
>>> count_acute_triangles(4, 1, 1, 1, 1)
4
*/
int count_acute_triangles(int edges_count, ...) {
    va_list args;
    va_start(args, edges_count);
    
    // Allocate memory to store the edges
    int *edges = (int *)malloc(edges_count * sizeof(int));
    for (int i = 0; i < edges_count; i++) {
        edges[i] = va_arg(args, int);
    }
    va_end(args);
    
    int count = 0;
    
    // Iterate through all possible triplets
    for (int i = 0; i < edges_count; i++) {
        for (int j = i + 1; j < edges_count; j++) {
            for (int k = j + 1; k < edges_count; k++) {
                int a = edges[i];
                int b = edges[j];
                int c = edges[k];
                
                // Check triangle inequality
                if (a + b > c && a + c > b && b + c > a) {
                    // Find the longest side
                    int max_side = a;
                    if (b > max_side) max_side = b;
                    if (c > max_side) max_side = c;
                    
                    // Check acute condition: sum of squares of the other two sides > square of the longest side
                    int sum_squares = a*a + b*b + c*c - max_side*max_side;
                    if (sum_squares > max_side*max_side) {
                        count++;
                    }
                }
            }
        }
    }
    
    free(edges);
    return count;
}

// Example usage (for testing purposes)
int main()
{
    assert(count_acute_triangles(4, 1, 1, 1, 1) == 4);
    assert(count_acute_triangles(3, 1, 2, 3) == 0);
    // Additional tests to ensure correctness
    assert(count_acute_triangles(5, 3, 4, 5, 7, 10) == 0);
    assert(count_acute_triangles(6, 6, 8, 10, 5, 5, 5) == 4);
    return 0;
}