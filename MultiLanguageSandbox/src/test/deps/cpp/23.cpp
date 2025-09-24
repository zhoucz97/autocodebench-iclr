#include <vector>
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
/**
Count the number of acute-angled triangles that can be formed by selecting any 3 edges out of
a given set of edges.
  
Args:
- edges_count: The number of edges passed.
- An arbitrary number of integer arguments representing the lengths of the edges.
  
Returns:
int: The count of distinct acute-angled triangles that can be formed.
  
Examples:
>>> count_acute_triangles(4, 1, 1, 1, 1)
4
**/
using namespace std;

int count_acute_triangles(int edges_count, ...) {
    va_list args;
    va_start(args, edges_count);
    
    vector<int> edges;
    for (int i = 0; i < edges_count; ++i) {
        edges.push_back(va_arg(args, int));
    }
    va_end(args);
    
    int count = 0;
    
    // Iterate through all possible triplets
    for (int i = 0; i < edges_count; ++i) {
        for (int j = i + 1; j < edges_count; ++j) {
            for (int k = j + 1; k < edges_count; ++k) {
                int a = edges[i];
                int b = edges[j];
                int c = edges[k];
                
                // Check triangle inequality
                if (a + b > c && a + c > b && b + c > a) {
                    // Find the longest side
                    int max_side = max({a, b, c});
                    int sum_of_squares = a*a + b*b + c*c;
                    int max_square = max_side * max_side;
                    
                    // Check if it's acute: sum of squares of all sides > 2 * max_square
                    // Or equivalently, sum of squares of the two smaller sides > max_square
                    if (sum_of_squares > 2 * max_square) {
                        count++;
                    }
                }
            }
        }
    }
    
    return count;
}
int main() {
    assert(count_acute_triangles(4, 1, 1, 1, 1) == 4);
    assert(count_acute_triangles(3, 1, 2, 3) == 0);
    // Additional tests to ensure correctness
    assert(count_acute_triangles(5, 3, 4, 5, 7, 10) == 0);
    assert(count_acute_triangles(6, 6, 8, 10, 5, 5, 5) == 4);
    return 0;
}