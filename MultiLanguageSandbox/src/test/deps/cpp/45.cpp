#include <vector>
#include <unordered_map>
#include <algorithm>
#include <cassert>
#include <cstdio>
/**
 * You are given a 1 by n pixel image, where each pixel has a color 
 * represented by an integer. You can perform an operation where you 
 * pick a color and change all connected pixels of the same color to 
 * the chosen color. Two pixels are connected if they are adjacent 
 * and have the same color. Find the minimum number of operations 
 * required to make all pixels the same color.
 *
 * Note: For each color, there are at most 20 pixels of that color.
 *
 * Examples:
 *   minOperations(5, {1, 2, 3, 2, 1}) -> 2
 */
using namespace std;

int minOperations(int n, int* pixels) {
    if (n == 0) return 0;
    
    unordered_map<int, vector<pair<int, int>>> colorRanges;
    
    // Step 1: Group pixels by their color and record their ranges (start and end indices)
    int currentColor = pixels[0];
    int start = 0;
    for (int i = 1; i < n; ++i) {
        if (pixels[i] != currentColor) {
            colorRanges[currentColor].emplace_back(start, i - 1);
            currentColor = pixels[i];
            start = i;
        }
    }
    // Add the last segment
    colorRanges[currentColor].emplace_back(start, n - 1);
    
    // Step 2: For each color, count how many distinct colors are present in the entire array
    // The minimal operations is the minimal (total distinct colors - 1)
    unordered_set<int> distinctColors;
    for (int i = 0; i < n; ++i) {
        distinctColors.insert(pixels[i]);
    }
    int totalDistinct = distinctColors.size();
    
    if (totalDistinct == 1) {
        return 0;
    }
    
    return totalDistinct - 1;
}
int main() {
    int a1[] = {1, 2, 3, 2, 1};
    int a2[] = {1, 1, 2, 2};
    int a3[] = {1, 2, 1, 4, 2};
    int a4[] = {5, 5, 5, 5, 5};
    int a5[] = {1, 1, 1, 2, 2, 2};
    int a6[] = {1, 3, 3, 3, 2, 2, 2};
    int a7[] = {4, 4, 4, 4, 3, 3, 3, 3};
    int a8[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    int a9[] = {1, 2, 1, 2, 1, 2, 1, 2, 1, 2};
    int a10[] = {3, 3, 3};
    int a11[] = {2, 1, 1, 2};
    assert(minOperations(5, a1) == 2);
    assert(minOperations(4, a2) == 1);
    assert(minOperations(5, a3) == 3);
    assert(minOperations(5, a4) == 0);
    assert(minOperations(6, a5) == 1);
    assert(minOperations(7, a6) == 2);
    assert(minOperations(8, a7) == 1);
    assert(minOperations(9, a8) == 8);
    assert(minOperations(10, a9) == 5);
    assert(minOperations(3, a10) == 0);
    assert(minOperations(4, a11) == 1);
    return 0;
}