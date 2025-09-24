#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
/*
You are given a 1 by n pixel image, where each pixel has a color represented by an integer. You can perform an operation where you pick a color and change all connected pixels of the same color to the chosen color. Two pixels are connected if they are adjacent and have the same color. Find the minimum number of operations required to make all pixels the same color.

Note: For each color, there are at most 20 pixels of that color.

Examples:
    >>> minOperations(5, (int[]){1, 2, 3, 2, 1})
    2
*/
int minOperations(int n, int* pixels) {
    if (n == 0) return 0;
    
    // Create a set to track unique colors
    bool colorSet[256] = {false}; // Assuming pixel values are within 0-255; adjust if necessary
    int uniqueColors = 0;
    
    for (int i = 0; i < n; i++) {
        int color = pixels[i];
        if (!colorSet[color]) {
            colorSet[color] = true;
            uniqueColors++;
        }
    }
    
    return uniqueColors - 1;
}
int main() {
    assert(minOperations(5, (int[]){1, 2, 3, 2, 1}) == 2);
    assert(minOperations(4, (int[]){1, 1, 2, 2}) == 1);
    assert(minOperations(5, (int[]){1, 2, 1, 4, 2}) == 3);
    assert(minOperations(5, (int[]){5, 5, 5, 5, 5}) == 0);
    assert(minOperations(6, (int[]){1, 1, 1, 2, 2, 2}) == 1);
    assert(minOperations(7, (int[]){1, 3, 3, 3, 2, 2, 2}) == 2);
    assert(minOperations(8, (int[]){4, 4, 4, 4, 3, 3, 3, 3}) == 1);
    assert(minOperations(9, (int[]){1, 2, 3, 4, 5, 6, 7, 8, 9}) == 8);
    assert(minOperations(10, (int[]){1, 2, 1, 2, 1, 2, 1, 2, 1, 2}) == 5);
    assert(minOperations(3, (int[]){3, 3, 3}) == 0);
    assert(minOperations(4, (int[]){2, 1, 1, 2}) == 1);
    return 0;
}