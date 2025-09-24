#include <stdio.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>
/*
You are given an array a of n integers. You can perform at most one operation where you select three integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j] with x, at a cost of (j - i + 1).
Find the minimum cost to make all elements in the array equal.
    >>> makeEqualAgain(3, (int[]){1, 2, 1})
    1
*/
int makeEqualAgain(int n, int a[]) {
    if (n == 0) return 0;
    
    // Check if all elements are the same
    int first = a[0];
    int allSame = 1;
    for (int i = 1; i < n; i++) {
        if (a[i] != first) {
            allSame = 0;
            break;
        }
    }
    if (allSame) {
        return 0;
    }
    
    // Find the leftmost position where the element is not equal to the first element
    int left = 0;
    while (left < n && a[left] == first) {
        left++;
    }
    
    // Find the rightmost position where the element is not equal to the last element
    int right = n - 1;
    while (right >= 0 && a[right] == a[n - 1]) {
        right--;
    }
    
    // The minimal cost is the minimum between replacing the left part, right part, or the middle part
    int cost1 = left; // replace left part (0..left-1) to make them equal to a[left]
    int cost2 = (n - 1 - right); // replace right part (right+1..n-1) to make them equal to a[right]
    int cost3 = (right - left + 1); // replace middle part (left..right) to make them equal to either left or right
    
    int minCost = INT_MAX;
    minCost = (cost1 < minCost) ? cost1 : minCost;
    minCost = (cost2 < minCost) ? cost2 : minCost;
    minCost = (cost3 < minCost) ? cost3 : minCost;
    
    return minCost;
}

// Example usage
int main() {
    int test1[] = {1, 2, 1};
    int test2[] = {5, 5, 1, 5, 5};
    int test3[] = {1, 1, 1, 1};
    int test4[] = {2, 2, 2, 3, 2, 2};
    int test5[] = {1};
    int test6[] = {1, 2};
    int test7[] = {1, 2, 2, 1};
    int test8[] = {4, 4, 4, 3, 3, 4, 4};
    int test9[] = {5, 4, 4, 4, 5, 5};
    int test10[] = {1, 2, 1, 2, 1, 2, 1};
    int a1[] = {1,2,3,4,5,1};
    int a2[] = {1,1,1,1,1,1,1};
    int a3[] = {8,8,8,1,2,8,8,8};
    int a4[] = {1,2,3};
    int a5[] = {4,3,2,7,1,1,3};
    int a6[] = {9,9,2,9,2,5,5,5,3};
    assert(makeEqualAgain(6, a1) == 4);
    assert(makeEqualAgain(7, a2) == 0);
    assert(makeEqualAgain(8, a3) == 2);
    assert(makeEqualAgain(3, a4) == 2);
    assert(makeEqualAgain(7, a5) == 6);
    assert(makeEqualAgain(9, a6) == 7);

    assert(makeEqualAgain(3, test1) == 1);
    assert(makeEqualAgain(5, test2) == 1);
    assert(makeEqualAgain(4, test3) == 0);
    assert(makeEqualAgain(6, test4) == 1);
    assert(makeEqualAgain(1, test5) == 0);
    assert(makeEqualAgain(2, test6) == 1);
    assert(makeEqualAgain(4, test7) == 2);
    assert(makeEqualAgain(7, test8) == 2);
    assert(makeEqualAgain(6, test9) == 3);
    assert(makeEqualAgain(7, test10) == 5);

    return 0;
}