#include <vector>
#include <algorithm>
#include <cstdio>
#include <cassert>
/**
 * You are given an array a of n integers. You can perform at most one operation where you
 * select three integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j] with x,
 * at a cost of (j - i + 1). Find the minimum cost to make all elements in the array equal.
 *
 * Examples:
 *    >>> makeEqualAgain(3, (int[]){1, 2, 1})
 *    1
 *    >>> makeEqualAgain(5, (int[]){5, 5, 1, 5, 5})
 *    1
 */
using namespace std;

int makeEqualAgain(int n, int a[]) {
    if (n == 0) return 0;
    
    // Check if all elements are the same
    bool allSame = true;
    for (int i = 1; i < n; ++i) {
        if (a[i] != a[0]) {
            allSame = false;
            break;
        }
    }
    if (allSame) {
        return 0;
    }
    
    // The minimal cost is the minimal between:
    // 1. The length of the left segment that is not equal to the rightmost element (if replacing left to make all equal to rightmost)
    // 2. The length of the right segment that is not equal to the leftmost element (if replacing right to make all equal to leftmost)
    // 3. The length of the middle segment that is not equal to the ends (if replacing middle to make all equal to ends)
    
    // Option 1: replace left part to make all equal to a[n-1]
    int leftCost = 0;
    while (leftCost < n && a[leftCost] == a[n-1]) {
        leftCost++;
    }
    int cost1 = n - leftCost;
    
    // Option 2: replace right part to make all equal to a[0]
    int rightCost = 0;
    while (rightCost < n && a[n - 1 - rightCost] == a[0]) {
        rightCost++;
    }
    int cost2 = n - rightCost;
    
    // Option 3: find the minimal segment in the middle that is not equal to the ends
    int minMiddleCost = n; // initialize with maximum possible cost
    for (int i = 1; i < n - 1; ++i) {
        if (a[i] != a[0] && a[i] != a[n-1]) {
            // The segment starts at the first position where a[i] != a[0] and ends at the last position where a[i] != a[n-1]
            int start = i;
            while (start > 0 && a[start - 1] != a[0]) {
                start--;
            }
            int end = i;
            while (end < n - 1 && a[end + 1] != a[n-1]) {
                end++;
            }
            int currentCost = end - start + 1;
            if (currentCost < minMiddleCost) {
                minMiddleCost = currentCost;
            }
        }
    }
    
    // The answer is the minimal among cost1, cost2, and minMiddleCost
    int result = min({cost1, cost2, minMiddleCost});
    return result;
}
int main() {
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

    int a7[] = {1, 2, 1};
    int a8[] = {5, 5, 1, 5, 5};
    int a9[] = {1, 1, 1, 1};
    int a10[] = {2, 2, 2, 3, 2, 2};
    int a11[] = {1};
    int a12[] = {1, 2};
    int a13[] = {1, 2, 2, 1};
    int a14[] = {4, 4, 4, 3, 3, 4, 4};
    int a15[] = {5, 4, 4, 4, 5, 5};
    int a16[] = {1, 2, 1, 2, 1, 2, 1};
    // Additional provided tests
    assert(makeEqualAgain(3, a7) == 1);
    assert(makeEqualAgain(5, a8) == 1);
    assert(makeEqualAgain(4, a9) == 0);
    assert(makeEqualAgain(6, a10) == 1);
    assert(makeEqualAgain(1, a11) == 0);
    assert(makeEqualAgain(2, a12) == 1);
    assert(makeEqualAgain(4, a13) == 2);
    assert(makeEqualAgain(7, a14) == 2);
    assert(makeEqualAgain(6, a15) == 3);
    assert(makeEqualAgain(7, a16) == 5);
    
    return 0;
}